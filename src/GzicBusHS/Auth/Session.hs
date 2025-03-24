{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.Auth.Session (
  SessionEnv (..),
  SessionState (..),
  Session (..),
  newSessionEnv,
  emptySessionState,
  runSession,
  saveSessionSate,
  loadSessionState,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Logger (
  LogLevel,
  LoggingT,
  MonadLogger,
  filterLogger,
  runStderrLoggingT,
 )
import Data.Aeson (KeyValue ((.=)), (.:))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import GzicBusHS.Auth.Cookies (
  PersistentCookieJar,
  mkPersistentCookieJar,
  unPersistentCookieJar,
 )
import GzicBusHS.Auth.Errors (SessionError)
import Network.HTTP.Client (CookieJar, Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import System.OsPath (OsPath, decodeFS)

newtype SessionEnv = SessionEnv
  { connectionManager :: Manager
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''SessionEnv

newtype SessionState = SessionState
  { cookies :: CookieJar
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''SessionState

newtype Session (m :: Type -> Type) (a :: Type)
  = Session
      ( ExceptT
          SessionError
          ( ReaderT
              SessionEnv
              ( StateT
                  SessionState
                  (LoggingT m)
              )
          )
          a
      )
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState SessionState
    , MonadReader SessionEnv
    , MonadError SessionError
    , MonadLogger
    )

instance MonadTrans Session where
  lift = Session . lift . lift . lift . lift

newSessionEnv ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  m SessionEnv
newSessionEnv =
  fmap SessionEnv $
    liftIO $
      newManager tlsManagerSettings

emptySessionState :: SessionState
emptySessionState = SessionState mempty

-- NOTE(chfanghr): We serialize cookies via PersistentCookieJar and this makes it
-- impossible to implement a lawful pair of From/ToJSON instances for SessionState.
-- We do guarantee that:
--   sessionStateToJSON (fromRight (sessionSateFromJSON jsonValue)) == jsonValue
sessionStateToJSON :: SessionState -> A.Value
sessionStateToJSON s =
  A.object
    [ "cookies" .= mkPersistentCookieJar (s ^. #cookies)
    ]

sessionSateFromJSON :: A.Value -> A.Parser SessionState
sessionSateFromJSON = A.withObject "SessionState" $ \obj -> do
  cookies :: PersistentCookieJar <- obj .: "cookies"
  pure $ SessionState $ unPersistentCookieJar cookies

saveSessionSate ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  OsPath ->
  SessionState ->
  m ()
saveSessionSate p s = do
  p' <- liftIO $ decodeFS p
  writeFileLBS p' $ A.encode $ sessionStateToJSON s

loadSessionState ::
  forall (m :: Type -> Type).
  (MonadIO m, MonadFail m) =>
  OsPath ->
  m SessionState
loadSessionState p = do
  p' <- liftIO $ decodeFS p
  bs <- readFileBS p'
  either
    (fail . ("unable to decode session state: " <>))
    pure
    $ A.eitherDecodeStrict' bs >>= A.parseEither sessionSateFromJSON

runSession ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadIO m, HasCallStack) =>
  Session m a ->
  SessionEnv ->
  SessionState ->
  LogLevel ->
  m (Either SessionError a, SessionState)
runSession (Session inner) env st lvl =
  runStderrLoggingT $
    filterLogger (const (>= lvl)) $
      usingStateT st $
        usingReaderT env $
          runExceptT inner
