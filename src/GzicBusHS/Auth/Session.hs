{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.Auth.Session (
  SessionEnv (..),
  SessionState (..),
  Session (..),
  newSessionEnv,
  emptySessionState,
  runSession,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Logger (
  LogLevel,
  LoggingT,
  MonadLogger,
  filterLogger,
  runStderrLoggingT,
 )
import GzicBusHS.Auth.Errors (SessionError)
import Network.HTTP.Client (CookieJar, Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Optics.TH (makeFieldLabelsNoPrefix)

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

-- TODO(chfanghr): Provide a way to persist/load SessionState

emptySessionState :: SessionState
emptySessionState = SessionState mempty

runSession ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadIO m, HasCallStack) =>
  Session m a ->
  SessionEnv ->
  SessionState ->
  LogLevel ->
  m (Either SessionError (a, SessionState))
runSession (Session inner) env st lvl =
  fmap (\(e, s') -> (,s') <$> e) $
    runStderrLoggingT $
      filterLogger (const (>= lvl)) $
        usingStateT st $
          usingReaderT env $
            runExceptT inner
