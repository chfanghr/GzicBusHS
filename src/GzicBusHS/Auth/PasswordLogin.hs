{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.Auth.PasswordLogin (
  login,
  checkLoginStatus,
) where

import Control.Monad.Error.Class (liftEither, withError)
import Control.Monad.Extra (whileM)
import Control.Monad.Logger (logDebugN)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types qualified as A
import Data.Base16.Types (extractBase16)
import Data.ByteString.Base16 (encodeBase16)
import Data.Either.Extra (maybeToEither)
import Data.Text qualified as T
import GzicBusHS.Auth.Errors (
  LoginError (
    FailToCheckLoginStatus,
    FailToExtractLoginToken,
    FailToLoadLoginPage,
    FailToSendLoginRequest
  ),
  LoginTokenExtractionError (LoginTokenNotFound),
  SessionError (LoginError),
  withGenericHttpClientError,
 )
import GzicBusHS.Auth.Session (Session)
import GzicBusHS.Auth.Token (postTokenReq)
import GzicBusHS.Auth.Utils (fuckedUpDes, performRequestWithCookies)
import Network.HTTP.Client (
  Request (checkResponse, method, requestBody),
  RequestBody (RequestBodyLBS),
  requestFromURI,
  responseBody,
  throwErrorStatusCodes,
 )
import Network.HTTP.Types (methodGet, methodPost)
import Network.URI (URI, parseURI)
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude.Unsafe qualified as Unsafe
import Text.RawString.QQ (r)
import Text.Regex.TDFA (MatchResult (mrSubList), Regex, RegexContext (matchM), RegexMaker (makeRegex))

loginPageURL :: URI
loginPageURL = Unsafe.fromJust $ parseURI "https://sso.scut.edu.cn/cas/login"

loginPageReq :: Request
loginPageReq = Unsafe.fromJust $ requestFromURI loginPageURL

getLoginPageReq :: Request
getLoginPageReq =
  loginPageReq
    { method = methodGet
    , checkResponse = throwErrorStatusCodes
    }

data LoginParams = LoginParams
  { username :: Text
  , password :: Text
  , loginId :: Text
  , twoFactorAuthenticationCode :: Maybe Text
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''LoginParams

-- NOTE(chfanghr): Oh fuck, yeah very secure :D
fixedDESPasswords :: [Text]
fixedDESPasswords = ["1", "2", "3"]

encodeLoginReqBody :: (HasCallStack) => LoginParams -> A.Value
encodeLoginReqBody params =
  let ul = T.length $ params ^. #username
      pl = T.length $ params ^. #password
      rsa =
        extractBase16 $
          encodeBase16 $
            fuckedUpDes fixedDESPasswords $
              mconcat
                [ params ^. #username
                , params ^. #password
                , params ^. #loginId
                ]

      execution :: Text =
        if isJust $ params ^. #twoFactorAuthenticationCode
          then "e1s1"
          else "e1s2"

      eventId :: Text = "submit"

      kv :: forall (a :: Type). (A.ToJSON a) => Text -> a -> A.Pair
      kv k v = (A.fromText k, A.toJSON v)

      val =
        A.object $
          mconcat
            [
              [ kv "ul" ul
              , kv "pl" pl
              , kv "rsa" rsa
              , kv "execution" execution
              , kv "_eventId" eventId
              ]
            , maybe mempty (one . kv "PM1") $
                params ^. #twoFactorAuthenticationCode
            ]
   in val

mkPostLoginReq :: A.Value -> Request
mkPostLoginReq body =
  loginPageReq
    { method = methodPost
    , -- NOTE(chfanghr): It's always 200 not matter what shit you throw at it.
      checkResponse = throwErrorStatusCodes
    , requestBody = RequestBodyLBS $ A.encode body
    }

checkLoginStatus ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Session m ()
checkLoginStatus = do
  void $
    withError (withGenericHttpClientError (LoginError . FailToCheckLoginStatus)) $
      performRequestWithCookies postTokenReq

login ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  m Text ->
  Text ->
  Text ->
  Session m ()
login retrieveTwoFactorAuthenticationCode username password = do
  logDebugN "loading login page"

  loginPage :: Text <-
    fmap (decodeUtf8 . responseBody) $
      withError (withGenericHttpClientError (LoginError . FailToLoadLoginPage)) $
        performRequestWithCookies getLoginPageReq

  logDebugN $ "login page: " <> loginPage

  loginToken <-
    liftEither $
      first (LoginError . FailToExtractLoginToken) $
        extractLoginToken loginPage

  logDebugN $ "login token: " <> loginToken

  require2FA <- doLogin loginToken Nothing

  when require2FA $ whileM $ do
    code <- lift retrieveTwoFactorAuthenticationCode
    doLogin loginToken $ Just code

  checkLoginStatus
  where
    doLogin :: (HasCallStack) => Text -> Maybe Text -> Session m Bool
    doLogin loginId twoFactorAuthenticationCode = do
      logDebugN "attempt to login"
      logDebugN $ "with 2fa code? " <> show (isJust twoFactorAuthenticationCode)

      let loginParams =
            LoginParams
              { username
              , password
              , loginId
              , twoFactorAuthenticationCode
              }

          loginBody = encodeLoginReqBody loginParams

      logDebugN $ "login body: " <> toStrict (encodeToLazyText loginBody)

      resp :: Text <-
        fmap
          (decodeUtf8 . responseBody)
          $ withError (withGenericHttpClientError (LoginError . FailToSendLoginRequest))
          $ performRequestWithCookies
          $ mkPostLoginReq loginBody

      logDebugN $ "login response: " <> resp

      pure $ "PM1" `T.isInfixOf` resp

extractLoginIdRegex :: Regex
extractLoginIdRegex = makeRegex ([r|<input.+name="lt"[[:space:]]+value="([^"]+)"|] :: Text)

extractLoginToken ::
  (HasCallStack) =>
  Text ->
  Either LoginTokenExtractionError Text
extractLoginToken inp = do
  matchResult :: MatchResult Text <-
    maybeToEither LoginTokenNotFound $
      matchM extractLoginIdRegex inp

  case mrSubList matchResult of
    [val] -> Right val
    _ -> error "unreachable"
