{-# LANGUAGE QuasiQuotes #-}

module GzicBusHS.Auth.QRLogin (login) where

import Control.Monad.Error.Class (
  MonadError (throwError),
  liftEither,
  withError,
 )
import Control.Monad.Logger (logDebugN)
import Data.Either.Extra (maybeToEither)
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import GzicBusHS.Auth.Errors (
  QRLoginError (
    FailToCheckQrCodeScan,
    FailToExtractQRLoginToken,
    FailToLoginWithToken,
    QRLoginTimeout
  ),
  QRLoginTokenExtractionError (QRLoginTokenNotFound),
  SessionError (QRLoginError),
  withGenericHttpClientError,
 )
import GzicBusHS.Auth.Session (Session)
import GzicBusHS.Auth.Token (checkLoginStatus)
import GzicBusHS.Auth.Utils (
  fromSingletonCaptureGroup,
  genFuckedUpTimeBasedV4UUID,
  performRequestWithCookies,
  retryWithErrorFilter,
 )
import Network.HTTP.Client (
  Request (checkResponse, method),
  Response (responseBody),
  requestFromURI,
  throwErrorStatusCodes,
 )
import Network.HTTP.Types (QueryItem, methodGet, renderQuery)
import Network.URI (URI (uriFragment, uriQuery), parseURI)
import Relude.Unsafe qualified as Unsafe
import System.Random (newStdGen)
import System.Time.Extra (sleep)
import Text.RawString.QQ (r)
import Text.Regex.TDFA (
  MatchResult,
  Regex,
  RegexContext (matchM),
  RegexMaker (makeRegex),
 )

login ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  (URI -> m ()) ->
  Maybe NominalDiffTime ->
  Session m ()
login presentQR maybeValidDuration = do
  rng <- newStdGen
  startingTime <- liftIO getCurrentTime
  let stateUUID = genFuckedUpTimeBasedV4UUID startingTime rng
      qrURI = mkQRCodeLoginUrl stateUUID
  logDebugN $ "qr uri: " <> show qrURI
  lift $ presentQR qrURI

  -- TODO(chfanghr): Retry limit
  loginToken <- retryWithErrorFilter
    ( \case
        QRLoginError QRLoginTimeout -> False
        _ -> True
    )
    $ do
      liftIO $ sleep 5

      whenJust maybeValidDuration $ \validDuration -> do
        currentTime <- liftIO getCurrentTime

        when ((currentTime `diffUTCTime` startingTime) > validDuration) $
          throwError $
            QRLoginError QRLoginTimeout

      retrieveQRLoginToken stateUUID
  logDebugN $ "login token fetched: " <> loginToken

  loginWithToken loginToken

  checkLoginStatus

retrieveQRLoginToken ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  UUID ->
  Session m Text
retrieveQRLoginToken stateUUID = do
  logDebugN "checking qr scan status"

  resp :: Text <-
    fmap
      (decodeUtf8 . responseBody)
      $ withError (withGenericHttpClientError (QRLoginError . FailToCheckQrCodeScan))
      $ performRequestWithCookies
      $ mkQRScanCheckReq stateUUID

  logDebugN $ "qr scan check response: " <> resp

  liftEither $
    first (QRLoginError . FailToExtractQRLoginToken) $
      extractQRLoginToken resp

loginWithToken ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Text ->
  Session m ()
loginWithToken token = do
  logDebugN "attempting to login with token"

  void $
    withError (withGenericHttpClientError (QRLoginError . FailToLoginWithToken)) $
      performRequestWithCookies $
        mkQRLoginWithTokenReq token

mkQRCodeLoginUrl :: UUID -> URI
mkQRCodeLoginUrl stateUUID =
  let baseURI = Unsafe.fromJust $ parseURI "https://open.weixin.qq.com/connect/oauth2/authorize"

      query :: [QueryItem] =
        [ ("appid", Just "wx39f121ed798af736")
        , ("redirect_uri", Just "https://sso.scut.edu.cn/cas/scutwxsso")
        , ("response_type", Just "code")
        , ("scope", Just "snsapi_base")
        , ("state", Just (show stateUUID))
        ]

      uri =
        baseURI
          { uriQuery = decodeUtf8 $ renderQuery True query
          , uriFragment = "#wechat_redirect"
          }
   in uri

mkQRScanCheckURL :: UUID -> URI
mkQRScanCheckURL stateUUID =
  let baseURI = Unsafe.fromJust $ parseURI "https://sso.scut.edu.cn/cas/scutqqcheck"
      query :: [QueryItem] = one ("uuid", Just (show stateUUID))
      uri =
        baseURI
          { uriQuery = decodeUtf8 $ renderQuery True query
          }
   in uri

mkQRScanCheckReq :: UUID -> Request
mkQRScanCheckReq = m . Unsafe.fromJust . requestFromURI . mkQRScanCheckURL
  where
    m req =
      req
        { method = methodGet
        , checkResponse = throwErrorStatusCodes
        }

mkQRLoginWithTokenURL :: Text -> URI
mkQRLoginWithTokenURL token =
  let baseURI = Unsafe.fromJust $ parseURI "https://sso.scut.edu.cn/cas/qRCode"
      query :: [QueryItem] =
        [ ("token", Just $ encodeUtf8 token)
        , ("service", Just "https://life.gzic.scut.edu.cn/login/cas/")
        ]
      uri =
        baseURI
          { uriQuery = decodeUtf8 $ renderQuery True query
          }
   in uri

mkQRLoginWithTokenReq :: Text -> Request
mkQRLoginWithTokenReq = m . Unsafe.fromJust . requestFromURI . mkQRLoginWithTokenURL
  where
    m req =
      req
        { method = methodGet
        , checkResponse = throwErrorStatusCodes
        }

extractQRLoginTokenRegex :: Regex
extractQRLoginTokenRegex = makeRegex ([r|null\("(..+)"\)|] :: Text)

extractQRLoginToken ::
  (HasCallStack) =>
  Text ->
  Either QRLoginTokenExtractionError Text
extractQRLoginToken inp = do
  matchResult :: MatchResult Text <-
    maybeToEither QRLoginTokenNotFound $
      matchM extractQRLoginTokenRegex inp

  pure $ fromSingletonCaptureGroup matchResult
