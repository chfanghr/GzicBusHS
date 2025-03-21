{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.AuthSession (
  AuthSessionError (..),
  LoginError (..),
  RetrieveTokenError (..),
  LoginIdExtractionError (..),
  AuthSessionEnv,
  newAuthSessionEnv,
  AuthSessionState,
  emptyAuthSessionState,
  AuthSession,
  runAuthSession,
  login,
  retrieveToken,
) where

import Control.Exception (catch)
import Control.Monad.Error.Class (
  MonadError,
  liftEither,
  withError,
 )
import Control.Monad.Extra (whileM)
import Crypto.Cipher.DES (DES)
import Crypto.Cipher.Types (BlockCipher (ecbEncrypt), Cipher (cipherInit))
import Crypto.Error (throwCryptoError)
import Data.Aeson (withObject, (.:))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.Types qualified as A
import Data.Base16.Types (extractBase16)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra (maybeToEither)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf16BE)
import Data.Time (getCurrentTime)
import Network.HTTP.Client (
  CookieJar,
  Manager,
  Request (
    checkResponse,
    cookieJar,
    method,
    requestBody,
    requestHeaders
  ),
  RequestBody (RequestBodyLBS),
  Response (responseBody),
  httpLbs,
  newManager,
  requestFromURI,
  throwErrorStatusCodes,
  updateCookieJar,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent, methodGet, methodPost)
import Network.URI (URI, parseURI)
import Optics ((^.))
import Optics.State.Operators ((.=))
import Optics.TH (makeFieldLabelsNoPrefix)
import Optics.View (ViewableOptic (gview), guse)
import Relude.Unsafe qualified as Unsafe
import Text.RawString.QQ (r)
import Text.Regex.TDFA (
  MatchResult (mrSubList),
  Regex,
  RegexContext (matchM),
  RegexMaker (makeRegex),
 )

data AuthSessionError
  = LoginError LoginError
  | RetrieveTokenError RetrieveTokenError
  | GenericHttpClientError SomeException
  deriving stock (Generic)

withGenericHttpClientError ::
  (SomeException -> AuthSessionError) ->
  AuthSessionError ->
  AuthSessionError
withGenericHttpClientError f (GenericHttpClientError e) = f e
withGenericHttpClientError _ e = e

data LoginError
  = FailToLoadLoginPage SomeException
  | FailToExtractLoginId LoginIdExtractionError
  | FailToSendLoginRequest SomeException
  | FailToCheckLoginStatus SomeException
  deriving stock (Generic)

data RetrieveTokenError
  = FailToSendPostTokenRequest SomeException
  | FailToSendGetTokenRequest SomeException
  | FailToParseToken String
  deriving stock (Generic)

data LoginIdExtractionError = LoginIdNotFound
  deriving stock (Generic)

newtype AuthSessionEnv = AuthSessionEnv
  { connectionManager :: Manager
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''AuthSessionEnv

newAuthSessionEnv :: forall (m :: Type -> Type). (MonadIO m) => m AuthSessionEnv
newAuthSessionEnv = fmap AuthSessionEnv $ liftIO $ newManager tlsManagerSettings

newtype AuthSessionState = AuthSessionState
  { cookies :: CookieJar
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''AuthSessionState

emptyAuthSessionState :: AuthSessionState
emptyAuthSessionState = AuthSessionState mempty

-- TODO: persist AuthSessionState

newtype AuthSession (m :: Type -> Type) (a :: Type)
  = AuthSession
      ( ExceptT
          AuthSessionError
          (ReaderT AuthSessionEnv (StateT AuthSessionState m))
          a
      )
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState AuthSessionState
    , MonadReader AuthSessionEnv
    , MonadError AuthSessionError
    )

instance MonadTrans AuthSession where
  lift = AuthSession . lift . lift . lift

runAuthSession ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadIO m) =>
  AuthSession m a ->
  AuthSessionEnv ->
  AuthSessionState ->
  m (Either AuthSessionError (a, AuthSessionState))
runAuthSession (AuthSession inner) env s =
  (\(e, s') -> (,s') <$> e)
    <$> runStateT (runReaderT (runExceptT inner) env) s

extractLoginIdRegex :: Regex
extractLoginIdRegex =
  makeRegex
    ([r|<input.+name="lt"[[:space:]]+value="([^"]+)"|] :: Text)

extractLoginId ::
  Text ->
  Either LoginIdExtractionError Text
extractLoginId inp = do
  matchResult :: MatchResult Text <-
    maybeToEither LoginIdNotFound $
      matchM extractLoginIdRegex inp

  case mrSubList matchResult of
    [val] -> Right val
    _ -> error "unreachable"

loginPageURL :: URI
loginPageURL = Unsafe.fromJust $ parseURI "https://sso.scut.edu.cn/cas/login"

loginPageReq :: Request
loginPageReq = Unsafe.fromJust $ requestFromURI loginPageURL

userAgent :: Text
userAgent = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.2 (KHTML, like Gecko) Chrome/22.0.1216.0 Safari/537.2"

performRequestWithCookies ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  Request ->
  AuthSession m (Response LBS.ByteString)
performRequestWithCookies req = do
  cookies <- guse #cookies
  manager <- gview #connectionManager

  let headers = requestHeaders req
      headers' = (hUserAgent, encodeUtf8 userAgent) : headers

      req' =
        req
          { cookieJar = Just cookies
          , requestHeaders = headers'
          }

  resp <-
    AuthSession $
      ExceptT $
        liftIO $
          catch
            (Right <$> httpLbs req' manager)
            (\(e :: SomeException) -> pure $ Left $ GenericHttpClientError e)

  now <- liftIO getCurrentTime
  let (cookies', resp') = updateCookieJar resp req' now cookies

  #cookies .= cookies'

  pure resp'

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

encodeLoginReqBody :: LoginParams -> A.Value
encodeLoginReqBody params =
  let ul = T.length $ params ^. #username
      pl = T.length $ params ^. #password
      rsa =
        extractBase16 $
          encodeBase16 $
            fuckedUpDes ["1", "2", "3"] $
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

mkPostLoginReq :: LoginParams -> Request
mkPostLoginReq body =
  loginPageReq
    { method = methodPost
    , checkResponse = throwErrorStatusCodes -- NOTE(chfanghr): Yeah, it's always 200
    , requestBody = RequestBodyLBS $ A.encode $ encodeLoginReqBody body
    }

login ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  m Text ->
  Text ->
  Text ->
  AuthSession m ()
login retrieveTwoFactorAuthenticationCode username password = do
  loginPage :: Text <-
    fmap (decodeUtf8 . responseBody) $
      withError (withGenericHttpClientError (LoginError . FailToLoadLoginPage)) $
        performRequestWithCookies getLoginPageReq

  loginId <-
    liftEither $
      first (LoginError . FailToExtractLoginId) $
        extractLoginId loginPage

  require2FA <- doLogin loginId Nothing

  when require2FA $ whileM $ do
    code <- lift retrieveTwoFactorAuthenticationCode
    doLogin loginId $ Just code

  checkLoginStatus
  where
    doLogin :: Text -> Maybe Text -> AuthSession m Bool
    doLogin loginId twoFactorAuthenticationCode = do
      let loginParams =
            LoginParams
              { username
              , password
              , loginId
              , twoFactorAuthenticationCode
              }

      resp :: Text <-
        fmap
          (decodeUtf8 . responseBody)
          $ withError (withGenericHttpClientError (LoginError . FailToSendLoginRequest))
          $ performRequestWithCookies
          $ mkPostLoginReq loginParams

      pure $ "PM1" `T.isInfixOf` resp

checkLoginStatus ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  AuthSession m ()
checkLoginStatus = do
  void $
    withError (withGenericHttpClientError (LoginError . FailToCheckLoginStatus)) $
      performRequestWithCookies postTokenReq

tokenPageUrl :: URI
tokenPageUrl = Unsafe.fromJust $ parseURI "https://life.gzic.scut.edu.cn/auth/login/cas/token"

tokenPageReq :: Request
tokenPageReq = Unsafe.fromJust $ requestFromURI tokenPageUrl

postTokenReq :: Request
postTokenReq =
  tokenPageReq
    { method = methodPost
    , checkResponse = throwErrorStatusCodes
    }

getTokenReq :: Request
getTokenReq =
  tokenPageReq
    { method = methodGet
    , checkResponse = throwErrorStatusCodes
    }

parseGetTokenResponse :: A.Value -> A.Parser Text
parseGetTokenResponse = withObject "GetTokenResponse" (.: "data")

retrieveToken :: forall (m :: Type -> Type). (MonadIO m) => AuthSession m Text
retrieveToken = do
  void $
    withError (withGenericHttpClientError (RetrieveTokenError . FailToSendPostTokenRequest)) $
      performRequestWithCookies postTokenReq

  respBS <-
    fmap responseBody $
      withError (withGenericHttpClientError (RetrieveTokenError . FailToSendGetTokenRequest)) $
        performRequestWithCookies getTokenReq

  liftEither $
    first (RetrieveTokenError . FailToParseToken) $
      A.eitherDecode @A.Value respBS >>= A.parseEither parseGetTokenResponse

fuckedUpDes :: [Text] -> Text -> ByteString
fuckedUpDes passwords dat = foldl' encPass (textToBytes dat) passwords
  where
    padData :: Int -> ByteString -> ByteString
    padData chunkSize bytes =
      case BS.length bytes `mod` chunkSize of
        0 -> bytes
        m -> bytes <> BS.replicate (chunkSize - m) 0

    textToBytes :: Text -> ByteString
    textToBytes = padData 8 . encodeUtf16BE

    chunksOfBS :: Int -> ByteString -> [ByteString]
    chunksOfBS chunkSize bs
      | BS.length bs == 0 = []
      | otherwise =
          let (c, bs') = BS.splitAt chunkSize bs
           in c : chunksOfBS chunkSize bs'

    encPass :: ByteString -> Text -> ByteString
    encPass dat' password =
      let passwordBytes = textToBytes password
          passwordByteGroups = chunksOfBS 8 passwordBytes
          op dat'' p =
            let c :: DES = throwCryptoError $ cipherInit p
             in ecbEncrypt c $ padData 8 dat''
       in foldl' op dat' passwordByteGroups
