module GzicBusHS.Auth.Utils (
  performRequestWithCookies,
  performRequest,
  fuckedUpDes,
  retryWithErrorFilter,
) where

import Control.Exception (catch)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Logger (MonadLogger, logWarnN)
import Crypto.Cipher.DES (DES)
import Crypto.Cipher.Types (BlockCipher (ecbEncrypt), Cipher (cipherInit))
import Crypto.Error (throwCryptoError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf16BE)
import Data.Time (getCurrentTime)
import GzicBusHS.Auth.Errors (SessionError (..))
import GzicBusHS.Auth.Session (Session (Session))
import Network.HTTP.Client (
  CookieJar,
  Request (cookieJar, requestHeaders),
  Response,
  httpLbs,
  updateCookieJar,
 )
import Network.HTTP.Types (hUserAgent)
import Optics (ViewableOptic (gview), guse)
import Optics.State.Operators ((.=))

userAgent :: Text
userAgent = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.2 (KHTML, like Gecko) Chrome/22.0.1216.0 Safari/537.2"

performRequestWithCookies ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  Request ->
  Session m (Response LBS.ByteString)
performRequestWithCookies req =
  do
    cookies <- guse #cookies
    (cookies', resp') <- performRequestWithCookies' (Just cookies) req
    #cookies .= cookies'

    pure resp'

performRequestWithCookies' ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  Maybe CookieJar ->
  Request ->
  Session m (CookieJar, Response LBS.ByteString)
performRequestWithCookies' maybePrevCookies req =
  do
    manager <- gview #connectionManager

    let headers = requestHeaders req
        headers' = (hUserAgent, encodeUtf8 userAgent) : headers

        prevCookies = fromJust mempty maybePrevCookies

        req' =
          req
            { cookieJar = Just prevCookies
            , requestHeaders = headers'
            }

    resp <-
      Session $
        ExceptT $
          liftIO $
            catch
              (Right <$> httpLbs req' manager)
              (\(e :: SomeException) -> pure $ Left $ GenericHttpClientError e)

    now <- liftIO getCurrentTime
    pure $ updateCookieJar resp req' now prevCookies

performRequest ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  Request ->
  Session m (Response LBS.ByteString)
performRequest = fmap snd . performRequestWithCookies' Nothing

fuckedUpDes :: [Text] -> Text -> ByteString
fuckedUpDes passwords dat = foldl' encPass (textToBytes dat) passwords
  where
    padBytes :: Int -> ByteString -> ByteString
    padBytes chunkSize bytes =
      case BS.length bytes `mod` chunkSize of
        0 -> bytes
        m -> bytes <> BS.replicate (chunkSize - m) 0

    textToBytes :: Text -> ByteString
    textToBytes = padBytes 8 . encodeUtf16BE

    chunksOfBS :: Int -> ByteString -> [ByteString]
    chunksOfBS chunkSize bs
      | BS.length bs == 0 = []
      | otherwise =
          let (c, bs') = BS.splitAt chunkSize bs
           in c : chunksOfBS chunkSize bs'

    encPass :: ByteString -> Text -> ByteString
    encPass bytes password =
      let passwordBytes = textToBytes password
          passwordByteGroups = chunksOfBS 8 passwordBytes
          op bytes' p =
            let c :: DES = throwCryptoError $ cipherInit p
             in ecbEncrypt c $ padBytes 8 bytes'
       in foldl' op bytes passwordByteGroups

retryWithErrorFilter ::
  ( MonadLogger (Session m)
  , HasCallStack
  , Monad m
  ) =>
  (SessionError -> Bool) ->
  Session m a ->
  Session m a
retryWithErrorFilter f a = catchError a $ \e -> do
  logWarnN $ "error encountered: " <> show e
  if f e
    then retryWithErrorFilter f a
    else throwError e
