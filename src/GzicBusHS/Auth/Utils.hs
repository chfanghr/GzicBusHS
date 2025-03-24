module GzicBusHS.Auth.Utils (
  performRequestWithCookies,
  performRequest,
  fuckedUpDes,
  retryWithErrorFilter,
  genFuckedUpTimeBasedV4UUID,
  fromSingletonCaptureGroup,
) where

import Control.Exception (catch)
import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.Logger (MonadLogger, logWarnN)
import Crypto.Cipher.DES (DES)
import Crypto.Cipher.Types (BlockCipher (ecbEncrypt), Cipher (cipherInit))
import Crypto.Error (throwCryptoError)
import Data.Bits (Bits ((.&.), (.|.)))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding (encodeUtf16BE)
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
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
import Optics (Field1 (_1), Field2 (_2), ViewableOptic (gview), guse)
import Optics.State.Operators ((%%=), (.=), (<<%=))
import Relude.Unsafe ((!!))
import Relude.Unsafe qualified as Unsafe
import System.Random (RandomGen (genWord64R), StdGen)
import Text.Regex.TDFA (MatchResult (mrSubList))

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

        prevCookies = maybeToMonoid maybePrevCookies

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

genFuckedUpTimeBasedV4UUID :: UTCTime -> StdGen -> UUID
genFuckedUpTimeBasedV4UUID currentTime = runGen
  where
    millisSinceEpoch :: UTCTime -> Word64
    millisSinceEpoch =
      floor
        . (* 1e3)
        . nominalDiffTimeToSeconds
        . utcTimeToPOSIXSeconds

    ub :: Word64
    ub = 2 ^ (60 :: Int)

    genChar :: Char -> State (Word64, StdGen) Char
    genChar '-' = pure '-'
    genChar '4' = pure '4'
    genChar ch = do
      d <- _1 <<%= (`div` 16)
      s <- _2 %%= genWord64R ub

      let r = (d + ((s * 16) `div` ub)) `mod` 16

      pure $ case ch of
        'x' -> hexDigits !! fromIntegral r
        'y' -> hexDigits !! fromIntegral (r .&. 0x3 .|. 0x8)
        _ -> error $ "bad character in uuid template: " <> toText uuidTemplate

    genUUIDStr :: State (Word64, StdGen) [Char]
    genUUIDStr = traverse genChar uuidTemplate

    uuidTemplate :: [Char]
    uuidTemplate = "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"

    hexDigits :: [Char]
    hexDigits = ['0' .. '9'] ++ ['a' .. 'f']

    runGen :: StdGen -> UUID
    runGen rng =
      let currentTimestamp = millisSinceEpoch currentTime
          uuidStr = evalState genUUIDStr (currentTimestamp, rng)
          uuid = Unsafe.fromJust $ UUID.fromString uuidStr
       in uuid

fromSingletonCaptureGroup :: MatchResult Text -> Text
fromSingletonCaptureGroup =
  mrSubList >>> \case
    [x] -> x
    xs -> error $ "expected one capture group, got: " <> show xs
