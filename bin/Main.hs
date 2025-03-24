module Main (main) where

import Codec.QRCode qualified as QRCode
import Control.Monad.Logger (LogLevel (LevelDebug))
import Data.Text.Lazy.Builder qualified as TBuilder
import GzicBusHS.Auth.QRLogin qualified as QRLogin
import GzicBusHS.Auth.Session qualified as Session
import Main.Utf8 qualified as Utf8
import Network.URI (URI)
import Relude.Unsafe qualified as Unsafe

main :: (HasCallStack) => IO ()
main = Utf8.withUtf8 $ do
  env <- Session.newSessionEnv

  void $
    Session.runSession
      (QRLogin.login presentQRCode Nothing)
      env
      Session.emptySessionState
      LevelDebug

qrCodeOptions :: QRCode.QRCodeOptions
qrCodeOptions = QRCode.defaultQRCodeOptions QRCode.L

presentQRCode :: (HasCallStack) => URI -> IO ()
presentQRCode uri = do
  putTextLn $ encodeQRImageToText False $ encodeURIToQRImage uri
  hFlush stdout

encodeURIToQRImage :: (HasCallStack) => URI -> QRCode.QRImage
encodeURIToQRImage =
  Unsafe.fromJust
    . QRCode.encodeText qrCodeOptions QRCode.Iso8859_1
    . show @Text

encodeQRImageToText :: Bool -> QRCode.QRImage -> Text
encodeQRImageToText invertColor qr =
  toStrict $
    TBuilder.toLazyTextWith bufferSize builder
  where
    bitmap :: [[Bool]]
    bitmap = QRCode.toMatrix True False qr

    blackSquare, whiteSquare :: Char
    blackSquare = '\x2588'
    whiteSquare = ' '

    repeatSquare :: Int
    repeatSquare = 2

    bufferSize = (repeatSquare *) $ sum $ length <$> bitmap

    builder :: TBuilder.Builder
    builder =
      foldMap
        ( foldMap
            ( mtimesDefault repeatSquare
                . TBuilder.singleton
                . ( \case
                      True -> blackSquare
                      False -> whiteSquare
                  )
                . xor invertColor
            )
        )
        bitmap
