{-# LANGUAGE RecordWildCards #-}

module GzicBusHS.Auth.Cookies (
  PersistentCookieJar,
  mkPersistentCookieJar,
  unPersistentCookieJar,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  (.:),
 )
import Data.Aeson qualified as A
import Data.Base16.Types (extractBase16)
import Data.ByteString.Base16 (decodeBase16Untyped, encodeBase16)
import Data.Time (UTCTime)
import Network.HTTP.Client (Cookie (..), CookieJar, createCookieJar, destroyCookieJar)

newtype PersisitentByteString = PersisitentByteString ByteString

unPersisitentByteString :: PersisitentByteString -> ByteString
unPersisitentByteString = coerce

instance ToJSON PersisitentByteString where
  toJSON (PersisitentByteString bs) =
    toJSON $ extractBase16 $ encodeBase16 bs

instance FromJSON PersisitentByteString where
  parseJSON = A.withText "PersisitentByteString" $ \hexStr -> do
    case decodeBase16Untyped $ encodeUtf8 hexStr of
      Left err -> fail $ "failed to decode hex encoded string: " <> toString err
      Right bs -> pure $ PersisitentByteString bs

newtype PersistentCookie = PersistentCookie Cookie

unPersistentCookie :: PersistentCookie -> Cookie
unPersistentCookie = coerce

instance ToJSON PersistentCookie where
  toJSON (PersistentCookie (Cookie {..})) =
    A.object
      [ "name" .= PersisitentByteString cookie_name
      , "value" .= PersisitentByteString cookie_value
      , "expiry_time" .= cookie_expiry_time
      , "domain" .= PersisitentByteString cookie_domain
      , "path" .= PersisitentByteString cookie_path
      , "creation_time" .= cookie_creation_time
      , "last_access_time" .= cookie_last_access_time
      , "persistent" .= cookie_persistent
      , "host_only" .= cookie_host_only
      , "secure_only" .= cookie_secure_only
      , "cookie_http_only" .= cookie_http_only
      ]

instance FromJSON PersistentCookie where
  parseJSON = A.withObject "PersistentCookie" $ \obj -> do
    name :: PersisitentByteString <- obj .: "name"
    value :: PersisitentByteString <- obj .: "value"
    expiryTime :: UTCTime <- obj .: "expiry_time"
    domain :: PersisitentByteString <- obj .: "domain"
    path :: PersisitentByteString <- obj .: "path"
    creationTime :: UTCTime <- obj .: "creation_time"
    lastAccessTime :: UTCTime <- obj .: "last_access_time"
    persistent :: Bool <- obj .: "persistent"
    hostOnly :: Bool <- obj .: "host_only"
    secureOnly :: Bool <- obj .: "secure_only"
    cookieHttpOnly :: Bool <- obj .: "cookie_http_only"

    pure $
      PersistentCookie $
        Cookie
          (unPersisitentByteString name)
          (unPersisitentByteString value)
          expiryTime
          (unPersisitentByteString domain)
          (unPersisitentByteString path)
          creationTime
          lastAccessTime
          persistent
          hostOnly
          secureOnly
          cookieHttpOnly

newtype PersistentCookieJar = PersistentCookieJar [PersistentCookie]
  deriving stock (Generic)
  deriving newtype (FromJSON, ToJSON)

mkPersistentCookieJar :: CookieJar -> PersistentCookieJar
mkPersistentCookieJar =
  PersistentCookieJar
    . fmap PersistentCookie
    . filter cookie_persistent
    . destroyCookieJar

unPersistentCookieJar :: PersistentCookieJar -> CookieJar
unPersistentCookieJar (PersistentCookieJar cookies) =
  createCookieJar $
    fmap unPersistentCookie cookies
