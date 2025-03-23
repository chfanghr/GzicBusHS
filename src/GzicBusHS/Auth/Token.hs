module GzicBusHS.Auth.Token (
  retrieveToken,
  postTokenReq,
) where

import Control.Monad.Error.Class (liftEither, withError)
import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import GzicBusHS.Auth.Errors (RetrieveTokenError (FailToParseToken, FailToSendGetTokenRequest, FailToSendPostTokenRequest), SessionError (RetrieveTokenError), withGenericHttpClientError)
import GzicBusHS.Auth.Session (Session)
import GzicBusHS.Auth.Utils (performRequestWithCookies)
import Network.HTTP.Client (Request (checkResponse, method), Response (responseBody), requestFromURI, throwErrorStatusCodes)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Method (methodGet)
import Network.URI (URI, parseURI)
import Relude.Unsafe qualified as Unsafe

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
parseGetTokenResponse = A.withObject "GetTokenResponse" (.: "data")

retrieveToken ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Session m Text
retrieveToken = do
  void $
    withError (withGenericHttpClientError (RetrieveTokenError . FailToSendPostTokenRequest)) $
      performRequestWithCookies postTokenReq

  respBS <-
    fmap responseBody $
      withError (withGenericHttpClientError (RetrieveTokenError . FailToSendGetTokenRequest)) $
        performRequestWithCookies getTokenReq

  liftEither $
    first (RetrieveTokenError . FailToParseToken . toText) $
      A.eitherDecode respBS >>= A.parseEither parseGetTokenResponse
