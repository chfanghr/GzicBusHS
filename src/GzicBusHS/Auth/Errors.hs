module GzicBusHS.Auth.Errors (
  SessionError (..),
  withGenericHttpClientError,
  LoginError (..),
  RetrieveTokenError (..),
  LoginTokenExtractionError (..),
) where

-- TODO(chfanghr): Better Show instances

data SessionError
  = LoginError LoginError
  | RetrieveTokenError RetrieveTokenError
  | GenericHttpClientError SomeException
  deriving stock (Generic, Show)

withGenericHttpClientError ::
  (SomeException -> SessionError) ->
  SessionError ->
  SessionError
withGenericHttpClientError f (GenericHttpClientError e) = f e
withGenericHttpClientError _ e = e

data LoginError
  = FailToLoadLoginPage SomeException
  | FailToExtractLoginToken LoginTokenExtractionError
  | FailToSendLoginRequest SomeException
  | FailToCheckLoginStatus SomeException
  deriving stock (Generic, Show)

data RetrieveTokenError
  = FailToSendPostTokenRequest SomeException
  | FailToSendGetTokenRequest SomeException
  | FailToParseToken Text
  deriving stock (Generic, Show)

data LoginTokenExtractionError = LoginTokenNotFound
  deriving stock (Generic, Show)
