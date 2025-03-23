module GzicBusHS.Auth.Errors (
  SessionError (..),
  withGenericHttpClientError,
  PasswordLoginError (..),
  QRLoginError (..),
  RetrieveTokenError (..),
  PasswordLoginTokenExtractionError (..),
  QRLoginTokenExtractionError (..),
) where

-- TODO(chfanghr): Better Show instances

data SessionError
  = PasswordLoginError PasswordLoginError
  | QRLoginError QRLoginError
  | RetrieveTokenError RetrieveTokenError
  | NotLoggedIn SomeException
  | GenericHttpClientError SomeException
  deriving stock (Generic, Show)

withGenericHttpClientError ::
  (SomeException -> SessionError) ->
  SessionError ->
  SessionError
withGenericHttpClientError f (GenericHttpClientError e) = f e
withGenericHttpClientError _ e = e

data PasswordLoginError
  = FailToLoadLoginPage SomeException
  | FailToExtractLoginToken PasswordLoginTokenExtractionError
  | FailToSendLoginRequest SomeException
  deriving stock (Generic, Show)

data QRLoginError
  = FailToCheckQrCodeScan SomeException
  | FailToExtractQRLoginToken QRLoginTokenExtractionError
  | FailToLoginWithToken SomeException
  | QRLoginTimeout
  deriving stock (Generic, Show)

data RetrieveTokenError
  = FailToSendPostTokenRequest SomeException
  | FailToSendGetTokenRequest SomeException
  | FailToParseToken Text
  deriving stock (Generic, Show)

data PasswordLoginTokenExtractionError = PasswordLoginTokenNotFound
  deriving stock (Generic, Show)

data QRLoginTokenExtractionError = QRLoginTokenNotFound
  deriving stock (Generic, Show)
