module GzicBusHS.Auth (
  Session,
  SessionState,
  SessionEnv,
  emptySessionState,
  newSessionEnv,
  runSession,
  qrLogin,
  passwordLogin,
  checkLoginStatus,
  retrieveToken,
  PasswordLoginError (..),
  PasswordLoginTokenExtractionError (..),
  QRLoginError (..),
  QRLoginTokenExtractionError (..),
  RetrieveTokenError (..),
  SessionError (..),
) where

import Data.Time (NominalDiffTime)
import GzicBusHS.Auth.Errors (
  PasswordLoginError (..),
  PasswordLoginTokenExtractionError (..),
  QRLoginError (..),
  QRLoginTokenExtractionError (..),
  RetrieveTokenError (..),
  SessionError (..),
 )
import GzicBusHS.Auth.PasswordLogin qualified as PasswordLogin
import GzicBusHS.Auth.QRLogin qualified as QRLogin
import GzicBusHS.Auth.Session (
  Session,
  SessionEnv,
  SessionState,
  emptySessionState,
  newSessionEnv,
  runSession,
 )
import GzicBusHS.Auth.Token (checkLoginStatus, retrieveToken)
import Network.URI (URI)

qrLogin ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  (URI -> m ()) ->
  Maybe NominalDiffTime ->
  Session m ()
qrLogin = QRLogin.login

passwordLogin ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  m Text ->
  Text ->
  Text ->
  Session m ()
passwordLogin = PasswordLogin.login
