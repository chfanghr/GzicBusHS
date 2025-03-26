{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.Bus.APIClient (
  APIClientError (..),
  HTTPClientError (..),
  APIClientEnv,
  APIClient,
  runAPIClient,
  newBusClientEnv,
  checkTokenValidity,
  PageNum,
  mkPageNum,
  unPageNum,
  increasePageNum,
  PageSize,
  mkPageSize,
  unPageSize,
  TicketStatus (..),
  listTickets,
  queryTicketDetails,
  cancelTicket,
  removeTicket,
  querySchedule,
  bookTickets,
) where

import Control.Composition ((.*))
import Control.Exception (catch)
import Control.Monad.Error.Class (MonadError (throwError), withError)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, runStderrLoggingT)
import Data.Aeson qualified as A
import Data.Default (Default (def))
import Data.Traversable (for)
import GzicBusHS.Bus.DomainTypes (
  BookTicketsRequest,
  GenericResponseWrapper,
  IsAdditionalFieldInResponse,
  ListTicketsResponse,
  QueryScheduleRequest,
  QueryScheduleResponse,
  QueryTicketDetailsResponse,
 )
import Network.HTTP.Client (
  Manager,
  Request (checkResponse, method, requestBody, requestHeaders),
  RequestBody (RequestBodyLBS),
  Response (responseBody),
  httpLbs,
  requestFromURI,
  throwErrorStatusCodes,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (QueryItem, hAuthorization, hContentType, hUserAgent, methodGet, methodPost, renderQuery)
import Network.URI (URI (uriQuery), parseRelativeReference, parseURI, relativeTo)
import Optics (ViewableOptic (gview), makeFieldLabelsNoPrefix, (^.))
import Relude.Unsafe qualified as Unsafe

data APIClientError
  = GenericHttpClientError HTTPClientError
  | ExpiredTokenError HTTPClientError
  deriving stock (Generic)

data HTTPClientError
  = HTTPClientException SomeException
  | JSONDecodingError Text
  | BadResponseCode Int Text
  deriving stock (Generic)

data APIClientEnv = APIClientEnv
  { connectionManager :: Manager
  , token :: Text
  }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''APIClientEnv

withGenericHttpClientError ::
  (HTTPClientError -> APIClientError) ->
  APIClientError ->
  APIClientError
withGenericHttpClientError f (GenericHttpClientError e) = f e
withGenericHttpClientError _ e = e

newtype APIClient m a
  = APIClient
      ( ExceptT
          APIClientError
          (ReaderT APIClientEnv (LoggingT m))
          a
      )
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader APIClientEnv
    , MonadError APIClientError
    , MonadLogger
    )

runAPIClient ::
  forall (a :: Type) (m :: Type -> Type).
  (MonadIO m) =>
  (HasCallStack) =>
  APIClient m a ->
  APIClientEnv ->
  m (Either APIClientError a)
runAPIClient (APIClient inner) env =
  runStderrLoggingT $
    usingReaderT env $
      runExceptT inner

instance MonadTrans APIClient where
  lift = APIClient . lift . lift . lift

newBusClientEnv ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  (HasCallStack) =>
  Text ->
  m APIClientEnv
newBusClientEnv token = do
  connectionManager <- newTlsManager
  pure $
    APIClientEnv
      { connectionManager
      , token
      }

userAgent :: Text
userAgent = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.2 (KHTML, like Gecko) Chrome/22.0.1216.0 Safari/537.2"

performRequestWithToken ::
  forall (a :: Type) (m :: Type -> Type).
  ( MonadIO m
  , A.FromJSON a
  , IsAdditionalFieldInResponse a
  , Show a
  ) =>
  (HasCallStack) =>
  Request ->
  APIClient m (Response (Maybe a))
performRequestWithToken req = do
  token <- gview #token
  manager <- gview #connectionManager

  let headers = requestHeaders req
      headers' =
        [ (hAuthorization, encodeUtf8 token)
        , (hUserAgent, encodeUtf8 userAgent)
        , (hContentType, "application/json")
        ]
          ++ headers

      req' =
        req
          { requestHeaders = headers'
          , checkResponse = throwErrorStatusCodes
          }

  logDebugN $ "final request: " <> show req'

  resp <-
    APIClient $
      ExceptT $
        liftIO $
          catch
            (Right <$> httpLbs req' manager)
            (pure . Left . GenericHttpClientError . HTTPClientException)

  for resp $ \body -> do
    wrappedResponse <-
      either
        ( throwError
            . GenericHttpClientError
            . JSONDecodingError
            . fromString
        )
        pure
        . (A.eitherDecode' @(GenericResponseWrapper a))
        $ body

    logDebugN $ "decoded body: " <> show wrappedResponse

    unless (wrappedResponse ^. #code == 200) $
      throwError $
        GenericHttpClientError $
          BadResponseCode (wrappedResponse ^. #code) (wrappedResponse ^. #msg)

    pure $ wrappedResponse ^. #additionalField

performRequestWithTokenUnwrap ::
  forall (a :: Type) (m :: Type -> Type).
  ( MonadIO m
  , A.FromJSON a
  , IsAdditionalFieldInResponse a
  , Show a
  ) =>
  (HasCallStack) =>
  Request ->
  APIClient m a
performRequestWithTokenUnwrap =
  fmap (Unsafe.fromJust . responseBody)
    . performRequestWithToken

authInfoURL :: URI
authInfoURL = Unsafe.fromJust $ parseURI "https://life.gzic.scut.edu.cn/auth/info"

authInfoReq :: Request
authInfoReq = Unsafe.fromJust $ requestFromURI authInfoURL

getAuthInfoReq :: Request
getAuthInfoReq = authInfoReq {method = methodGet}

checkTokenValidity ::
  forall (m :: Type -> Type).
  (MonadIO m) =>
  (HasCallStack) =>
  APIClient m ()
checkTokenValidity = do
  logDebugN "making sure that auth token is not expired"

  void $
    withError (withGenericHttpClientError ExpiredTokenError) $
      performRequestWithToken @Void getAuthInfoReq

commuteOrderAPIBaseURL :: URI
commuteOrderAPIBaseURL =
  Unsafe.fromJust $ parseURI "https://life.gzic.scut.edu.cn/commute/open/commute/commuteOrder"

mkCommuteOrderAPIEndpointURL :: URI -> [QueryItem] -> URI
mkCommuteOrderAPIEndpointURL path query =
  (path `relativeTo` commuteOrderAPIBaseURL)
    { uriQuery = decodeUtf8 $ renderQuery True query
    }

mustParseRelativeReference :: (HasCallStack) => String -> URI
mustParseRelativeReference = Unsafe.fromJust . parseRelativeReference

mkGetReqFromURL :: URI -> Request
mkGetReqFromURL =
  (\req -> req {method = methodGet})
    . Unsafe.fromJust
    . requestFromURI

mkPostReqFromURLWithBody ::
  forall (a :: Type).
  (A.ToJSON a) =>
  a ->
  URI ->
  Request
mkPostReqFromURLWithBody x =
  ( \req ->
      req
        { method = methodPost
        , requestBody = RequestBodyLBS $ A.encode x
        }
  )
    . Unsafe.fromJust
    . requestFromURI

newtype PageNum = PageNum Word
  deriving stock (Generic, Show)

instance Default PageNum where
  def = PageNum 1

mkPageNum :: Word -> Maybe PageNum
mkPageNum = \case
  0 -> Nothing
  n -> Just $ PageNum n

increasePageNum :: PageNum -> PageNum
increasePageNum (PageNum x) = PageNum $ x + 1

unPageNum :: PageNum -> Word
unPageNum = coerce

newtype PageSize = PageSize Word
  deriving stock (Generic, Show)

instance Default PageSize where
  def = PageSize 8

mkPageSize :: Word -> Maybe PageSize
mkPageSize = \case
  0 -> Nothing
  n -> Just $ PageSize n

unPageSize :: PageSize -> Word
unPageSize = coerce

listTicketsPath :: URI
listTicketsPath = mustParseRelativeReference "orderFindAll"

data TicketStatus
  = AllReserved
  | ReservedUnused
  | Missed
  | Unrated
  deriving stock (Generic, Show)

instance Default TicketStatus where
  def = AllReserved

ticketStatusToEnum :: TicketStatus -> Int
ticketStatusToEnum AllReserved = 0
ticketStatusToEnum ReservedUnused = 1
ticketStatusToEnum Missed = 2
ticketStatusToEnum Unrated = 3

mkListTicketsURL ::
  TicketStatus ->
  Maybe (PageNum, PageSize) ->
  URI
mkListTicketsURL ticketStatus pageConfig =
  let (pageNum, pageSize) =
        maybe
          (0, 0)
          (bimap unPageNum unPageSize)
          pageConfig
   in mkCommuteOrderAPIEndpointURL
        listTicketsPath
        [ ("status", Just $ show $ ticketStatusToEnum ticketStatus)
        , ("pageNum", Just $ show pageNum)
        , ("pageSize", Just $ show pageSize)
        ]

mkListTicketsReq ::
  TicketStatus ->
  Maybe (PageNum, PageSize) ->
  Request
mkListTicketsReq = mkGetReqFromURL .* mkListTicketsURL

listTickets ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  TicketStatus ->
  Maybe (PageNum, PageSize) ->
  APIClient m ListTicketsResponse
listTickets ticketStatus pageConfig = do
  logDebugN $
    "listing tickets: ticket status: "
      <> show ticketStatus
      <> ", page config: "
      <> show pageConfig

  let req = mkListTicketsReq ticketStatus pageConfig

  performRequestWithTokenUnwrap req

queryTicketDetailsPath :: URI
queryTicketDetailsPath = mustParseRelativeReference "ticketDetail"

mkQueryTicketDetailsURL :: Text -> URI
mkQueryTicketDetailsURL ticketId =
  mkCommuteOrderAPIEndpointURL
    queryTicketDetailsPath
    [ ("id", Just $ encodeUtf8 ticketId)
    ]

mkQueryTicketDetailsReq :: Text -> Request
mkQueryTicketDetailsReq = mkGetReqFromURL . mkQueryTicketDetailsURL

queryTicketDetails ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Text ->
  APIClient m QueryTicketDetailsResponse
queryTicketDetails ticketId = do
  logDebugN $ "querying ticket details: ticket id: " <> show ticketId

  let req = mkQueryTicketDetailsReq ticketId

  performRequestWithTokenUnwrap req

cancelTicketPath :: URI
cancelTicketPath = mustParseRelativeReference "cancelTicket"

mkCancelTicketURL :: Text -> URI
mkCancelTicketURL ticketId =
  mkCommuteOrderAPIEndpointURL
    cancelTicketPath
    [ ("id", Just $ encodeUtf8 ticketId)
    ]

mkCancelTicketRequest :: Text -> Request
mkCancelTicketRequest = mkGetReqFromURL . mkCancelTicketURL

cancelTicket ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Text ->
  APIClient m ()
cancelTicket ticketId = do
  logDebugN $ "cancel ticket: ticket id: " <> show ticketId

  let req = mkCancelTicketRequest ticketId

  void $ performRequestWithToken @Void req

removeTicketPath :: URI
removeTicketPath = mustParseRelativeReference "removeOrderCanal"

mkRemoveTicketURL :: Text -> URI
mkRemoveTicketURL ticketId =
  mkCommuteOrderAPIEndpointURL
    removeTicketPath
    [ ("id", Just $ encodeUtf8 ticketId)
    ]

mkRemoveTicketRequest :: Text -> Request
mkRemoveTicketRequest = mkGetReqFromURL . mkRemoveTicketURL

removeTicket ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  Text ->
  APIClient m ()
removeTicket ticketId = do
  logDebugN $ "remove ticket: ticket id: " <> show ticketId

  let req = mkRemoveTicketRequest ticketId

  void $ performRequestWithToken @Void req

querySchedulePath :: URI
querySchedulePath = mustParseRelativeReference "frequencyChoice"

mkQueryScheduleURL :: Maybe (PageNum, PageSize) -> URI
mkQueryScheduleURL pageConfig =
  let (pageNum, pageSize) =
        maybe
          (0, 0)
          (bimap unPageNum unPageSize)
          pageConfig
   in mkCommuteOrderAPIEndpointURL
        querySchedulePath
        [ ("pageNum", Just $ show pageNum)
        , ("pageSize", Just $ show pageSize)
        ]

mkQueryScheduleRequest ::
  QueryScheduleRequest ->
  Maybe (PageNum, PageSize) ->
  Request
mkQueryScheduleRequest body =
  mkPostReqFromURLWithBody body
    . mkQueryScheduleURL

querySchedule ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  QueryScheduleRequest ->
  Maybe (PageNum, PageSize) ->
  APIClient m QueryScheduleResponse
querySchedule params pageConfig = do
  logDebugN $
    "query schedule: params: "
      <> show params
      <> ", page config: "
      <> show pageConfig

  let req = mkQueryScheduleRequest params pageConfig

  performRequestWithTokenUnwrap req

bookTicketsPath :: URI
bookTicketsPath = mustParseRelativeReference "submitTicket"

bookTicketsURL :: URI
bookTicketsURL = mkCommuteOrderAPIEndpointURL bookTicketsPath []

mkBookTicketRequest :: BookTicketsRequest -> Request
mkBookTicketRequest = flip mkPostReqFromURLWithBody bookTicketsURL

bookTickets ::
  forall (m :: Type -> Type).
  (MonadIO m, HasCallStack) =>
  BookTicketsRequest ->
  APIClient m ()
bookTickets params = do
  logDebugN $ "book tickets: params: " <> show params

  let req = mkBookTicketRequest params

  void $ performRequestWithToken @Void req

-- TODO(chfanghr): Better error reporting during api calls
