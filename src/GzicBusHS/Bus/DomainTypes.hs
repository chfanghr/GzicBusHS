{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module GzicBusHS.Bus.DomainTypes (
  DateYYMMDD,
  mkDateYYMMDD,
  TimeHHMM,
  mkTimeHHMM,
  TicketInfo (..),
  BusInfo (..),
  ListTicketsResponse (..),
  QueryScheduleRequest (..),
  QueryScheduleResponse (..),
  OneTicketPlease (..),
  BookTicketsRequest (..),
  QueryTicketDetailsResponse (..),
  GenericResponseWrapper (..),
  IsAdditionalFieldInResponse,
) where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as AMap
import Data.Aeson.Types qualified as A (Parser)
import Data.Traversable (for)
import GHC.Read qualified as R
import GHC.Show qualified as S
import Optics ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Text.ParserCombinators.ReadP qualified as RP
import Text.ParserCombinators.ReadPrec qualified as RPrec

newtype JSONViaShowRead a = JSONViaShowRead a

instance (Show a) => A.ToJSON (JSONViaShowRead a) where
  toJSON (JSONViaShowRead x) = A.toJSON @String $ show x

instance (Read a) => A.FromJSON (JSONViaShowRead a) where
  parseJSON =
    A.parseJSON @String >=> \str -> do
      case readEither @a str of
        Right x -> pure $ JSONViaShowRead x
        Left err -> fail $ "fail to decode JSONViaShowRead: " <> toString err

data DateYYMMDD = DateYYMMDD
  { year :: Int
  , month :: Int
  , day :: Int
  }
  deriving stock (Generic)
  deriving (A.FromJSON, A.ToJSON) via (JSONViaShowRead DateYYMMDD)

mkDateYYMMDD :: Int -> Int -> Int -> Maybe DateYYMMDD
mkDateYYMMDD y m d = do
  guard $ 1000 <= y && y <= 9999
  guard $ 1 <= m && m <= 12
  guard $ 1 <= d && d <= 31

  pure $ DateYYMMDD y m d

instance S.Show DateYYMMDD where
  show d =
    intercalate
      "/"
      [ show $ d ^. #year
      , show $ d ^. #month
      , show $ d ^. #day
      ]

instance Read DateYYMMDD where
  readPrec = RPrec.lift $ do
    ints <- RP.sepBy (RPrec.readPrec_to_P (R.readPrec @Int) 0) (RP.char '/')
    (y, m, d) <- case ints of
      [y', m', d'] -> pure (y', m', d')
      _ -> fail "Invalid DateYYMMDD: expected three words seperated by '/'"
    maybe (fail "Invalid DateYYMMDD: date out of bound") pure $ mkDateYYMMDD y m d

data TimeHHMM = TimeHHMM
  { hour :: Int
  , minute :: Int
  }
  deriving stock (Generic)
  deriving (A.FromJSON, A.ToJSON) via (JSONViaShowRead TimeHHMM)

mkTimeHHMM :: Int -> Int -> Maybe TimeHHMM
mkTimeHHMM hour minute = do
  guard $ hour <= 23
  guard $ minute <= 59
  pure $ TimeHHMM hour minute

instance S.Show TimeHHMM where
  show t =
    intercalate
      ":"
      [ show $ t ^. #hour
      , show $ t ^. #minute
      ]

instance R.Read TimeHHMM where
  readPrec = RPrec.lift $ do
    ints <- RP.sepBy (RPrec.readPrec_to_P (R.readPrec @Int) 0) (RP.char ':')
    (h, m) <- case ints of
      [h', m'] -> pure (h', m')
      _ -> fail "Invalid TimeHHMM: expected two words seperated by ':'"
    maybe (fail "Invalid TimeHHMM: time out of bound") pure $ mkTimeHHMM h m

data TicketInfo = TickInfo
  { id :: Int
  , orderDate :: DateYYMMDD
  , departureDate :: DateYYMMDD
  , startTime :: TimeHHMM
  , endTime :: TimeHHMM
  , ruteName :: Text
  }
  deriving stock (Generic, Show)

instance A.ToJSON TicketInfo where
  toJSON i =
    A.object
      [ "id" .= (i ^. #id)
      , "orderDate" .= (i ^. #orderDate)
      , "dateDeparture" .= (i ^. #departureDate)
      , "startTime" .= (i ^. #startTime)
      , "endTime" .= (i ^. #endTime)
      , "ruteName" .= (i ^. #ruteName)
      ]

instance A.FromJSON TicketInfo where
  parseJSON = A.withObject "TickInfo" $ \obj ->
    TickInfo
      <$> obj .: "id"
      <*> obj .: "orderDate"
      <*> obj .: "dateDeparture"
      <*> obj .: "startTime"
      <*> obj .: "endTime"
      <*> obj .: "ruteName"

data Campus
  = GuangzhouInternational
  | UniversityTown
  | Wushan
  deriving stock (Generic)
  deriving (A.FromJSON, A.ToJSON) via (JSONViaShowRead Campus)

instance S.Show Campus where
  show GuangzhouInternational = "广州国际校区"
  show UniversityTown = "大学城校区"
  show Wushan = "五山校区"

instance R.Read Campus where
  readPrec =
    RPrec.lift $
      asum
        [ RP.string "广州国际校区" $> GuangzhouInternational
        , RP.string "大学城校区" $> UniversityTown
        , RP.string "五山校区" $> Wushan
        ]

data BusInfo = BusInfo
  { ids :: Text -- WTF
  , departureDate :: DateYYMMDD
  , startTime :: TimeHHMM
  , endTime :: TimeHHMM
  , startLocation :: Text
  , endLocation :: Text
  }
  deriving stock (Generic, Show)

busInfoToJSONObject :: BusInfo -> A.Object
busInfoToJSONObject i =
  AMap.fromList
    [ "ids" .= (i ^. #ids)
    , "dateDeparture" .= (i ^. #departureDate)
    , "startDate" .= (i ^. #startTime)
    , "endDate" .= (i ^. #endTime)
    , "startLocation" .= (i ^. #startLocation)
    , "downtown" .= (i ^. #endLocation)
    ]

busInfoFromJSONObject :: A.Object -> A.Parser BusInfo
busInfoFromJSONObject obj =
  BusInfo
    <$> obj .: "ids"
    <*> obj .: "dateDeparture"
    <*> obj .: "startDate"
    <*> obj .: "endDate"
    <*> obj .: "startLocation"
    <*> obj .: "downtown"

instance A.ToJSON BusInfo where
  toJSON = A.toJSON . busInfoToJSONObject

instance A.FromJSON BusInfo where
  parseJSON = A.withObject "BusInfo" busInfoFromJSONObject

newtype ListTicketsResponse = ListTicketsResponse
  { tickets :: [TicketInfo]
  }
  deriving stock (Generic, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

instance IsAdditionalFieldInResponse ListTicketsResponse where
  additionalFieldName = Just "list"

data QueryScheduleRequest = QueryScheduleRequest
  { startDate :: DateYYMMDD
  , startTime :: TimeHHMM
  , startCampus :: Campus
  , endDate :: DateYYMMDD
  , endTime :: TimeHHMM
  , endCampus :: Campus
  }
  deriving stock (Generic, Show)

instance A.ToJSON QueryScheduleRequest where
  toJSON p =
    A.object
      [ "startDate" .= (p ^. #startDate)
      , "startHsTime" .= (p ^. #startTime)
      , "startCampus" .= (p ^. #startCampus)
      , "endDate" .= (p ^. #endDate)
      , "endHsTime" .= (p ^. #endTime)
      , "endCampus" .= (p ^. #endCampus)
      ]

instance A.FromJSON QueryScheduleRequest where
  parseJSON = A.withObject "QueryScheduleRequest" $ \obj ->
    QueryScheduleRequest
      <$> obj .: "startDate"
      <*> obj .: "startHsTime"
      <*> obj .: "startCampus"
      <*> obj .: "endDate"
      <*> obj .: "endHsTime"
      <*> obj .: "endCampus"

newtype QueryScheduleResponse = QueryScheduleResponse
  { availableBuses :: [BusInfo]
  }
  deriving stock (Generic, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

instance IsAdditionalFieldInResponse QueryScheduleResponse where
  additionalFieldName = Just "list"

newtype OneTicketPlease = OneTicketPlease
  { busInfo :: BusInfo
  }
  deriving stock (Generic, Show)

instance A.ToJSON OneTicketPlease where
  toJSON (OneTicketPlease busInfo) =
    A.toJSON $
      AMap.fromList
        [ "tickets" .= (1 :: Int)
        , "ischecked" .= True
        , "subTickets" .= (1 :: Int)
        ]
        <> busInfoToJSONObject busInfo

instance A.FromJSON OneTicketPlease where
  parseJSON = A.withObject "OneTicketPlease" $ \obj -> do
    tickets :: Int <- obj .: "tickets"
    ischecked :: Bool <- obj .: "ischecked"
    subTickets :: Int <- obj .: "subTickets"

    guard (tickets == 1 && ischecked && subTickets == 1)

    OneTicketPlease <$> busInfoFromJSONObject obj

newtype BookTicketsRequest = BookTicketsRequest
  { tickets :: [OneTicketPlease]
  }
  deriving stock (Generic, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

newtype QueryTicketDetailsResponse = QueryTicketDetailsResponse
  { ticket :: TicketInfo
  }
  deriving stock (Generic, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

instance IsAdditionalFieldInResponse QueryTicketDetailsResponse where
  additionalFieldName = Just "data"

newtype QueryTicketsResponse = QueryTicketsResponse
  { tickets :: [TicketInfo]
  }
  deriving stock (Generic, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

instance IsAdditionalFieldInResponse QueryTicketsResponse where
  additionalFieldName = Just "list"

instance IsAdditionalFieldInResponse Void where
  additionalFieldName = Nothing

class IsAdditionalFieldInResponse a where
  additionalFieldName :: Maybe A.Key

data GenericResponseWrapper (a :: Type) = GenericResponse
  { code :: Int
  , msg :: Text
  , additionalField :: Maybe a
  }
  deriving stock (Generic, Show)

instance
  (A.ToJSON a, IsAdditionalFieldInResponse a) =>
  A.ToJSON (GenericResponseWrapper a)
  where
  toJSON o =
    A.object $
      mconcat
        [
          [ "code" .= (o ^. #code)
          , "msg" .= (o ^. #msg)
          ]
        , maybeToMonoid
            ( do
                k <- additionalFieldName @a
                pure $ one $ k .= (o ^. #additionalField)
            )
        ]

instance
  (A.FromJSON a, IsAdditionalFieldInResponse a) =>
  A.FromJSON (GenericResponseWrapper a)
  where
  parseJSON = A.withObject "GenericResponse" $ \obj ->
    GenericResponse
      <$> obj .: "code"
      <*> obj .: "msg"
      <*> (join <$> for (additionalFieldName @a) (obj .:?))

makeFieldLabelsNoPrefix ''DateYYMMDD
makeFieldLabelsNoPrefix ''TimeHHMM
makeFieldLabelsNoPrefix ''TicketInfo
makeFieldLabelsNoPrefix ''BusInfo
makeFieldLabelsNoPrefix ''QueryScheduleRequest
makeFieldLabelsNoPrefix ''QueryScheduleResponse
makeFieldLabelsNoPrefix ''OneTicketPlease
makeFieldLabelsNoPrefix ''BookTicketsRequest
makeFieldLabelsNoPrefix ''GenericResponseWrapper
makeFieldLabelsNoPrefix ''QueryTicketDetailsResponse

-- TODO(chfanghr): UTCTime -> Maybe (DateYYMMDD, TimeHHMM)
