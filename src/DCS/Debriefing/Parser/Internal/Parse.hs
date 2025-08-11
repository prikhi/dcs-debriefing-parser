module DCS.Debriefing.Parser.Internal.Parse where

import Control.Monad (forM_, (>=>))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (DiffTime, TimeOfDay, timeToTimeOfDay)

import DCS.Debriefing.Parser.Internal.Lexer (RawDebriefing (..), RawValue (..), Variable (..), displayRawValue, lexerHarness)
import DCS.Debriefing.Parser.Types


data ParseError
    = TopLevelValueMissing Text
    | TopLevelValueWrongType Text Text Text
    | ArrayParseError Text Int ValueParseError
    | -- | TODO: no top-level sparse arrays?
      SparseArrayParseError Text Int ValueParseError
    | MapParseError Text ValueParseError
    deriving stock (Show, Eq)


data ValueParseError
    = UnexpectedType Text Text
    | MapFieldMissing Text
    | UnknownEnumValue Text Text
    deriving stock (Show, Eq)


testParsing :: IO ()
testParsing = forM_ [1 .. 17] $ \(i :: Int) -> do
    let fileName = show i <> ".log"
    lexResult <- lexerHarness $ "fixtures/" <> fileName
    case lexResult of
        Left e ->
            putStrLn $ fileName <> ":\tlexing failed - " <> e
        Right r ->
            case parseDebriefing r of
                Left e ->
                    putStrLn $ fileName <> ":\tparsing failed - " <> show e
                Right _b ->
                    putStrLn $ fileName <> ":\tOK"


-- TOP LEVEL

parseDebriefing :: RawDebriefing -> Either ParseError DCSDebriefing
parseDebriefing (RawDebriefing vs) = do
    missionFilePath <- expectStringVar "mission_file_path"
    callsign <- expectStringVar "callsign"
    missionFileMark <- expectIntegerVar "mission_file_mark"
    missionTime <- parseMissionTime
    result <- expectIntegerVar "result"
    triggerState <- expectMapVar "triggers_state" parseTriggerState
    warehouses <- expectMapVar "warehouses" parseWarehouses
    events <- expectArrayVar "events" parseEvent
    pure
        DCSDebriefing
            { worldState = []
            , ..
            }
  where
    lookupVar :: Text -> Either ParseError RawValue
    lookupVar name =
        maybe (Left $ TopLevelValueMissing name) (Right . varValue) $
            L.find ((== name) . varName) vs
    expectStringVar :: Text -> Either ParseError Text
    expectStringVar name =
        lookupVar name >>= \case
            String s ->
                Right s
            v ->
                Left $ TopLevelValueWrongType name "String" $ displayRawValue v
    expectIntegerVar :: Text -> Either ParseError Integer
    expectIntegerVar name =
        lookupVar name >>= \case
            Number n ->
                case floatingOrInteger @Double n of
                    Left _ ->
                        Left $ TopLevelValueWrongType name "Integer" $ displayRawValue (Number n)
                    Right int ->
                        Right int
            v ->
                Left $ TopLevelValueWrongType name "Integer" $ displayRawValue v
    expectArrayVar :: Text -> (RawValue -> Either ValueParseError a) -> Either ParseError [a]
    expectArrayVar name valueParser =
        lookupVar name >>= \case
            Array xs ->
                mapM (\(ix, x) -> first (ArrayParseError name ix) $ valueParser x) $ zip [1 ..] xs
            v ->
                Left $ TopLevelValueWrongType name "Array" $ displayRawValue v
    _expectSpareArrayVar
        :: Text
        -> (RawValue -> Either ValueParseError a)
        -> Either ParseError (IntMap a)
    _expectSpareArrayVar name valueParser =
        lookupVar name >>= \case
            SparseArray m ->
                IM.traverseWithKey
                    ( \ix v ->
                        first (SparseArrayParseError name ix) $ valueParser v
                    )
                    m
            Array [] ->
                pure IM.empty
            v ->
                Left $ TopLevelValueWrongType name "SparseArray" $ displayRawValue v
    expectMapVar
        :: Text
        -> (HashMap Text RawValue -> Either ValueParseError a)
        -> Either ParseError a
    expectMapVar name mapParser =
        lookupVar name >>= \case
            Map m ->
                first (MapParseError name) $ mapParser m
            Array [] ->
                first (MapParseError name) $ mapParser HM.empty
            v ->
                Left $ TopLevelValueWrongType name "Map" $ displayRawValue v

    parseMissionTime :: Either ParseError DiffTime
    parseMissionTime =
        lookupVar "mission_time" >>= \case
            Number n ->
                Right $ realToFrac n
            v ->
                Left $ TopLevelValueWrongType "mission_time" "Number" $ displayRawValue v


-- VARIABLES

parseTriggerState :: HashMap Text RawValue -> Either ValueParseError (HashMap Text TriggerState)
parseTriggerState = traverse $ \case
    Map hm ->
        parseStateMap hm
    v ->
        Left $ UnexpectedType "Map" $ displayRawValue v
  where
    parseStateMap :: HashMap Text RawValue -> Either ValueParseError TriggerState
    parseStateMap hm = do
        time <- hm .: "time"
        value <- hm .: "value"
        pure TriggerState {..}


parseWarehouses :: HashMap Text RawValue -> Either ValueParseError Warehouses
parseWarehouses hm = do
    airports <- expectMapField "airports" hm >>= parseWarehouseMap "airports"
    warehouses <- expectMapField "warehouses" hm >>= parseWarehouseMap "warehouses"
    pure Warehouses {..}
  where
    -- fields may be array or sparse array depending on ID of first value
    parseWarehouseMap :: Text -> RawValue -> Either ValueParseError (IntMap WarehouseData)
    parseWarehouseMap warehouseType = \case
        SparseArray im ->
            IM.traverseWithKey (parseWarehouseData warehouseType) im
        Array xs ->
            fmap IM.fromList . mapM (\(ix, a) -> (ix,) <$> parseWarehouseData warehouseType ix a) $ zip [1 ..] xs
        v ->
            Left $ UnexpectedType (warehouseType <> ":Array|SparseArray") $ displayRawValue v

    parseWarehouseData :: Text -> Int -> RawValue -> Either ValueParseError WarehouseData
    parseWarehouseData warehouseType ix v = do
        am <- expectMapValue (warehouseType <> ":" <> T.pack (show ix)) v
        unlimitedFuel <- am .: "unlimitedFuel"
        unlimitedMunitions <- am .: "unlimitedMunitions"
        unlimitedAircrafts <- am .: "unlimitedAircrafts"
        pure WarehouseData {..}


parseEvent :: RawValue -> Either ValueParseError Event
parseEvent = \case
    Map hm ->
        parseEventMap hm
    v ->
        Left $ UnexpectedType "Array" $ displayRawValue v
  where
    parseEventMap :: HashMap Text RawValue -> Either ValueParseError Event
    parseEventMap hm = do
        eventId <- optionalMapField "event_id" hm & sequence . fmap (expectIntegerValue "event_id")
        time <- hm .: "t"
        linkedEventId <- optionalMapField "linked_event_id" hm & sequence . fmap (expectIntegerValue "linked_event_id")
        eventTypeTag <- hm .: "type"
        eventType <- parseEventType hm eventTypeTag
        pure Event {..}
    parseEventType :: HashMap Text RawValue -> Text -> Either ValueParseError EventType
    parseEventType hm = \case
        "mission start" -> do
            timeActual <- hm .: "ta"
            pure $ MissionStart MissionStartData {..}
        "under control" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            targetMissionId <- hm .: "targetMissionID"
            target <- hm .: "target"
            pure $ UnderControl UnderControlData {..}
        "took control" ->
            pure TookControl
        "weapon rearm" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ WeaponRearm WeaponRearmData {..}
        "engine startup" ->
            EngineStartup <$> parseEngineStartupData hm
        "engine shutdown" -> do
            -- Same exact fields as "engine starup" event
            EngineStartupData {..} <- parseEngineStartupData hm
            pure $ EngineShutdown EngineShutdownData {..}
        "takeoff" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            place <- hm .: "place"
            let placeDisplayName = hm .:? "placeDisplayName"
            pure $ Takeoff TakeoffData {..}
        "land" -> do
            -- Same exact fields as "engine starup" event
            EngineStartupData {..} <- parseEngineStartupData hm
            pure $ Land LandData {..}
        "start shooting" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            let target = hm .:? "target"
                targetMissionId = hm .:? "targetMissionID"
                targetPilotName = hm .:? "targetPilotName"
                targetCoalition = hm .:? "target_coalition"
                targetObjectId = hm .:? "target_object_id"
                targetUnitType = hm .:? "target_unit_type"
                targetWsType1 = hm .:? "target_ws_type1"
            weapon <- hm .: "weapon"
            pure $ StartShooting StartShootingData {..}
        "end shooting" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            ammoConsumption <- hm .: "ammo_consumption"
            weapon <- hm .: "weapon"
            weaponType <- hm .: "weapon_type"
            pure $ EndShooting EndShootingData {..}
        "shot" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            weapon <- hm .: "weapon"
            weaponType <- hm .: "weapon_type"
            let target = hm .:? "target"
                targetMissionId = hm .:? "targetMissionID"
                targetPilotName = hm .:? "targetPilotName"
                targetCoalition = hm .:? "target_coalition"
                targetObjectId = hm .:? "target_object_id"
                targetUnitType = hm .:? "target_unit_type"
                targetWsType1 = hm .:? "target_ws_type1"
            pure $ Shot ShotData {..}
        "hit" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            targetPilotName <- hm .: "targetPilotName"
            targetCoalition <- hm .: "target_coalition"
            targetObjectId <- hm .: "target_object_id"
            targetUnitType <- hm .: "target_unit_type"
            targetWsType1 <- hm .: "target_ws_type1"
            let weapon = hm .:? "weapon"
                weaponType = hm .:? "weapon_type"
            pure $ Hit HitData {..}
        "kill" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            targetPilotName <- hm .: "targetPilotName"
            targetCoalition <- hm .: "target_coalition"
            targetObjectId <- hm .: "target_object_id"
            targetUnitType <- hm .: "target_unit_type"
            targetWsType1 <- hm .: "target_ws_type1"
            weapon <- hm .: "weapon"
            let weaponType = hm .:? "weapon_type"
            pure $ Kill KillData {..}
        "bda" -> do
            let initiatorUnitType = hm .:? "initiator_unit_type"
                initiatorObjectId = hm .:? "initiator_object_id"
                initiatorPilotName = hm .:? "initiatorPilotName"
                initiatorCoalition = hm .:? "initiator_coalition"
                initiatorMissionID = hm .:? "initiatorMissionID"
                initiatorWsType1 = hm .:? "initiator_ws_type1"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            targetPilotName <- hm .: "targetPilotName"
            targetCoalition <- hm .: "target_coalition"
            targetObjectId <- hm .: "target_object_id"
            targetUnitType <- hm .: "target_unit_type"
            targetWsType1 <- hm .: "target_ws_type1"
            let weapon = hm .:? "weapon"
                weaponType = hm .:? "weapon_type"
            pure $ Bda BdaData {..}
        "score" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            amount <- hm .: "amount"
            comment <- hm .: "comment"
            pure $ Score ScoreData {..}
        "weapon drop" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ WeaponDrop WeaponDropData {..}
        "ai abort mission" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ AiAbortMission AiAbortMissionData {..}
        "pilot dead" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ PilotDead PilotDeadData {..}
        "eject" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            targetPilotName <- hm .: "targetPilotName"
            targetCoalition <- hm .: "target_coalition"
            targetObjectId <- hm .: "target_object_id"
            targetUnitType <- hm .: "target_unit_type"
            targetWsType1 <- hm .: "target_ws_type1"
            pure $ Eject EjectData {..}
        "pilot discard chair" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            targetPilotName <- hm .: "targetPilotName"
            targetCoalition <- hm .: "target_coalition"
            targetObjectId <- hm .: "target_object_id"
            targetUnitType <- hm .: "target_unit_type"
            targetWsType1 <- hm .: "target_ws_type1"
            pure $ PilotDiscardChair PilotDiscardChairData {..}
        "pilot landing" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ PilotLanding PilotLandingData {..}
        "failure" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            failure <- hm .: "failure"
            failureDisplayName <- hm .: "failureDisplayName"
            pure $ Failure FailureData {..}
        "crash" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            initiatorMissionID <- hm .: "initiatorMissionID"
            initiatorWsType1 <- hm .: "initiator_ws_type1"
            pure $ Crash CrashData{..}
        "relinquished" -> do
            initiatorUnitType <- hm .: "initiator_unit_type"
            initiatorObjectId <- hm .: "initiator_object_id"
            initiatorPilotName <- hm .: "initiatorPilotName"
            initiatorCoalition <- hm .: "initiator_coalition"
            target <- hm .: "target"
            targetMissionId <- hm .: "targetMissionID"
            pure $ Relinquished RelinquishedData {..}
        "mission end" -> do
            comment <- hm .: "comment"
            pure $ MissionEnd MissionEndData {..}
        unknown ->
            Left $ UnknownEnumValue "type" unknown
    parseEngineStartupData :: HashMap Text RawValue -> Either ValueParseError EngineStartupData
    parseEngineStartupData hm = do
        initiatorUnitType <- hm .: "initiator_unit_type"
        initiatorObjectId <- hm .: "initiator_object_id"
        initiatorPilotName <- hm .: "initiatorPilotName"
        initiatorCoalition <- hm .: "initiator_coalition"
        initiatorMissionID <- hm .: "initiatorMissionID"
        initiatorWsType1 <- hm .: "initiator_ws_type1"
        place <- hm .: "place"
        placeDisplayName <- hm .: "placeDisplayName"
        pure $ EngineStartupData {..}


-- BASE

expectStringValue :: Text -> RawValue -> Either ValueParseError Text
expectStringValue label = \case
    String s ->
        Right s
    v ->
        Left $ UnexpectedType (label <> ":String") $ displayRawValue v


expectNumberValue :: Text -> RawValue -> Either ValueParseError Scientific
expectNumberValue label = \case
    Number n ->
        Right n
    v ->
        Left $ UnexpectedType (label <> ":Number") $ displayRawValue v


expectIntegerValue :: Text -> RawValue -> Either ValueParseError Integer
expectIntegerValue label =
    expectNumberValue label
        >=> \n -> case floatingOrInteger n of
            Left (_ :: Double) ->
                Left $ UnexpectedType (label <> ":Integer") $ displayRawValue (Number n)
            Right i ->
                pure i


expectBooleanValue :: Text -> RawValue -> Either ValueParseError Bool
expectBooleanValue label = \case
    Boolean b ->
        Right b
    v ->
        Left $ UnexpectedType (label <> ":Boolean") $ displayRawValue v


expectSparseArrayValue :: Text -> RawValue -> Either ValueParseError (IntMap RawValue)
expectSparseArrayValue label = \case
    SparseArray m ->
        Right m
    v ->
        Left $ UnexpectedType (label <> ":SparseArray") $ displayRawValue v


expectMapValue :: Text -> RawValue -> Either ValueParseError (HashMap Text RawValue)
expectMapValue label = \case
    Map hm ->
        Right hm
    v ->
        Left $ UnexpectedType (label <> ":Map") $ displayRawValue v


expectMapField :: Text -> HashMap Text RawValue -> Either ValueParseError RawValue
expectMapField field hm =
    maybe (Left $ MapFieldMissing field) Right $ HM.lookup field hm


expectMapFieldValue
    :: Text
    -> HashMap Text RawValue
    -> (Text -> RawValue -> Either ValueParseError a)
    -> Either ValueParseError a
expectMapFieldValue field hm valueParser =
    expectMapField field hm >>= valueParser field


optionalMapField :: Text -> HashMap Text RawValue -> Maybe RawValue
optionalMapField = HM.lookup


-- MAP PARSING

class FromMap a where
    (.:) :: HashMap Text RawValue -> Text -> Either ValueParseError a


instance FromMap Text where
    hm .: field = expectMapFieldValue field hm expectStringValue


instance FromMap Integer where
    hm .: field = expectMapFieldValue field hm expectIntegerValue


instance FromMap Bool where
    hm .: field = expectMapFieldValue field hm expectBooleanValue


instance FromMap DiffTime where
    hm .: field = realToFrac <$> expectMapFieldValue field hm expectNumberValue


instance FromMap TimeOfDay where
    hm .: field = timeToTimeOfDay <$> hm .: field


-- | TODO: keep this or just use .:?
instance (FromMap a) => FromMap (Maybe a) where
    hm .: field =
        Right $ case hm .: field of
            Left _ ->
                Nothing
            Right r ->
                Just r


(.:?) :: (FromMap a) => HashMap Text RawValue -> Text -> Maybe a
(.:?) hm field =
    case hm .: field of
        Left _ ->
            Nothing
        Right a ->
            Just a
