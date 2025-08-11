{-# LANGUAGE DuplicateRecordFields #-}

module DCS.Debriefing.Parser.Types where

import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.Time (DiffTime, TimeOfDay)


data DCSDebriefing = DCSDebriefing
    { missionFilePath :: Text
    , callsign :: Text
    , worldState :: [()]
    -- ^ TODO
    , missionFileMark :: Integer
    , missionTime :: DiffTime
    , warehouses :: Warehouses
    , triggerState :: HashMap Text TriggerState
    , result :: Integer
    , events :: [Event]
    }
    deriving stock (Show, Eq)


data TriggerState = TriggerState
    { time :: DiffTime
    , value :: Integer
    }
    deriving stock (Show, Eq)


-- | TODO: there is a `warehouses` map nested in here as well but I don't
-- have an example file w/ data in it.
data Warehouses = Warehouses
    { airports :: IntMap WarehouseData
    , warehouses :: IntMap WarehouseData
    }
    deriving stock (Show, Eq)


-- | This also has an 'aircraft' key w/ 'planes' & 'helicopter' values
-- under it.
data WarehouseData = WarehouseData
    { unlimitedFuel :: Bool
    , unlimitedMunitions :: Bool
    , unlimitedAircrafts :: Bool
    }
    deriving stock (Show, Eq)


data Event = Event
    { eventId :: Maybe Integer
    -- ^ Maybe because only UnderControl event doesn't have this field
    , linkedEventId :: Maybe Integer
    -- ^ Maybe because only UnderControl event doesn't have this field
    , time :: DiffTime
    , eventType :: EventType
    }
    deriving stock (Show, Eq)


-- | TODO: collect more debriefing logs, collect more events
data EventType
    = MissionStart MissionStartData
    | UnderControl UnderControlData
    | TookControl
    | WeaponRearm WeaponRearmData
    | EngineStartup EngineStartupData
    | EngineShutdown EngineShutdownData
    | Takeoff TakeoffData
    | Land LandData
    | StartShooting StartShootingData
    | EndShooting EndShootingData
    | Shot ShotData
    | Hit HitData
    | Kill KillData
    | Score ScoreData
    | Bda BdaData
    | WeaponDrop WeaponDropData
    | AiAbortMission AiAbortMissionData
    | PilotDead PilotDeadData
    | Eject EjectData
    | PilotDiscardChair PilotDiscardChairData
    | PilotLanding PilotLandingData
    | Failure FailureData
    | Crash CrashData
    | Relinquished RelinquishedData
    | MissionEnd MissionEndData
    deriving stock (Show, Eq)


data MissionStartData = MissionStartData
    { timeActual :: TimeOfDay
    }
    deriving stock (Show, Eq)


data UnderControlData = UnderControlData
    { initiatorUnitType :: Text
    , initiatorObjectId :: Integer
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , targetMissionId :: Text
    , target :: Text
    }
    deriving stock (Show, Eq)

data WeaponRearmData = WeaponRearmData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)

data EngineStartupData = EngineStartupData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , place :: Text
    , placeDisplayName :: Text
    }
    deriving stock (Show, Eq)


data EngineShutdownData = EngineShutdownData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , place :: Text
    , placeDisplayName :: Text
    }
    deriving stock (Show, Eq)


data TakeoffData = TakeoffData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , place :: Text
    , placeDisplayName :: Maybe Text
    }
    deriving stock (Show, Eq)


data LandData = LandData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , place :: Text
    , placeDisplayName :: Text
    }
    deriving stock (Show, Eq)

data StartShootingData = StartShootingData
    { initiatorUnitType :: Text
    , initiatorObjectId :: Integer
    , initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Maybe Text
    , targetMissionId :: Maybe Text
    , targetPilotName :: Maybe Text
    , targetCoalition :: Maybe Integer
    , targetObjectId :: Maybe Integer
    , targetUnitType :: Maybe Text
    , targetWsType1 :: Maybe Integer
    -- ^ No idea what this is
    , weapon :: Text
    }
    deriving stock (Show, Eq)

data EndShootingData = EndShootingData
    { initiatorUnitType :: Text
    , initiatorObjectId :: Integer
    , initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorWsType1 :: Integer
    , weapon :: Text
    , weaponType :: Integer
    , ammoConsumption :: Integer
    }
    deriving stock (Show, Eq)


data ShotData = ShotData
    { initiatorUnitType :: Text
    , initiatorObjectId :: Integer
    , initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Maybe Text
    , targetMissionId :: Maybe Text
    , targetPilotName :: Maybe Text
    , targetCoalition :: Maybe Integer
    , targetObjectId :: Maybe Integer
    , targetUnitType :: Maybe Text
    , targetWsType1 :: Maybe Integer
    -- ^ No idea what this is
    , weapon :: Text
    , weaponType :: Integer
    }
    deriving stock (Show, Eq)


data HitData = HitData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Text
    , targetMissionId :: Text
    , targetPilotName :: Text
    , targetCoalition :: Integer
    , targetObjectId :: Integer
    , targetUnitType :: Text
    , targetWsType1 :: Integer
    -- ^ No idea what this is
    , weapon :: Maybe Text
    , weaponType :: Maybe Integer
    }
    deriving stock (Show, Eq)


data KillData = KillData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Text
    , targetMissionId :: Text
    , targetPilotName :: Text
    , targetCoalition :: Integer
    , targetObjectId :: Integer
    , targetUnitType :: Text
    , targetWsType1 :: Integer
    -- ^ No idea what this is
    , weapon :: Text
    , weaponType :: Maybe Integer
    }
    deriving stock (Show, Eq)


data ScoreData = ScoreData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , amount :: Integer
    , comment :: Text
    }
    deriving stock (Show, Eq)


data BdaData = BdaData
    { initiatorMissionID :: Maybe Text
    , initiatorPilotName :: Maybe Text
    , initiatorCoalition :: Maybe Integer
    , initiatorObjectId :: Maybe Integer
    , initiatorUnitType :: Maybe Text
    , initiatorWsType1 :: Maybe Integer
    , target :: Text
    , targetMissionId :: Text
    , targetPilotName :: Text
    , targetCoalition :: Integer
    , targetObjectId :: Integer
    , targetUnitType :: Text
    , targetWsType1 :: Integer
    , weapon :: Maybe Text
    , weaponType :: Maybe Integer
    }
    deriving stock (Show, Eq)


data WeaponDropData = WeaponDropData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    }
    deriving stock (Show, Eq)


data AiAbortMissionData = AiAbortMissionData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)


data PilotDeadData = PilotDeadData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)

data EjectData = EjectData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Text
    , targetMissionId :: Text
    , targetPilotName :: Text
    , targetCoalition :: Integer
    , targetObjectId :: Integer
    , targetUnitType :: Text
    , targetWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)


data PilotDiscardChairData = PilotDiscardChairData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , target :: Text
    , targetMissionId :: Text
    , targetPilotName :: Text
    , targetCoalition :: Integer
    , targetObjectId :: Integer
    , targetUnitType :: Text
    , targetWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)

data PilotLandingData = PilotLandingData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    }
    deriving stock (Show, Eq)

data FailureData = FailureData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    , failure :: Text
    , failureDisplayName :: Text
    }
    deriving stock (Show, Eq)

data CrashData = CrashData
    { initiatorMissionID :: Text
    , initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , initiatorWsType1 :: Integer
    -- ^ No idea what this is
    }
    deriving stock (Show, Eq)

data RelinquishedData = RelinquishedData
    { initiatorPilotName :: Text
    , initiatorCoalition :: Integer
    , initiatorObjectId :: Integer
    , initiatorUnitType :: Text
    , target :: Text
    , targetMissionId :: Text
    }
    deriving stock (Show, Eq)

data MissionEndData = MissionEndData
    { comment :: Text
    }
    deriving stock (Show, Eq)
