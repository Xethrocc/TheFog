module GameTypes where

import Data.Array
import Data.List
import Data.Maybe

data Direction = N|O|S|W|SE deriving (Eq,Show,Enum,Ord)
type LocString = String

data Location = Location {
    locId :: Int,
    locName :: LocString,
    locExits :: [(Int, Direction)]
} deriving (Eq, Show)

type Map = [Location]

type PossibleActions = [String]

type ObjectID = Int
type ObjString = String
type ObjDescri = String

data Object = Object {
    objPos :: Int,
    objId :: ObjectID,
    objName :: ObjString,
    objDescription :: ObjDescri,
    objActions :: PossibleActions
} deriving (Eq, Show)

type ObjectList = [Object]

type Inventory  = ObjectList

data Character = Character {
    charName   :: String,
    charSteps  :: Steps,
    charAttack :: Ang,
    charDefense :: Def,
    charLife   :: Life
} deriving (Eq, Show)

type Steps = Int
type Ang   = Int
type Def   = Int
type Life  = Int

-- Princess status tracking
data PrincessStatus = PrincessAlive | PrincessDead | PrincessSaved deriving (Eq,Show)

-- Wolf data tracking
data Wolf = Wolf {
    wolfId :: Int,
    wolfLoc :: Int,
    wolfGuardian :: Bool  -- True if this wolf is the shrine guardian
} deriving (Eq,Show)

data Game = Game {
    gameLocation   :: Location,
    gameCharacter  :: Character,
    gameInventory  :: Inventory,
    gameObjects    :: ObjectList,
    gameStepCounter :: Int,        -- Deterministic random seed
    gameWolves     :: [Wolf],      -- All wolves with current positions
    gameShrineFlags :: (Bool, Bool, Bool, Bool), -- Earth, Water, Fire, Air
    gamePrincess   :: PrincessStatus
} deriving (Eq, Show)
