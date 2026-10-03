module GameTypes where

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
    charLife   :: Life,
    charMaxLife :: Life
} deriving (Eq, Show)

type Steps = Int
type Ang   = Int
type Def   = Int
type Life  = Int

data PrincessStatus = PrincessAlive | PrincessDead | PrincessSaved deriving (Eq,Show)

data Wolf = Wolf {
    wolfId :: Int,
    wolfLoc :: Int,
    wolfPath :: [Int],
    wolfPathIndex :: Int,
    wolfGuardian :: Bool,
    wolfHp :: Int,
    wolfAttack :: Int,
    wolfDefense :: Int,
    wolfDead :: Bool
} deriving (Eq,Show)

-- NOTE (A6): gameStepCounter entfernt. Es gab zwei tote Step-Counter -
-- charSteps (echt, wird angezeigt) und gameStepCounter (wurde nur an
-- spawnPostFogWolves gereicht, dessen step-Parameter nie benutzt wurde).
-- Neu: advanceTurn in GameActions ist der einzige Zugzaehler.
data Game = Game {
    gameLocation    :: Location,
    gameCharacter   :: Character,
    gameInventory   :: Inventory,
    gameObjects     :: ObjectList,
    gameWolves      :: [Wolf],
    gameShrineFlags :: (Bool, Bool, Bool, Bool),
    gamePrincess    :: PrincessStatus
} deriving (Eq, Show)
