module GameTypes where

import Data.Array
import Data.List
import Data.Maybe

data Direction = N|O|S|W|SE deriving (Eq,Show,Enum,Ord) -- Datentyp für Mögliche Richtungen N O S W und SE für spezielle Richtungen
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

data Game = Game {
    gameLocation  :: Location,
    gameCharacter :: Character,
    gameInventory :: Inventory,
    gameObjects   :: ObjectList
} deriving (Eq, Show)