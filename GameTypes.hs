module GameTypes where

-- (Plan 3) Der Zufallsgenerator ist Teil des Spielzustands (siehe `Game`).
import System.Random (StdGen)

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
    -- (Plan 1) Getrennt von `wolfHp`: die LP des Wolfs sinken im Kampf,
    -- die Max-LP sind konstant. Ohne dieses Feld lieferte `wolfFullStats`
    -- (hp, hp), wodurch ein angeschlagener Wolf mit einem vollen HP-Balken
    -- und 12/12 LP angezeigt wurde und der Fortschritt unsichtbar war.
    wolfMaxHp :: Int,
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
    gamePrincess    :: PrincessStatus,
    -- (Plan 3) Der Generator IST Zustand. Vorher zog `roll` (main.hs) fuer
    -- jeden Wurf eine frische `newStdGen`: derselbe Befehlsstrom ergab jedes
    -- Mal einen anderen Kampf, und Tests konnten den Kampfverlauf ueberhaupt
    -- nicht pruefen. Mit dem Generator im Spiel zaehlt die Zufallsfolge von
    -- `gameWithSeed` an - gleicher Seed + gleiche Befehle = gleicher Kampf.
    --
    -- `deriving Show` funktioniert, weil random >= 1.3 wieder einen
    -- Show-Instanz fuer StdGen mitbringt. Mit random 1.2 muesste man
    -- `instance Show Game` von Hand schreiben.
    gameStdGen      :: StdGen,
    -- (Punkt 6) Zustand, nicht Ursache: "die Wand wurde gelesen". Der Tod
    -- an der Wand haengt nicht an den LP, sondern an diesem Flag - er muss
    -- auch jemanden ereilen, der noch 30 LP hat. Deshalb steht die Abfrage
    -- in `gameLoop` VOR dem `getHP`-Test und nicht in einem Handler, der
    -- sich selbst beenden kann.
    --
    -- Bewusst kein `Maybe Steps` o.ae.: es gibt nur den einen Fall.
    gameWallRead    :: Bool
} deriving (Eq, Show)
