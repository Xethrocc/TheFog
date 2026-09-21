module GameUtils where

import Data.Array
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import GameTypes
import GameData


-- Helper functions for Location
locS :: Location -> LocString
locS = locName

locD :: Location -> String
locD loc = descr ! locId loc

locI :: Direction -> Location -> Maybe Int
locI targetDir loc = find (\(locId', dir) -> dir == targetDir) (locExits loc) >>= Just . fst

locA :: Location -> Int
locA = locId

getObjStr :: Object -> ObjString
getObjStr = objName

isActionPossible :: String -> Object -> Bool
isActionPossible x obj = x `elem` objActions obj

splitString :: String -> [String]
splitString x = case break (==' ') x of
                (a, ' ':b) -> a : splitString b
                (a, "")    -> [a]

isObjHere :: Object -> Location -> Bool
isObjHere obj loc = objPos obj == locId loc

-- Get objects at location (including wolves)
getObjHere :: ObjectList -> [Wolf] -> Location -> String
getObjHere objList wolves loc =
    objObjects ++ objWolves
  where
    objObjects = getObjHereObjects objList loc
    objWolves = getWolfHere wolves loc

getObjHereObjects :: ObjectList -> Location -> String
getObjHereObjects [] _ = ""
getObjHereObjects (x:xs) loc = obj ++ (getObjHereObjects xs loc)
                              where
                               obj = if (isObjHere x loc) then (objName x) ++ "\n" else ""

getWolfHere :: [Wolf] -> Location -> String
getWolfHere [] _ = ""
getWolfHere (w:ws) loc = obj ++ (getWolfHere ws loc)
                         where
                          obj = if (wolfLoc w == locId loc) && not (wolfDead w) then 
                                   (wolfIdToName (wolfId w)) ++ "\n" else ""

wolfIdToName :: Int -> String
-- (Fix 23) Wölfe unterscheidbar benennen
wolfIdToName 0 = "Wolf (Guardian)"
wolfIdToName n = "Patrolling Wolf #" ++ show n

-- Find an object by a string (also searches wolves)
getObject :: String -> ObjectList -> [Wolf] -> Maybe Object
getObject x objList wolves = 
    case getObjectInList x objList of
        Just obj -> Just obj
        Nothing -> findWolfByName x wolves

getObjectInList :: String -> ObjectList -> Maybe Object
-- (Fix 8) Nur echter Namens-Match; Woelfe werden ausschliesslich ueber
-- findWolfByName (Fallthrough in getObject) gefunden. Vorher matchte
-- "wolf" das ERSTE Objekt der Liste (= Paper).
getObjectInList _ []     = Nothing
getObjectInList x (y:ys) = if x == objName y then Just y else getObjectInList x ys

-- Helper to check if string matches a wolf
wolfMatch :: String -> Wolf -> Bool
wolfMatch x w = x == wolfIdToName (wolfId w) || 
                x == "wolf" || 
                x == "wolves"

findWolfByName :: String -> [Wolf] -> Maybe Object
findWolfByName x wolves = 
    case find (wolfMatch x) wolves of
        Just w -> Just (wolfToObj w)
        Nothing -> Nothing

wolfToObj :: Wolf -> Object
wolfToObj w = Object 
    { objPos = wolfLoc w 
    , objId = wolfId w 
    , objName = wolfIdToName (wolfId w)
    , objDescription = "A " ++ (if wolfGuardian w then "guardian " else "patrolling ") ++ "wolf"
    , objActions = ["examine", "attack", "fight"]
    }

-- Get the String from Character
getString :: Character -> String
getString = charName

getSteps :: Character -> Steps
getSteps = charSteps

getAtk :: Character -> Ang
getAtk = charAttack

getDef :: Character -> Def
getDef = charDefense

getHP :: Character -> Life
getHP = charLife

giveName :: String -> Character
-- (Fix 24) Leere/Whitespace-Namen bekommen den Standardnamen "Hero"
giveName x = Character { charName = if all isSpace x then "Hero" else x
                       , charSteps = 0, charAttack = 2, charDefense = 1, charLife = 10 }

-- (Fix 20) Wird in handleMovement aufgerufen, damit der Steps-Zaehler stimmt
addSteps :: Character -> Character
addSteps char = char { charSteps = charSteps char + 1 }

addAng :: Inventory -> Character -> Character
-- DESIGN (Autor): Der Protagonist ist NICHT der Held der Prophezeiung.
-- Das Schwert leuchtet fuer ihn nur kurz auf und erlischt (interactSword) -
-- es verleiht bewusst KEINEN Angriffsbonus. addAng bleibt wirkungslos.
addAng _ char = char

addDef :: Inventory -> Character -> Character
addDef inv char = if any (\obj -> objId obj == 3) inv then char { charDefense = 10 } else char
