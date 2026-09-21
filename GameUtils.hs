module GameUtils where

import Data.Array
import Data.List
import Data.Maybe
import GameTypes
import GameData
import System.Random (StdGen, newStdGen, randomR)


-- Helper functions for Location
locS :: Location -> LocString
locS = locName

locD :: Location -> String
locD loc = descr ! locId loc

locI :: Direction -> Location -> Maybe Int
locI targetDir loc = find (\(locId', dir) -> dir == targetDir) (locExits loc) >>= Just . fst

locA :: Location -> Int
locA = locId

getObjPos :: Object -> Int
getObjPos = objPos

getObjID :: Object -> ObjectID
getObjID = objId

getObjStr :: Object -> ObjString
getObjStr = objName

getObjDes :: Object -> ObjDescri
getObjDes = objDescription

getObjActions :: Object -> PossibleActions
getObjActions = objActions

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
wolfIdToName 0 = "Wolf (Guardian)"
wolfIdToName _ = "Patrolling Wolf"

-- Find an object by a string (also searches wolves)
getObject :: String -> ObjectList -> [Wolf] -> Maybe Object
getObject x objList wolves = 
    case getObjectInList x objList of
        Just obj -> Just obj
        Nothing -> findWolfByName x wolves

getObjectInList :: String -> ObjectList -> Maybe Object
getObjectInList _ []     = Nothing
getObjectInList x (y:ys) = if (x == objName y || x `elem` ["wolf", "wolves"]) 
                           then Just y 
                           else getObjectInList x ys

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
giveName x = Character { charName = x, charSteps = 0, charAttack = 1, charDefense = 1, charLife = 10 }

addSteps :: Character -> Character
addSteps char = char { charSteps = charSteps char + 1 }

addAng :: Inventory -> Character -> Character
addAng inv char = if any (\obj -> objId obj == 9) inv then char { charAttack = 10 } else char

addDef :: Inventory -> Character -> Character
addDef inv char = if any (\obj -> objId obj == 3) inv then char { charDefense = 10 } else char

shouldWaitForEnter :: String -> [String] -> Bool
shouldWaitForEnter input allowedDirections = not (input `elem` allowedDirections)

selsort :: (Eq a,Ord a) => [a] -> [a]
selsort [] = []
selsort xs = let 
               min  = minimum xs
               rest = delete min xs
             in
               min : selsort rest  

qsort :: (Eq a,Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = let
                min  = qsort [m | m <- (x:xs), m <  x]
                same =       [m | m <- (x:xs), m == x]
                max  = qsort [m | m <- (x:xs), m >  x] 
               in
                min ++ same ++ max   
                
random :: StdGen -> (Int, StdGen)
random gen = randomR (1, 10) gen

randomObj :: StdGen -> ObjectList -> Object -> (ObjectList, StdGen)
randomObj gen oLs obj = (obj { objPos = newPos } : oLs, newGen)
  where
    (newPos, newGen) = randomR (0, 54) gen

randText :: Int -> String -> IO()
randText int y = if (int > 5) then putStrLn ("You "++ y ++"ed it") else putStrLn ("That was not working.. try again")
