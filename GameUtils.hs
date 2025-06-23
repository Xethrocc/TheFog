module GameUtils where

import Data.Array
import Data.List
import Data.Maybe
import GameTypes
import GameData
import System.Random (StdGen, newStdGen, randomR)


-- Helper functions for Location
locS :: Location -> LocString -- Gets the Location String from the Location-Type
locS = locName

locD :: Location -> String -- Gets the description of the Location from the descr array
locD loc = descr ! locId loc

locI :: Direction -> Location -> Maybe Int -- Returns the index of the next Location for a specific cardinal direction, if the direction is not present, one remains at the same Location.
locI targetDir loc = find (\(locId', dir) -> dir == targetDir) (locExits loc) >>= Just . fst

-- Returns the Int of the Current Position
locA :: Location -> Int
locA = locId

-- Returns the position of the object
getObjPos :: Object -> Int
getObjPos = objPos

-- Returns the Object-ID
getObjID :: Object -> ObjectID
getObjID = objId

-- Returns the name of the object
getObjStr :: Object -> ObjString
getObjStr = objName

-- Returns the description of the object
getObjDes :: Object -> ObjDescri
getObjDes = objDescription

-- Returns the possible actions
getObjActions :: Object -> PossibleActions
getObjActions = objActions

-- Says whether an action is applicable to this object
isActionPossible :: String -> Object -> Bool
isActionPossible x obj = x `elem` objActions obj

-- Small function to split string inputs --
splitString :: String -> [String]
splitString x = case break (==' ') x of
                (a, ' ':b) -> a : splitString b
                (a, "")    -> [a]

-- Checks if an object is at a location
isObjHere :: Object -> Location -> Bool
isObjHere obj loc = objPos obj == locId loc

getObjHere :: ObjectList -> Location -> String
getObjHere [] _ = ""
getObjHere (x:xs) loc = obj ++ (getObjHere xs loc)
                              where
                               obj = if (isObjHere x loc) then (objName x) ++ "\n" else ""


-- Find an object by a string
getObject :: String -> ObjectList -> Maybe Object
getObject _ []     = Nothing
getObject x (y:ys) = if (x == objName y) then Just y else getObject x ys


-- Get the String from Character
getString :: Character -> String
getString = charName
-- Get the Steps
getSteps ::  Character -> Steps
getSteps = charSteps
-- Get the Attack value
getAtk ::    Character -> Ang
getAtk = charAttack
-- Get the Defense value
getDef ::    Character -> Def
getDef = charDefense
-- Get the Health points
getHP ::    Character -> Life
getHP = charLife
-- Write the name into an empty Character
giveName :: String -> Character
giveName x = Character { charName = x, charSteps = 0, charAttack = 1, charDefense = 1, charLife = 10 }
-- Addstep
addSteps :: Character -> Character
addSteps char = char { charSteps = charSteps char + 1 }
-- AddAngr and defense
addAng :: Inventory -> Character -> Character
addAng inv char = if any (\obj -> objId obj == 9) inv then char { charAttack = 10 } else char

addDef :: Inventory -> Character -> Character
addDef inv char = if any (\obj -> objId obj == 3) inv then char { charDefense = 10 } else char

-- Helper function to determine if "Press 'Enter' to continue" should be displayed
shouldWaitForEnter :: String -> [String] -> Bool
shouldWaitForEnter input allowedDirections = not (input `elem` allowedDirections)

-- Sorting functions
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
                

-- Simple Random Number from 1-10
random :: StdGen -> (Int, StdGen)
random gen = randomR (1, 10) gen

-- Creates a new object at a random position
randomObj :: StdGen -> ObjectList -> Object -> (ObjectList, StdGen)
randomObj gen oLs obj = (obj { objPos = newPos } : oLs, newGen)
  where
    (newPos, newGen) = randomR (0, 54) gen -- Assuming 0-54 are valid location IDs

-- Output for actions that access random
randText :: Int -> String -> IO()
randText int y = if (int > 5) then putStrLn ("You "++ y ++"ed it") else putStrLn ("That was not working.. try again")