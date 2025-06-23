module GameUtils where

import Data.Array
import Data.List
import Data.Maybe
import System.Console.ANSI
import GameTypes
import GameData

-- Helper functions for Location
locS :: Location -> LocString -- Gets the Location String from the Location-Type
locS (a,location,xs) = location

locD :: Location -> String -- Gets the description of the Location from the descr array
locD (a,location,xs) = descr! a

locI :: Direction -> Location -> Int -- Returns the index of the next Location for a specific cardinal direction, if the direction is not present, one remains at the same Location.
locI x (a,location,[(d,e)]) = if (e == x) then d else a
locI x (a,location,(y:ys)) =  if (dir == x) then loc else locI x (a,location,(ys))
                              where 
                               (loc,dir) = y

-- Returns the Int of the Current Position 
locA :: Location -> Int
locA (a,location,xs) =  a

-- Returns the position of the object
getObjPos :: Object -> Int
getObjPos (pos,b,c,d,es) = pos

-- Returns the Object-ID
getObjID :: Object -> ObjectID
getObjID (a,id,c,d,es) = id

-- Returns the name of the object
getObjStr :: Object -> ObjString
getObjStr (a,b,string,d,es) = string

-- Returns the description of the object
getObjDes :: Object -> ObjDescri
getObjDes (a,b,c,description,es) = description

-- Returns the possible actions
getObjActions :: Object -> PossibleActions
getObjActions (a,b,c,d,actions) = actions

-- Says whether an action is applicable to this object
isActionPossible :: String -> Object -> Bool
isActionPossible x (a,b,c,d,actions) 
                        | x `elem` actions = True
                        | otherwise        = False

-- Small function to split string inputs --
splitString x = case break (==' ') x of
                (a, ' ':b) -> a : splitString b
                (a, "")    -> [a]

-- Checks if an object is at a location
isObjHere :: Object -> Location -> Bool
isObjHere (a,b,c,d,e) (f,location,xs) = if (a==f) then True else False

getObjHere :: ObjectList -> Location -> String
getObjHere [] xs = ""
getObjHere (x:xs) loc = obj ++ (getObjHere xs loc)
                              where
                               obj = if (isObjHere x loc) then (getObjStr x) ++ "\n" else ""


-- Find an object by a string
getObject :: String -> ObjectList -> Object
getObject x []     = (90,90,"","",[])
getObject x (y:ys) = if (x == string) then y else getObject x ys
                       where string = getObjStr y
                       

-- Get the String from Character 
getString :: Character -> String
getString (a,b,c,d,l) = a
-- Get the Steps
getSteps ::  Character -> Steps
getSteps (a,b,c,d,l)  = b
-- Get the Attack value
getAtk ::    Character -> Ang
getAtk (a,b,c,d,l)    = c
-- Get the Defense value
getDef ::    Character -> Def
getDef (a,b,c,d,l)    = d
-- Get the Health points
getHP ::    Character -> Life
getHP (a,b,c,d,l)    = l
-- Write the name into an empty Character
giveName :: String -> Character
giveName x = (x,0,1,1,10)
-- Addstep
addSteps :: Character -> Character
addSteps (a,b,c,d,l) = (a,b+1,c,d,l)
-- AddAngr and defense
addAng :: Inventory -> Character -> Character
addAng inv (a,b,c,d,l) = if "Sword" `elem`(map getObjStr inv) then (a,b,10,d,l) else (a,b,c,d,l)

addDef :: Inventory -> Character -> Character
addDef inv (a,b,c,d,l) = if "Shield" `elem`(map getObjStr inv) then (a,b,c,10,l) else (a,b,c,d,l)

-- Helper function so that "Press 'Enter' to continue" is not displayed for direction inputs                        
ifThereIsAnyDirection :: String -> [String] -> IO()
ifThereIsAnyDirection x y = if (x `elem` y) 
                            then do putStrLn("") 
                            else do putStrLn("Press 'Enter' to continue.")
                                    enter <- getLine
                                    putStrLn("")

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
                
-- PseudoRandom-functions
randomInts = [8, 9, 1, 6, 10, 4, 4, 8, 10, 5, 10, 10, 2, 7, 8, 7, 4, 5, 8, 7, 8, 2, 7, 5, 4, 1, 3, 7, 5, 5, 3, 9, 10, 2, 7, 5, 3, 6, 4, 10, 9, 8, 8, 3, 9, 5, 1, 4, 3, 4, 1, 9, 2, 5, 4, 5, 7, 2, 5, 9, 3, 10, 1, 5, 1, 10, 7, 10, 3, 5, 5, 6, 4, 7, 9, 2, 7, 9, 1, 4, 6, 5, 4, 1, 8, 3, 10, 1, 2, 1, 7, 6, 1, 1, 5, 1, 7, 10, 9, 8, 7, 7, 1, 2, 3, 9, 5, 3, 3, 8, 7, 2, 6, 4, 1, 1, 7, 4, 3, 3, 3]
randomInts2 = [29, 12, 51, 42, 51, 15, 45, 28, 32, 12, 4, 25, 8, 30, 28, 40, 16, 27, 31, 32, 4, 38, 5, 44, 7, 32, 23, 45, 48, 32, 34, 36, 45, 52, 19, 33, 52, 16, 49, 24, 48, 42, 41, 29, 46, 4, 6, 39, 35, 43, 23, 34, 18, 42, 35, 46, 41, 17, 50, 52, 11, 15, 32, 9, 20, 34, 30, 49, 37, 40, 35, 14, 43, 9, 43, 47, 40, 46, 15, 15, 22, 37, 7, 21, 42, 28, 47, 16, 14, 21]

-- Simple Random Number from 1-10
random :: Int -> Int
random x 
      | x <= 120 = randomInts !! x
      | x > 120  = random $ x-120

-- Creates a new object at a random position      
randomObj :: Int -> ObjectList -> Object -> ObjectList
randomObj x oLs (pos,b,c,d,es) 
                   | x <= 120 = ((randomInts2 !! x),b,c,d,es):oLs
                   | x  > 120 = randomObj (x-90) oLs (pos,b,c,d,es)
                   
-- Output for actions that access random
randText :: Int -> String -> IO()
randText int y = if ((random int) > 5) then putStrLn ("You "++ y ++"ed it") else putStrLn ("That was not working.. try again")