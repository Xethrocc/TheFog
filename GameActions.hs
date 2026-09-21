module GameActions where

import Data.List
import Data.Maybe
import Data.Array
import GameTypes
import GameData

-- Wolf movement function with path index tracking
moveWolves :: Int -> [Wolf] -> [Wolf]
moveWolves _ wolves = map moveWolf wolves
  where
    moveWolf w | wolfGuardian w || wolfDead w = w
               | null (wolfPath w) = w
               | otherwise = w { wolfPathIndex = newIndex, wolfLoc = nextLoc }
              where
                path = wolfPath w
                currentIdx = wolfPathIndex w
                newIndex = (currentIdx + 1) `mod` length path
                nextLoc = path !! newIndex

-- Check if player encounters a live wolf
checkEncounter :: Location -> [Wolf] -> Bool
checkEncounter loc = any (wolfFound loc)
  where
    wolfFound loc w = wolfLoc w == locId loc && not (wolfDead w)

-- Check ending condition
checkEnding :: (Bool, Bool, Bool, Bool) -> Bool
checkEnding (e, w, f, a) = e && w && f && a

-- Get wolf stats (affected by shrine activation)
getWolfStats :: (Bool, Bool, Bool, Bool) -> (Int, Int, Int)
getWolfStats flags = 
    let multiplier = if checkEnding flags then 3 else 1
        attackMult = if checkEnding flags then 2 else 1
    in (30 * multiplier, 8 * attackMult, 2 * attackMult)

-- Add new wolves after fog descends
spawnPostFogWolves :: [Wolf] -> (Bool, Bool, Bool, Bool) -> Int -> [Wolf]
spawnPostFogWolves wolves flags step =
    if checkEnding flags
        then let baseId = 100
                 locations = postFogWolfLocations
                 baseStats = getWolfStats flags
                 newWolf loc id = Wolf id loc [] 0 False 
                                            (fst3 baseStats) (snd3 baseStats) (thd3 baseStats) False
             in wolves ++ zipWith newWolf locations [baseId..baseId + length locations - 1]
        else wolves

-- Helper for triple tuples
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- Objekt untersuchen
examineObj :: Object -> IO()
examineObj Object { objId = d} = putStrLn $ objectText ! d

-- Objekt lesen                        
readObj :: Object -> IO()
readObj Object { objId = b } = putStrLn $ objectText ! b

-- Objekt angreifen
attackObj :: Object -> ObjectList -> ObjectList
attackObj obj objList = newObj : delete obj objList
                          where
                           newObj = obj { objName = "dead-" ++ objName obj, objDescription = "It's the dead " ++ objName obj, objActions = ["examine"] }

-- Nichts machen --
doNothinSimple :: Object -> IO()
doNothinSimple x = putStrLn ("")

-- Shrine Flag Helpers
updateShrineFlags :: Object -> (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool)
updateShrineFlags obj flags =
    case objId obj of
        5 -> setFlag 1 True flags
        6 -> setFlag 2 True flags
        7 -> setFlag 3 True flags
        8 -> setFlag 0 True flags
        _ -> flags

setFlag :: Int -> Bool -> (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool)
setFlag 0 v (e, w, f, a) = (v, w, f, a)
setFlag 1 v (e, w, f, a) = (e, v, f, a)
setFlag 2 v (e, w, f, a) = (e, w, v, a)
setFlag 3 v (e, w, f, a) = (e, w, f, v)
setFlag _ _ f = f

-- Objekt aktivieren                           
activateObj :: Object -> ObjectList -> Inventory -> ObjectList
activateObj obj objList inv = if isJust (find (\o -> objId o == 4) inv) then newObj : delete obj objList else objList
                               where
                                newObj = obj { objName = "activated-" ++ objName obj, objDescription = "It's The activated " ++ objName obj, objActions = ["examine"] }

-- Objekt nehmen -> ins Inventar nehmen und aus der Objektliste löschen                           
takeObj :: Object -> ObjectList -> Inventory -> (ObjectList,Inventory)
takeObj obj objList inv = (oL2,inv2)
                  where
                   oL2 = delete obj objList
                   inv2 | objId obj == 2 = obj { objActions = ["examine","consume","eat"] } : inv
                        | otherwise  = obj { objActions = ["examine"] } : inv
                   
-- Das Inventar Anzeigen
showInventory :: Inventory -> IO()
showInventory []     = putStrLn ("Your Inventory is empty")
showInventory inv = putStrLn (show(map (objName) inv))

-- Nachsehen ob ein Objekt im Inventar ist
isInInv :: Object -> Inventory -> Bool
isInInv obj inv = obj `elem` inv

interactSword :: Object -> IO ()
interactSword _ = putStrLn "Die Klinge des Schwertes leuchtet kurz auf, erlischt dann aber wieder."
