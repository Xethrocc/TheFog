module GameActions where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Array
import GameTypes
import GameData
import GameUtils

-- Shrine Flag Helpers
updateShrineFlags :: Object -> (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool)
updateShrineFlags obj flags =
    case objId obj of
        5 -> setFlag 1 True flags -- Water
        6 -> setFlag 2 True flags -- Fire
        7 -> setFlag 3 True flags -- Air
        8 -> setFlag 0 True flags -- Earth
        _ -> flags

setFlag :: Int -> Bool -> (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool)
setFlag 0 v (e, w, f, a) = (v, w, f, a)
setFlag 1 v (e, w, f, a) = (e, v, f, a)
setFlag 2 v (e, w, f, a) = (e, w, v, a)
setFlag 3 v (e, w, f, a) = (e, w, f, v)
setFlag _ _ f = f

-------- Aktionen ---------

-- Nichts machen --
doNothin :: Object -> ObjectList -> ObjectList
doNothin x ys = ys

doNothinSimple :: Object -> IO()
doNothinSimple x = putStrLn ("")

doNothinExt :: Object -> ObjectList -> Inventory -> (ObjectList,Inventory)
doNothinExt x y i = (y,i)

-- Objekt untersuchen
examineObj :: Object -> IO()
examineObj Object { objId = d} = putStrLn $ objectText ! d

-- Objekt lesen                        
readObj :: Object -> IO()
readObj Object { objId = b } = putStrLn $ objectText ! b

-- Objekt anbrennen/verbrennen
burnObj :: Object -> ObjectList -> Int -> ObjectList
burnObj obj objList randVal = newObj : delete obj objList
                          where
                           newObj = if (randVal > 5) then obj { objName = "burned-" ++ objName obj, objDescription = "It's the burned down " ++ objName obj, objActions = ["examine"] } else obj

-- Wolf movement function
-- Wolf movement function
moveWolves :: Int -> [Wolf] -> [Wolf]
moveWolves stepCount wolves = map moveWolf wolves
  where
    moveWolf w | wolfGuardian w = w  -- Guardian stays put
               | otherwise = moveWolfOnPath w
    moveWolfOnPath w = w { wolfLoc = getNextLoc (wolfLoc w) }
    getNextLoc current = 
        let pathList = findPath current
            currentIndex = findPathIndex current
            nextIndex = (currentIndex + 1) `mod` length pathList
        in pathList !! nextIndex
    findPath loc = head [ p | p <- wolfForestPaths, loc `elem` p ]
    findPathIndex loc = 
        case findIndex (\p -> loc `elem` p) wolfForestPaths of
            Just i -> i
            Nothing -> 0  -- Should not happen

-- Check if player encounters a wolf
checkEncounter :: Location -> [Wolf] -> Bool
checkEncounter loc = any wolfFound
  where
    wolfFound w = wolfLoc w == locId loc
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
                   
-- Objekt angreifen
attackObj :: Object -> ObjectList -> ObjectList
attackObj obj objList = newObj : delete obj objList
                          where
                           newObj = obj { objName = "dead-" ++ objName obj, objDescription = "It's the dead " ++ objName obj, objActions = ["examine"] }
                           
-- Prinzessin retten (deterministisch based on step counter mod logic)
savePrincess :: Object -> ObjectList -> Int -> ObjectList
savePrincess obj objList stepCounter = newObj : delete obj objList
                          where
                           result = stepCounter `mod` 3
                           newObj = if result == 0 then obj { objName = "dead-" ++ objName obj, objDescription = "It's the dead " ++ objName obj, objActions = ["examine"] }
                                  else if result == 1 then obj { objName = "saved-" ++ objName obj, objDescription = "It's the saved " ++ objName obj, objActions = ["examine"] }
                                   else obj

-- Das Inventar Anzeigen
showInventory :: Inventory -> IO()
showInventory []     = putStrLn ("Your Inventory is empty")
showInventory inv = putStrLn (show(map getObjStr inv))

-- Nachsehen ob ein Objekt im Inventar ist
isInInv :: Object -> Inventory -> Bool
isInInv obj inv = obj `elem` inv
