module GameActions where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Array
import GameTypes
import GameData
import GameUtils

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

-- Ojekt aktivieren                           
activateObj :: Object -> ObjectList -> Inventory -> ObjectList
activateObj obj objList inv = if isJust (find (\o -> objId o == 4) inv) then newObj : delete obj objList else objList
                               where
                                newObj = obj { objName = "activated-" ++ objName obj, objDescription = "It's The activated " ++ objName obj, objActions = ["examine"] }

-- Objekt nehmen -> ins Inventar nehmen und aus der Objektliste löschen                           
takeObj :: Object -> ObjectList -> Inventory -> (ObjectList,Inventory)
takeObj obj objList inv = (oL2,inv2)
                  where
                   oL2 = delete obj objList
                   inv2 | objId obj == 2 = obj { objActions = ["examine","consume","eat"] } : inv -- Apple has objId 2
                        | otherwise  = obj { objActions = ["examine"] } : inv
                   
-- Objekt angreifen
attackObj :: Object -> ObjectList -> ObjectList
attackObj obj objList = newObj : delete obj objList
                          where
                           newObj = obj { objName = "dead-" ++ objName obj, objDescription = "It's the dead " ++ objName obj, objActions = ["examine"] }
                           
-- Prinzessin retten (or not)
savePrincess :: Object -> ObjectList -> Int -> ObjectList
savePrincess obj objList randVal = newObj : delete obj objList
                          where
                           newObj = if (randVal > 5) then obj { objName = "saved-" ++ objName obj, objDescription = "It's the saved " ++ objName obj, objActions = ["examine"] }
                                  else if (randVal < 3) then obj { objName = "dead-" ++ objName obj, objDescription = "It's the dead " ++ objName obj, objActions = ["examine"] }
                                   else obj
-- Das Inventar Anzeigen

showInventory :: Inventory -> IO()
showInventory []     = putStrLn ("Your Inventory is empty")
showInventory inv = putStrLn (show(map getObjStr inv))

-- Nachsehen ob ein Objekt im Inventar ist
isInInv :: Object -> Inventory -> Bool
isInInv obj inv = obj `elem` inv