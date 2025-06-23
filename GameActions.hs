module GameActions where

import Data.List
import Data.Maybe
import GameTypes
import GameData
import GameUtils (random)

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
examineObj (a,b,c,d,actions) = putStrLn $ d

-- Objekt lesen                        
readObj :: Object -> IO()
readObj (a,b,c,d,actions) = putStrLn $ objectText!b

-- Objekt anbrennen/verbrennen
burnObj :: Object -> ObjectList -> Int -> ObjectList
burnObj (a,b,c,d,actions) xs int = obj:lst
                          where
                           obj = if ((random int) > 5) then (a,b,"burned-"++c,"It's the burned down "++c,["examine"]) else (a,b,c,d,actions)
                           lst = delete (a,b,c,d,actions) xs

-- Ojekt aktivieren                           
activateObj :: Object -> ObjectList -> Inventory -> ObjectList
activateObj (a,b,c,d,actions) xs ys = if (isJust (isInInv (objectList !! 4) ys)) then obj:lst else xs
                              where 
                               obj = (a,b,"activated-"++c,"It's The activated "++c,["examine"])
                               lst = delete (a,b,c,d,actions) xs

-- Objekt nehmen -> ins Inventar nehmen und aus der Objektliste löschen                           
takeObj :: Object -> ObjectList -> Inventory -> (ObjectList,Inventory)
takeObj (a,b,c,d,actions) oL inv = (oL2,inv2)
                  where
                   oL2 = delete (a,b,c,d,actions) oL
                   inv2 | c=="Apple" = (a,b,c,d,["examine","consume","eat"]) : inv
                        | otherwise  = (a,b,c,d,["examine"]) : inv
                   
-- Objekt angreifen
attackObj :: Object -> ObjectList -> ObjectList
attackObj (a,b,c,d,actions) xs = obj:lst
                          where
                           obj = (a,b,"dead-"++c,"It's the dead "++c,["examine"])
                           lst = delete (a,b,c,d,actions) xs  
                           
-- Prinzessin retten (or not)
savePrincess :: Object -> ObjectList -> Steps -> ObjectList
savePrincess (a,b,c,d,actions) xs int = obj:lst
                          where
                           obj = if ((random int) > 5) then (a,b,"saved-"++c,"It's the saved "++c,["examine"]) 
                                 else if ((random int) < 3) then (a,b,"dead-"++c,"It's the dead "++c,["examine"]) 
                                  else (a,b,c,d,actions)
                           lst = delete (a,b,c,d,actions) xs  
-- Das Inventar Anzeigen

showInventory :: Inventory -> IO()
showInventory []     = putStrLn ("Your Inventory is empty")
showInventory inv = putStrLn (show(map getObjStr inv))

-- Nachsehen ob ein Objekt im Inventar ist
isInInv :: Object -> Inventory -> Maybe Bool
isInInv x [] = Nothing
isInInv x (y:ys) 
               | x==y      = Just True
               | otherwise = isInInv x ys