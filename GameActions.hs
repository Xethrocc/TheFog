module GameActions where

import Data.List
import Data.Maybe
import Data.Array
import Data.Ix (inRange)
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
-- (Fix 9) Ausgewogenere Post-Fog-Werte (vorher 90 HP / x3 Multiplikator)
getWolfStats :: (Bool, Bool, Bool, Bool) -> (Int, Int, Int)
getWolfStats _ = (30, 16, 4)

-- (Fix 9) Kampf-Stats eines Wolfs nach wolfId
-- (Fallback (8,2) fuer das statische Wolf-Objekt ohne Wolf-Eintrag)
wolfCombatStats :: [Wolf] -> ObjectID -> (Int, Int)
wolfCombatStats wolves wid = case find (\w -> wolfId w == wid) wolves of
    Just w  -> (wolfAttack w, wolfDefense w)
    Nothing -> (8, 2)

-- Add new wolves after fog descends
-- (Fix 14) Guard: nur spawnen, wenn noch keine Post-Fog-Woelfe existieren
-- (vorher verdoppelten sich die Woelfe mit jedem Schritt)
spawnPostFogWolves :: [Wolf] -> (Bool, Bool, Bool, Bool) -> Int -> [Wolf]
spawnPostFogWolves wolves flags step =
    if checkEnding flags && not (any (\w -> wolfId w >= 100) wolves)
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

-- (Fix 6) Defensiver Text-Zugriff: falls die objId nicht in objectText
-- liegt, wird objDescription als Fallback ausgegeben statt zu crashen.
objectTextFor :: Object -> String
objectTextFor obj@Object { objId = i, objDescription = d }
    | inRange (bounds objectText) i = objectText ! i
    | otherwise = d

-- Objekt untersuchen
examineObj :: Object -> IO()
examineObj obj = putStrLn (objectTextFor obj)

-- Objekt lesen                        
readObj :: Object -> IO()
readObj obj = putStrLn (objectTextFor obj)

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
                                -- (Fix 6) ID wird auf den passenden "activated"-Eintrag
                                -- angehoben (5->14, 6->15, 7->16, 8->17), damit der
                                -- korrekte examine-Text angezeigt wird
                                newObj = obj { objId = objId obj + 9, objName = "activated-" ++ objName obj, objDescription = "It's The activated " ++ objName obj, objActions = ["examine"] }

-- Objekt nehmen -> ins Inventar nehmen und aus der Objektliste löschen                           
takeObj :: Object -> ObjectList -> Inventory -> (ObjectList,Inventory)
takeObj obj objList inv = (oL2,inv2)
                  where
                   oL2 = delete obj objList
                   inv2 | objId obj == 2 = obj { objActions = ["examine","consume","eat"] } : inv
                        | otherwise  = obj { objActions = ["examine"] } : inv
                   
-- Das Inventar Anzeigen
-- (Fix 18) Schoene Auflistung statt Haskell-Listensyntax
showInventory :: Inventory -> IO()
showInventory []     = putStrLn ("Your Inventory is empty")
showInventory inv = do
    putStrLn "Your Inventory:"
    mapM_ (\o -> putStrLn ("  - " ++ objName o)) inv

interactSword :: Object -> IO ()
-- (Fix 19) Englischer Text statt deutscher Zeile
interactSword _ = putStrLn "The blade of the Sword glows briefly, then fades again."
