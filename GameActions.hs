module GameActions where

import Data.List
import Data.Array (bounds, (!))
import Data.Ix (inRange)
import GameTypes
import GameData
import GameConstants
import GameUtils

-- ---------------------------------------------------------------------------
-- Zug-Ökonomie (A4/B5)
-- ---------------------------------------------------------------------------
-- EIN Zug = Bewegung, Kampf-Runde oder `flee`. `examine`/`read`/`take`/
-- `activate` kosten nichts.
--
-- `advanceTurn` ist der einzige Zugzähler im Spiel: es zählt die Schritte,
-- bewegt die Wölfe und - falls der Nebel steht - zieht Lebenspunkte ab.
-- Reihenfolge deterministisch: erst Wölfe, dann Nebel.
--
-- Ohne das war der Kampf zeitlich gratis (`fightLoop` fasste
-- `gameStepCounter` nie an), wodurch es keinen Zeitdruck gab.
advanceTurn :: Game -> Game
advanceTurn game = applyFogDamage stepped
  where
    moved   = game { gameWolves = moveWolves (gameWolves game) }
    stepped = moved { gameCharacter = addSteps (gameCharacter moved) }

    applyFogDamage g
        | fogActive g = let c = gameCharacter g
                        in g { gameCharacter = c { charLife = max 0 (charLife c - fogDamage) } }
        | otherwise   = g

-- Wolf movement function with path index tracking
-- Ein leerer `wolfPath` bedeutet: der Wolf steht still (Guardian,
-- Nebelwölfe, die statischen Wächter in Schloss und Katakomben).
moveWolves :: [Wolf] -> [Wolf]
moveWolves = map moveWolf
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

-- (B1) Die Nebel-Phase wird abgeleitet, nicht gespeichert. Eine einzige
-- Quelle der Wahrheit: sind alle vier Schreine aktiv, steht der Nebel.
fogActive :: Game -> Bool
fogActive = checkEnding . gameShrineFlags

-- Get wolf stats (affected by shrine activation)
getWolfStats :: (Bool, Bool, Bool, Bool) -> (Int, Int, Int)
getWolfStats _ = (30, 16, 4)

-- Kampf-Stats eines Wolfs nach wolfId. Beide Komponenten werden benutzt -
-- vorher wurde `wolfDefense` mit `_` verworfen und war toter Code.
wolfCombatStats :: [Wolf] -> ObjectID -> (Int, Int)
wolfCombatStats wolves wid = case find (\w -> wolfId w == wid) wolves of
    Just w  -> (wolfAttack w, wolfDefense w)
    Nothing -> (8, 2)

-- Vollstaendige Wolf-Stats inkl. maximaler LP, fuer die Kampf-Anzeige.
-- `enemyHP` war fest auf 12 verdrahtet, wodurch der Guardian mit 12/30 HP
-- und die HP-Balken mit hart kodierten 30 angezeigt wurden.
wolfFullStats :: [Wolf] -> ObjectID -> (Life, Life)
wolfFullStats wolves wid = case find (\w -> wolfId w == wid) wolves of
    Just w  -> (wolfHp w, wolfHp w)
    Nothing -> (12, 12)

-- Add new wolves after fog descends
-- (Fix 14) Guard: nur spawnen, wenn noch keine Post-Fog-Woelfe existieren
-- (vorher verdoppelten sich die Woelfe mit jedem Schritt).
-- (A6) Der `step`-Parameter ist entfernt - er wurde nie benutzt.
spawnPostFogWolves :: [Wolf] -> (Bool, Bool, Bool, Bool) -> [Wolf]
spawnPostFogWolves wolves flags =
    if checkEnding flags && not (any (\w -> wolfId w >= postFogWolfIdBase) wolves)
        then let locations = postFogWolfLocations
                 baseStats = getWolfStats flags
                 newWolf loc id = Wolf id loc [] 0 False
                                            (fst3 baseStats) (snd3 baseStats) (thd3 baseStats) False
             in wolves ++ zipWith newWolf locations
                                  [postFogWolfIdBase .. postFogWolfIdBase + length locations - 1]
        else wolves

-- Helper for triple tuples
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z
-- ---------------------------------------------------------------------------
-- (A6) Objekte nach `objId` loeschen, nicht nach Strukturgleichheit.
-- `delete` verglich ALLE Felder inklusive `objActions`. Heute harmlos,
-- mit ~15 neuen Objekten und veraenderten Aktionslisten aber real: `take`
-- aendert `objActions`, wodurch ein anschliessendes `delete` ins Leere
-- griff und dasselbe Objekt doppelt im Zustand existierte.
-- ---------------------------------------------------------------------------
deleteByObjId :: ObjectID -> ObjectList -> ObjectList
deleteByObjId i = filter ((/= i) . objId)

-- (Fix 6) Defensiver Text-Zugriff: falls die objId nicht in objectText
-- liegt, wird objDescription als Fallback ausgegeben statt zu crashen.
objectTextFor :: Object -> String
objectTextFor Object { objId = i, objDescription = d }
    | inRange (bounds objectText) i = objectText ! i
    | otherwise = d

-- Objekt untersuchen
examineObj :: Object -> IO()
examineObj obj = putStrLn (objectTextFor obj)

-- Objekt lesen
readObj :: Object -> IO ()
readObj obj = putStrLn (objectTextFor obj)

-- Objekt angreifen
attackObj :: Object -> ObjectList -> ObjectList
attackObj obj objList = newObj : deleteByObjId (objId obj) objList
                          where
                           newObj = obj { objName = "dead-" ++ objName obj
                                        , objDescription = "It's the dead " ++ objName obj
                                        , objActions = ["examine"] }

-- Nichts machen --
doNothinSimple :: Object -> IO ()
doNothinSimple _ = putStrLn ("")

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
setFlag 0 v (_, w, f, a) = (v, w, f, a)
setFlag 1 v (e, _, f, a) = (e, v, f, a)
setFlag 2 v (e, w, _, a) = (e, w, v, a)
setFlag 3 v (e, w, f, _) = (e, w, f, v)
setFlag _ _ f = f

-- Objekt aktivieren
activateObj :: Object -> ObjectList -> Inventory -> ObjectList
activateObj obj objList inv =
    if any ((== crystalId) . objId) inv
        then newObj : deleteByObjId (objId obj) objList
        else objList
  where
    -- (Fix 6) ID wird auf den passenden "activated"-Eintrag
    -- angehoben (5->14, 6->15, 7->16, 8->17), damit der
    -- korrekte examine-Text angezeigt wird
    newObj = obj { objId = objId obj + 9
                 , objName = "activated-" ++ objName obj
                 , objDescription = "It's The activated " ++ objName obj
                 , objActions = ["examine"] }

-- Objekt nehmen -> ins Inventar nehmen und aus der Objektliste löschen
takeObj :: Object -> ObjectList -> Inventory -> (ObjectList, Inventory)
takeObj obj objList inv = (deleteByObjId (objId obj) objList, inv2)
  where
    inv2 | isFood (objId obj) = obj { objActions = ["examine", "consume", "eat"] } : inv
         | otherwise          = obj { objActions = ["examine"] } : inv

-- Das Inventar Anzeigen
-- (Fix 18) Schoene Auflistung statt Haskell-Listensyntax
showInventory :: Inventory -> IO ()
showInventory []     = putStrLn "Your Inventory is empty"
showInventory inv = do
    putStrLn "Your Inventory:"
    mapM_ (\o -> putStrLn ("  - " ++ objName o)) inv
interactSword :: Object -> IO ()
-- (Fix 19) Englischer Text statt deutscher Zeile
-- DESIGN (Autor): Der Protagonist ist nicht der Schwert-Held. Das Schwert
-- gibt keinen Bonus, es loest den Nebel aus. Dieser Satz bleibt der
-- einzige Hinweis darauf und wird nicht angefasst.
interactSword _ = putStrLn "The blade of the Sword glows briefly, then fades again."
