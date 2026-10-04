module GameActions where

import Data.List
import Data.Array (bounds, (!))
import Data.Ix (inRange)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import System.Random (mkStdGen, randomR)
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

-- ---------------------------------------------------------------------------
-- (Plan 3) Zufall als Teil des Spielzustands
-- ---------------------------------------------------------------------------
-- Vorher stand in main.hs `roll lo hi = newStdGen >>= \\(value, _) ->
-- return value`: JEDER Wurf startete eine neue, frische Zufallsfolge. Der
-- Kampfverlauf haengte damit nicht vom Spielstand ab - derselbe
-- Befehlsstrom ergab jedes Mal etwas anderes, und Tests konnten den Kampf
-- ueberhaupt nicht nachstellen.
--
-- `rollIn` zieht aus `gameStdGen` und legt den fortgeschrittenen Generator
-- zurueck: gleicher Seed + gleiche Befehle => gleiches Ergebnis.
--
-- Bewusst in GameActions und nicht in main.hs: Tests.hs kann main.hs nicht
-- importieren (es ist ein Main-Modul). Mit dieser Verschiebung ist die
-- Zufallsfolge pruefbar.
rollIn :: Int -> Int -> Game -> (Int, Game)
rollIn lo hi game = (value, game { gameStdGen = next })
  where
    (value, next) = randomR (lo, hi) (gameStdGen game)

-- Spielstand mit festem Seed. `game` (main.hs) benutzt `newStdGen`; Tests
-- und kuenftige Checkpoints (Plan 5) setzen hier einen festen Wert.
gameWithSeed :: Int -> Game -> Game
gameWithSeed seed game = game { gameStdGen = mkStdGen seed }

-- ---------------------------------------------------------------------------
-- (Plan 2) Wolf-Vorwarnung fuer Nachbarräume
-- ---------------------------------------------------------------------------
-- Bisher warnte `checkEncounter` ausschliesslich bei einem Wolf IM aktuellen
-- Raum. Ein Wolf, der durch den Nachbarraum patrouilliert, war voellig
-- unsichtbar - bis er im nächsten Zug zuschlug. Der Spieler bekam keine
-- Gelegenheit, den Weg zu wechseln oder den Kampf vorzubereiten.
--
-- Die Warnung nennt die Richtung, damit sie handlungsrelevant ist. Ob ein
-- Wolf im Nebel anders behandelt wird, entscheidet `nearbyWolfWarning`.
neighbourWolves :: Location -> [Wolf] -> [Wolf]
neighbourWolves loc = filter inNeighbour
  where
    neighbours = map fst (locExits loc)
    inNeighbour w = not (wolfDead w) && wolfLoc w `elem` neighbours

-- Steht mindestens ein lebender Wolf in einem direkt angrenzenden Raum?
checkNeighbourWolf :: Location -> [Wolf] -> Bool
checkNeighbourWolf loc = not . null . neighbourWolves loc

-- In welche Richtung(en) liegt ein Wolf? Fuer die Warnanzeige.
--
-- `SE` wird wie in `exitNames` weggelassen: der geheime Durchgang ist eine
-- Belohnung und darf nicht durch eine Warnung verraten werden. Steht der Wolf
-- ausschliesslich hinter dem geheimen Zugang, bleibt es bei der
-- richtungslosen Variante der Warnung.
neighbourWolfDirs :: Location -> [Wolf] -> [Direction]
neighbourWolfDirs loc wolves =
    mapMaybe visible (locExits loc)
  where
    visible (target, dir)
        | dir /= SE && any (isIn target) wolves = Just dir
        | otherwise                              = Nothing
    isIn target w = not (wolfDead w) && wolfLoc w == target

-- Text der Nachbarraum-Warnung. Im Nebel wird die Richtung bewusst NICHT
-- verraten - dann ist jeder Weg eine Lotterie, das passt zum Todesmarsch.
nearbyWolfWarning :: Bool -> [Direction] -> String
nearbyWolfWarning inFog dirs
    | inFog     = "Something is breathing just beyond the fog. It is close."
    | otherwise = case dirs of
        [] -> "Something moves close by."
        [d] -> "You hear a wolf close by, to the " ++ lower (directionName d) ++ "."
        _ -> "You hear wolves close by, to the "
             ++ intercalate " and the " (map (lower . directionName) dirs) ++ "."
  where
    lower = map toLower

-- ---------------------------------------------------------------------------
-- (Plan 1) Wolf-HP ueber die Flucht hinweg behalten
-- ---------------------------------------------------------------------------
-- `fightLoop` (main.hs) rechnet die Wolf-HP in einem Parameter (`enemyHP`),
-- nicht im Spielzustand. Dieser Parameter wurde beim `flee` einfach
-- weggeworfen: nach fuenf Treffern und einer Flucht stand der Wolf wieder mit
-- vollen 12/12 LP da und der Kampf bis zum Neuangriff umsonst.
--
-- `setWolfHp` schreibt den Zwischenstand zurueck. Zwei Guard-Bedingungen:
--   * `hp > 0`  - ein toter Wolf wird nicht wiederbelebt (den Sieg-Fall
--                  setzt main.hs ueber `wolfDead`).
--   * `min wolfMaxHp` - die LP koennen die Max-LP nicht uebersteigen.
setWolfHp :: ObjectID -> Life -> [Wolf] -> [Wolf]
setWolfHp wid hp = map upd
  where
    upd w | wolfId w == wid && hp > 0 = w { wolfHp = min (wolfMaxHp w) hp }
          | otherwise                = w

-- Kampfstand eines Wolfs in den Spielzustand zurueckschreiben. Wird bei
-- Flucht, Kampf-Abbruch (EOF) und Tod des Helden benutzt - in allen drei
-- Faellen hat der Wolf den Schaden behalten.
persistWolfHp :: Game -> ObjectID -> Life -> Game
persistWolfHp game wid hp = game { gameWolves = setWolfHp wid hp (gameWolves game) }

-- ---------------------------------------------------------------------------
-- (Punkt 3) Flucht ist ein Wurf, kein Kalten Druecken
-- ---------------------------------------------------------------------------
-- `flee` war bedingungslos erfolgreich. Damit liess sich jeder Kampf abbrechen
-- - auch der Guardian (30 LP, Atk 8) und der Crypt Warden (24 LP, Atk 10),
-- die einzigen Kämpfe, die wirklich etwas kosten.
--
-- HARTE AUSNAHME IM NEBEL: `fogActive game` heisst, die Nebelwoelfe sind da
-- (30 LP, Atk 16, Def 4 - `getWolfStats`). Der Held richtet 2-4 Schaden an,
-- sie hauen 11-13 zurueck. Ein Fehlschlag waere dort ein Tod ohne
-- Gegenmoeglichkeit, und genau die Phase, in der der Todesmarsch laeuft,
-- waere unabspielbar. Also wird im Nebel gar nicht erst geworfen - das
-- kostet keinen Wurf, die Zufallsfolge bleibt zwischen den Phasen
-- vergleichbar.
--
-- Der Wurf laeuft ueber `rollIn`, damit die Reproduzierbarkeit aus Plan 3
-- erhalten bleibt: gleicher Seed + gleiche Befehle = gleiche Flucht.
fleeChance :: Int
fleeChance = 1   -- 1 von 2

fleeSucceeds :: Game -> (Bool, Game)
fleeSucceeds game
    | fogActive game = (True, game)
    | otherwise      = let (r, g) = rollIn 0 fleeChance game in (r == fleeChance, g)

-- ---------------------------------------------------------------------------
-- (Punkt 4) Ein getoeteter Wolf laesst manchmal etwas fallen
-- ---------------------------------------------------------------------------
-- Nur die drei Kaempfe, die dem Spieler wirklich etwas kosten: Guardian
-- (40, Loc 28), Castle Guard (45, Loc 36), Crypt Warden (46, Loc 58).
--
-- Bewusst NICHT der Tutorial-Wolf (44, Loc 3). Dort ist Nahrung kein
-- Preise fuer einen Kampf, den man ohnehin nicht vermeiden kann - ein
-- zufaelliges Stueck neben dem ersten Wolf wuerde den ersten Kampf
-- wertlos machen. Ebenso nicht die Patrouillenwoelfe (41-43): sie laufen
-- einem bei jedem Zug ueber den Weg, das ist der Grund fuer die Vorwarnung
-- aus Plan 2, nicht fuer eine Belohnung.
--
-- Und nicht die Nebelwoelfe (100+): sie nehmen nur 2-4 Schaden und hauen
-- 11-13 zurueck. Sie zu bekaempfen ist ohnehin aussichtslos, eine Belohnung
-- dafuer waere eine Einladung, es zu versuchen.
dropsFrom :: ObjectID -> Bool
dropsFrom wid = wid `elem` [guardianWolfId, castleGuardId, cryptWardenId]

-- 1 von 4. Niedrig ist Absicht: die Beute soll einen Kampf versuessnen, nicht
-- zum Essen der letzte Grund werden, ihn ueberhaupt zu fuehren.
dropRoll :: Int
dropRoll = 3

-- Liefert die neue Beute und den weitergereichten Spielzustand. Der Wurf
-- muss ueber `rollIn` laufen, sonst waere die Beute nicht reproduzierbar
-- (Plan 3) - und die Auswahl des Stuecks ueber einen zweiten Wurf, nicht
-- ueber `cycle`, damit sie dieselbe Folge folgt.
--
-- Aufrufer: `fightLoop` im Sieger-Zweig, und zwar auf dem Zustand NACH dem
-- Angriff. Auf dem Zustand davor waere die Zufallsfolge des Kampfes
-- verschoben.
wolfDrop :: Game -> ObjectID -> (ObjectList, Game)
wolfDrop game wid
    | not (dropsFrom wid) = ([], game)
    | otherwise =
        let (hit, g1) = rollIn 0 dropRoll game
        in if hit /= 0 then ([], g1)
           else let (which, g2) = rollIn 0 (length dropFoodTemplates - 1) g1
                in ([placeAt (wolfLocOf wid (gameWolves g2)) (dropFoodTemplates !! which)], g2)
  where
    placeAt loc t = t { objPos = loc }

-- Wo der Wolf stand. Zum Zeitpunkt des Aufrufs ist er ueber `wolfDead`
-- bereits als tot markiert, `wolfLoc` gilt aber weiterhin. Ein Wolf, den es
-- nicht gibt, legt die Beute auf Loc 0 - das Objekt bleibt dann immer noch
-- auffindbar und essbar, nur eben im falschen Raum.
wolfLocOf :: ObjectID -> [Wolf] -> Int
wolfLocOf wid = maybe 0 wolfLoc . find ((== wid) . wolfId)

-- ---------------------------------------------------------------------------
-- (Punkt 2) `save` ohne Prinzessin im Raum
-- ---------------------------------------------------------------------------
-- Anwesenheit muss ausdruecklich geprueft werden. Der Status allein sagt
-- nichts darueber aus, ob sie vor einem steht: `PrincessSaved` loescht ihr
-- Objekt aus `gameObjects`, `PrincessDead` laesst es liegen (beides in
-- main.hs). Nur `gameObjects` an der aktuellen Location beantwortet die
-- Frage, die der Spieler stellt.
princessInRoom :: Game -> Bool
princessInRoom game = any inRoom (gameObjects game)
  where
    inRoom o = objId o == princessId && objPos o == locId (gameLocation game)

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
-- (Plan 1) Die Max-LP kommen aus `wolfMaxHp`, nicht mehr aus dem aktuellen
-- Stand. Vorher stand hier `(hp, hp)`: ein Wolf, der nach einem Kampf noch
-- 7 LP hatte, wurde mit "7/7" und einem vollen Balken angezeigt - der
-- angerichtete Schaden war unsichtbar, obwohl er im Zustand stand.
wolfFullStats :: [Wolf] -> ObjectID -> (Life, Life)
wolfFullStats wolves wid = case find (\w -> wolfId w == wid) wolves of
    Just w  -> (wolfHp w, wolfMaxHp w)
    Nothing -> (12, 12)

-- Add new wolves after fog descends
-- (Fix 14) Guard: nur spawnen, wenn noch keine Post-Fog-Woelfe existieren
-- (vorher verdoppelten sich die Woelfe mit jedem Schritt).
-- (A6) Der `step`-Parameter ist entfernt - er wurde nie benutzt.
spawnPostFogWolves :: [Wolf] -> (Bool, Bool, Bool, Bool) -> [Wolf]
spawnPostFogWolves wolves flags =
    if checkEnding flags && not (any (\w -> wolfId w >= postFogWolfIdBase) wolves)
        then let locations = postFogWolfLocations
                 (hp, atk, def) = getWolfStats flags
                 -- (Plan 1) Nebelwoelfe starten mit vollen LP, also sind
                 -- aktuelle und maximale LP identisch.
                 newWolf loc id = Wolf id loc [] 0 False
                                            hp hp atk def False
             in wolves ++ zipWith newWolf locations
                                  [postFogWolfIdBase .. postFogWolfIdBase + length locations - 1]
        else wolves
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
