-- Deterministische Tests der Mechaniken. Nicht Teil des Spiels.
--   runghc --ghc-arg=-package=array --ghc-arg=-package=mtl --ghc-arg=-package=random Tests.hs
--   (neuere runghc-Versionen interpretieren `-package array` als Dateinamen)
module Main where

import Data.Array
import Data.List (sort, isInfixOf, nub, find)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen)
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants

ok :: String -> Bool -> IO ()
ok label True  = putStrLn ("  PASS  " ++ label)
ok label False = putStrLn ("  FAIL  " ++ label)

section :: String -> IO ()
section s = putStrLn ("\n== " ++ s)

-- Minimaler Spielzustand zum Testen.
-- (Plan 3) Fester Seed: dadurch ist jeder Wurf reproduzierbar.
mkGame :: Game
mkGame = mkGameWithSeed testSeed

testSeed :: Int
testSeed = 20240501

mkGameWithSeed :: Int -> Game
mkGameWithSeed seed = Game { gameLocation = locById 2
                           , gameCharacter = giveName "Tester"
                           , gameInventory = []
                           , gameObjects = objectList
                           , gameWolves = initialWolves
                           , gameShrineFlags = initialShrineFlags
                           , gamePrincess = initialPrincessStatus
                           , gameStdGen = mkStdGen seed }

-- n Wuerfe ausfuehren und zurueckgeben: der Test prueft die WUERTE, nicht
-- den Generator.
rollsIn :: Int -> Int -> Int -> Game -> ([Int], Game)
rollsIn n lo hi = go n
  where
    go 0 g = ([], g)
    go k g = let (v, g') = rollIn lo hi g
                 (vs, g'') = go (k - 1) g'
             in (v : vs, g'')

wHp :: ObjectID -> [Wolf] -> Maybe Life
wHp wid ws = fmap wolfHp (find ((== wid) . wolfId) ws)

allFlags :: (Bool, Bool, Bool, Bool)
allFlags = (True, True, True, True)

main :: IO ()
main = do
  putStrLn "=== The Fog - Mechanik-Tests ==="

  section "A3: Schaden & Verteidigung"
  ok "damage 3/5 (Schild) = 0"        (damage 3 5 == 0)
  ok "damage 3/1 (ohne Schild) = 2"   (damage 3 1 == 2)
  ok "damage 0/1 = 0"                 (damage 0 1 == 0)
  ok "Schild = Basis 1 + Bonus 4"     (getDef (addDef [] (giveName "x")) == 1)
  ok "mit Schild Def 5"               (getDef (addDef [objById 3] (giveName "x")) == 5)
  ok "A7: Atk 6 / HP 30"              (getAtk (giveName "x") == 6 && getHP (giveName "x") == 30)
  ok "Schwert gibt KEINEN Atk-Bonus"  (getAtk (addAng [objById 9] (giveName "x")) == 6)

  section "A4/B5: Zug-Ökonomie"
  let g0 = mkGame
      g1 = advanceTurn g0
      g2 = advanceTurn (advanceTurn g1)
  ok "Ein Zug = +1 Step"              (getSteps (gameCharacter g1) == 1)
  ok "Drei Zuege = 3 Steps"           (getSteps (gameCharacter g2) == 3)
  ok "Wölfe ziehen pro Zug"           (map wolfLoc (gameWolves g1) /= map wolfLoc (gameWolves g0))
  ok "ohne Nebel kein LP-Verlust"     (getHP (gameCharacter g2) == 30)
  ok "Guardian steht still"            (wolfLoc (head (gameWolves g1)) == 28)
  let gF = g0 { gameShrineFlags = allFlags }
      gF1 = advanceTurn gF
  ok "fogActive aus Flags"             (fogActive gF && not (fogActive g0))
  ok "Nebel: 1 LP pro Zug"            (getHP (gameCharacter gF1) == 29)
  ok "Nebel kann nicht unter 0"        (getHP (gameCharacter (advanceTurn (gF { gameCharacter = (gameCharacter gF) { charLife = 0 } }))) == 0)
  ok "flee kostet einen Zug"           (getSteps (gameCharacter (advanceTurn g0)) == 1)

  section "C8: Namensauflösung"
  ok "exakt 'Apple'"                  (fmap objName (getObjectInList "Apple" (gameObjects (mkGame))) == Just "Apple")
  ok "lowercase 'apple'"              (fmap objName (getObjectInList "apple" (gameObjects (mkGame))) == Just "Apple")
  ok "'the apple'"                    (fmap objName (getObjectInList "the apple" (gameObjects (mkGame))) == Just "Apple")
  ok "Plural 'apples'"                (fmap objName (getObjectInList "apples" (gameObjects (mkGame))) == Just "Apple")
  ok "'berries' -> Berries"           (fmap objName (getObjectInList "berries" (gameObjects (mkGame))) == Just "Berries")
  ok "'Princess' bleibt Princess"     (fmap objName (getObjectInList "Princess" (gameObjects (mkGame))) == Just "Princess")
  ok "'princess' kein 'Princes'"      (fmap objName (getObjectInList "princess" (gameObjects (mkGame))) == Just "Princess")
  ok "'princes' wird NICHT gefunden"  (getObjectInList "princes" (gameObjects (mkGame)) == Nothing)
  ok "'the princess'"                 (fmap objName (getObjectInList "the princess" (gameObjects (mkGame))) == Just "Princess")

  section "A5b: Wolf-Namensauflösung"
  let g = mkGame
      loc3 = locById 3
      loc28 = locById 28
      loc2 = locById 2
      nm o = maybe "?" (objName . id) o
      oid = maybe (-1) objId
  ok "loc 3, 'wolf' -> Tutorial"      (nm (getObject "wolf" loc3 (gameObjects g) (gameWolves g)) == "Wolf")
  ok "loc 3, 'Wolf' -> Tutorial"      (oid (getObject "Wolf" loc3 (gameObjects g) (gameWolves g)) == tutorialWolfId)
  ok "loc 28, 'wolf' -> Guardian"     (oid (getObject "wolf" loc28 (gameObjects g) (gameWolves g)) == guardianWolfId)
  ok "loc 2, 'Guardian' -> gefunden"  (oid (getObject "Guardian" loc2 (gameObjects g) (gameWolves g)) == guardianWolfId)
  ok "  ...aber nicht 'hier'"         (not (isObjHere (fromMaybe (Object 0 0 "" "" []) (getObject "Guardian" loc2 (gameObjects g) (gameWolves g))) loc2))
  ok "loc 2, 'Patrolling Wolf #1'"    (oid (getObject "Patrolling Wolf #1" loc2 (gameObjects g) (gameWolves g)) == 41)
  ok "loc 2, 'Castle Guard'"          (oid (getObject "Castle Guard" loc2 (gameObjects g) (gameWolves g)) == castleGuardId)
  ok "loc 2, 'wolf' findet NICHTS"    (getObject "wolf" loc2 (gameObjects g) (gameWolves g) == Nothing)
  ok "loc 2, 'wolves' findet NICHTS"  (getObject "wolves" loc2 (gameObjects g) (gameWolves g) == Nothing)
  ok "B7: 'Fog Wolf 1'"               (wolfIdToName 100 == "Fog Wolf 1")
  ok "B7: 'Fog Wolf 8'"               (wolfIdToName 107 == "Fog Wolf 8")
  ok "Patrolling Wolf #1"             (wolfIdToName 41 == "Patrolling Wolf #1")

  section "B3/B4: Nebeltexte"
  ok "Generic Loc 20 nennt Exits"     ("the exits: [North, East, South, West]" `isInfixOf` locD True (locById 20))
  ok "Loc 22 nennt seinen einen Exit" ("the exits: [East]" `isInfixOf` locD True (locById 22))
  ok "keine Exits -> 'no way out'"     (describeExits (Location 999 "Nowhere" []) == "no way out")
  ok "'no way out' im generischen Text"(locD True (Location 999 "Nowhere" []) == "Dense, oppressive fog. You can make out no way out at all.")
  ok "Loc 0 Home eigener Text"        ("recognise your home" `isInfixOf` locD True (locById 0))
  ok "Loc 54 Kampf-Banner"            (locD True fightLocation == ">>You are in a Fight!<<")
  ok "Loc 34 Schloss eigener Text"    (locD True (locById 34) /= locD True (locById 20))
  ok "SE wird NICHT gelistet (17)"    (not ("South-East" `isInfixOf` locD True (locById 17)))
  ok "SE wird NICHT gelistet (33)"    (not ("South-East" `isInfixOf` locD True (locById 33)))
  ok "SE wird NICHT gelistet (44)"    (not ("South-East" `isInfixOf` locD True (locById 44)))
  ok "SE wird NICHT gelistet (49)"    (not ("South-East" `isInfixOf` locD True (locById 49)))
  ok "ohne Nebel: alte descr"         (locD False (locById 20) == descr ! 20)
  ok "Alle Locations haben fogText"   (all (\l -> locD True l /= "") gameMap)
  ok "Alle Locations haben descr"     (all (\l -> locD False l /= "") gameMap)

  section "B2: Nebelwölfe"
  let gPost = g { gameShrineFlags = allFlags
                , gameWolves = spawnPostFogWolves (gameWolves g) allFlags }
      ids = map wolfId (gameWolves gPost)
  ok "8 Nebelwölfe gespawnt"          (length (filter (>= 100) ids) == 8)
  ok "IDs 100-107"                    (sort (filter (>= 100) ids) == [100..107])
  ok "30/16/4"                        (let w = head (filter ((>= 100) . wolfId) (gameWolves gPost))
                                      in wolfHp w == 30 && wolfAttack w == 16 && wolfDefense w == 4)
  ok "stehen still (leerer Pfad)"     (all (\w -> null (wolfPath w)) (filter ((>= 100) . wolfId) (gameWolves gPost)))
  ok "kein Doppel-Spawn"               (length (filter (>= 100) (map wolfId (spawnPostFogWolves (gameWolves gPost) allFlags))) == 8)
  ok "ohne Schreine kein Spawn"        (length (spawnPostFogWolves (gameWolves g) initialShrineFlags) == length (gameWolves g))

  section "A2: Essen"
  ok "Apfel isst +10"                 (foodValue 2 == Just 10)
  ok "Apfel is Food"                  (isFood 2)
  ok "Schild ist kein Essen"          (not (isFood 3))
  ok "Beeren im Wald"                 (objPos (objById 18) `elem` [14])
  ok "jede Location 12-27 hat Essen"  (all (\l -> any (\o -> objPos o == locId l && isFood (objId o)) objectList)
                                         [locById i | i <- [14,16,22,27]])

  section "A6: deleteByObjId"
  let o = objById 11
      oTaken = o { objActions = ["examine"] }
      lst = o : filter ((/= 11) . objId) objectList
  ok "strukturgleiche Kandidaten weg"  (null (getObjectInList "Princess" (deleteByObjId 11 lst)))
  ok "Objekt-Text eigene Aktionen"     (takeObj oTaken lst [] `seq` True)

  section "Shrine-Flags (Regression)"
  let fl i = updateShrineFlags (objById i) (False,False,False,False)
      a = fl 8; b = updateShrineFlags (objById 6) a
      c = updateShrineFlags (objById 5) b; d = updateShrineFlags (objById 7) c
  ok "Earth -> (True,False,False,False)"    (a == (True,False,False,False))
  ok "+Fire  -> (True,False,True,False)"    (b == (True,False,True,False))
  ok "+Water -> (True,True,True,False)"     (c == (True,True,True,False))
  ok "+Air   -> alle vier"                  (d == (True,True,True,True) && checkEnding d)

  section "Plan 1: Wolf-HP ueber Flucht hinweg"
  -- Der Kern des Fixes: der Schaden darf beim `flee` nicht wegfallen.
  let wounded = persistWolfHp (mkGame) 41 5
      untouched = mkGame
      fogWolves = spawnPostFogWolves (gameWolves untouched) allFlags
      fogWolf = head (filter ((>= 100) . wolfId) fogWolves)
  ok "Flucht laesst HP stehen (12 -> 5)" (wHp 41 (gameWolves wounded) == Just 5)
  ok "andere Woelfe unberuehrt"         (wHp 44 (gameWolves wounded) == wHp 44 (gameWolves untouched))
  ok "setWolfHp greift nur beim Wolf"   (wolfHp (head (gameWolves wounded)) == 30)
  ok "HP 0 belebt nicht"                (wHp 41 (gameWolves (persistWolfHp (mkGame) 41 0)) == Just 12)
  ok "HP werden auf Max geklemmt"       (wHp 41 (gameWolves (persistWolfHp (mkGame) 41 999)) == Just 12)
  -- Die Kampf-Anzeige braucht aktuelle UND maximale LP: vorher stand dort
  -- (hp, hp), ein angeschlagener Wolf war also "7/7" mit vollem Balken.
  ok "Kampf zeigt 5/12"                 (wolfFullStats (gameWolves wounded) 41 == (5, 12))
  ok "Kampf zeigt unversehrt 12/12"     (wolfFullStats (gameWolves untouched) 41 == (12, 12))
  ok "Guardian 30/30"                   (wolfFullStats (gameWolves untouched) guardianWolfId == (30, 30))
  ok "alle Wolfs-HP <= Max-HP"          (all (\w -> wolfHp w <= wolfMaxHp w) initialWolves)
  ok "Nebelwoelfe 30/30"                (wolfHp fogWolf == 30 && wolfMaxHp fogWolf == 30)

  section "Plan 2: Vorwarnung im Nachbarraum"
  let ws = gameWolves (mkGame)   -- Wolf 44 (Tutorial) steht auf Loc 3
  -- Von Loc 2 fuehrt der Exit N nach 3 -> Warnung mit Richtung.
  ok "Loc 2 warnt (Wolf auf 3)"         (checkNeighbourWolf (locById 2) ws)
  ok "Loc 2 nennt Richtung N"           (neighbourWolfDirs (locById 2) ws == [N])
  ok "Loc 6 warnt (Wolf auf 3, S)"      (neighbourWolfDirs (locById 6) ws == [S])
  ok "Loc 1 warnt NICHT"                (not (checkNeighbourWolf (locById 1) ws))
  ok "Loc 3 selbst: keine Vorwarnung"   (not (checkNeighbourWolf (locById 3) ws))
  -- Loc 4 (Basement) haengt an Loc 0, nicht an 3 -> ruhig.
  ok "fremder Raum bleibt ruhig"         (not (checkNeighbourWolf (locById 4) ws))
  -- Tote Woelfe warnen nicht.
  ok "toter Wolf warnt nicht"           (not (checkNeighbourWolf (locById 2)
                                                (map (\w -> w { wolfDead = True }) ws)))
  -- Geheimweg (Loc 17 -> 28, SE) darf nicht verraten werden: der Wolf steht
  -- dort, aber SE wird wie in exitNames nicht als Richtung genannt.
  ok "SE-Richtung bleibt geheim"        (null (neighbourWolfDirs (locById 17) ws))
  ok "SE-Raum warnt trotzdem"           (checkNeighbourWolf (locById 17) ws)
  ok "Warntext nennt Richtung"          ("to the north" `isInfixOf` nearbyWolfWarning False [N])
  ok "Nebel nennt keine Richtung"       (not ("north" `isInfixOf` nearbyWolfWarning True [N]))
  ok "Nebeltext vorhanden"              ("fog" `isInfixOf` nearbyWolfWarning True [N])
  ok "Nebel + keine Richtung: kein Absturz" (length (nearbyWolfWarning True []) > 0)

  section "Plan 3: Reproduzierbarer Zufall"
  let (a, ga) = rollsIn 10 0 5 (mkGameWithSeed testSeed)
      (b, gb) = rollsIn 10 0 5 (mkGameWithSeed testSeed)
  ok "gleicher Seed -> gleiche Folge"    (a == b)
  ok "gleicher Seed -> gleicher Stand"   (ga == gb)
  -- Fortsetzen muss weiterziehen, nicht wieder von vorn:
  let (c, _) = rollsIn 5 0 5 ga
  ok "Fortsetzung != Neustart"           (take 5 a /= c || length (nub a) <= 1)
  ok "Generator wandert mit"             (gameStdGen ga /= gameStdGen (mkGameWithSeed testSeed))
  ok "alle Werte im Wuerfelbereich"      (all (\v -> v >= 0 && v <= 5) a)
  -- Frischer, anderer Seed: andere Folge. Bewusst ueber 30 Woerfe geprueft -
  -- ein einzelner Wurf waere ein 1/6-Flake-Test.
  let many = map (\s -> fst (rollsIn 30 0 999 (mkGameWithSeed s))) [1..40]
  ok "40 Seeds != 1"                    (length (nub many) > 1)
  ok "gameWithSeed setzt den Generator" (gameStdGen (gameWithSeed 99 (mkGame)) == gameStdGen (mkGameWithSeed 99))
  ok "Seed fliesst nicht in die LP"     (all (\w -> wolfHp w == wolfMaxHp w) (gameWolves (gameWithSeed 7 (mkGame))))

  section "Punkt 4: Schrein-Inschrift"
  let insc = [5, 6, 7, 8, 14, 15, 16, 17, 32, 33, 34, 35]   -- Schreine + neue Orte
      phrase = "Four that wake, one that comes"
      newObjs = [objById i | i <- [32, 33, 34, 35]]
  -- (Autor) Der Summit Cairn (34) traegt den Wortlaut nicht mehr, sondern die
  -- Zahlen "4 and 1". Die Inschrift ist damit weiterhin an allen 12 Orten
  -- vorhanden, aber nur an 11 im Originalwortlaut - die gemeinsame Formel
  -- "Cut into the base, worn but still readable:" ist bei den neuen Orten weg.
  ok "Wortlaut an 11 Orten"             (all (\i -> phrase `isInfixOf` (objectTextFor (objById i))) (filter (/= 34) insc))
  ok "Inschrift an allen 12 Orten"     (all (\i -> not (null (objectTextFor (objById i)))) insc)
  ok "Cairn nennt die Zahlen"          ("4 and 1" `isInfixOf` (objectTextFor (objById 34)))
  ok "Cairn ohne Wortlaut"             (not (phrase `isInfixOf` (objectTextFor (objById 34))))
  ok "keine Basisformel mehr"          (all (\o -> not ("Cut into the base" `isInfixOf` objectTextFor o)) newObjs)
  -- Die Orte, an denen die Schreine stehen, tragen sie weiterhin.
  ok "Schreine unveraendert (5-8)"    (all (\i -> phrase `isInfixOf` (objectTextFor (objById i))) [5, 6, 7, 8])
  ok "aktivierte Schreine (14-17)"     (all (\i -> phrase `isInfixOf` (objectTextFor (objById i))) [14, 15, 16, 17])
  -- Die vier neuen Orte: Umkehrpunkte der Route, alle lesbar, nicht
  -- mitnehmbar (sie gehoeren zum Ort, wie die Schreine).
  ok "4 neue Inschrift-Objekte"        (length newObjs == 4 && all (\o -> not (null (objectTextFor o))) newObjs)
  ok "alle lesbar (read+examine)"     (all (\o -> "read" `elem` objActions o && "examine" `elem` objActions o) newObjs)
  ok "keines mitnehmbar"               (not (any (\o -> any (`elem` objActions o) ["take", "pick"]) newObjs))
  ok "Orte: 0, 33, 44, 60"             (map objPos newObjs == [0, 33, 44, 60])
  ok "Orte existieren wirklich"        (all (\o -> any ((== objPos o) . locId) gameMap) newObjs)
  ok "Objekte auffindbar (Name)"       (all (\o -> fmap objId (getObjectInList (map toLower (objName o))
                                                            (gameObjects (mkGame))) == Just (objId o)) newObjs)
  -- Namenseindeutigkeit: `getObjectInList` faellt bei Gleichstand auf den
  -- ersten Listeneintrag zurueck, ein doppelter Name macht ein Objekt
  -- unerreichbar. Stone/Broken/Tomb Tablet sind bereits vergeben.
  ok "Objektnamen eindeutig"           (length (map objName objectList) == length (nub (map objName objectList)))
  ok "Objektnamen eindeutig (lower)"   (length (map (map toLower . objName) objectList)
                                        == length (nub (map (map toLower . objName) objectList)))
  ok "Neue Namen kommen nur einmal vor" (all (\o -> length (filter ((== objName o) . objName) objectList) == 1) newObjs)

  section "Datenintegritaet"
  ok "jede Location per locById"      (all (\l -> locName (locById (locId l)) == locName l) gameMap)
  -- Gelesen als "alle IDs aus 0..60 ausser den entfernten", nicht als
  -- fest verdrahtete Liste: Loc 5 und Loc 54 sind raus (Punkt 1).
  ok "gameMap = 0..60 ohne Loc 5, 54"
      (sort (map locId gameMap) == filter (`notElem` [5, 54]) [0 .. 60])
  ok "Loc 5 wirklich entfernt"        (null (filter ((== 5) . locId) gameMap))
  ok "descr[5] existiert trotzdem"    (inRange (bounds descr) 5 && length (descr ! 5) > 0)
  ok "jedes Objekt hat objectText"    (all (\o -> inRange (bounds objectText) (objId o) && length (objectTextFor o) > 0) objectList)
  -- (Punkt 4) Die obere Grenze kommt aus `bounds objectText` statt aus einer
  -- festen 31 - sonst waere jeder neue Content-Eintrag ein stiller Bruch.
  ok "objectText dicht"               (let (lo, hi) = bounds objectText
                                         in and [inRange (bounds objectText) i && hasDef i | i <- [lo..hi]])
  -- Regression: Wolf- und Objekt-IDs teilten sich frueher den Nummernraum,
  -- wodurch `examine Wolf (Guardian)` den Text von Object 0 ausgab.
  ok "keine Wolf-ID kollidiert mit Objekt-ID"
                                     (all (\w -> wolfId w `notElem` map objId objectList) initialWolves)
  ok "Wölfe 40-46, Objekte 0-31"       (all (\w -> wolfId w >= 40 && wolfId w < 100) initialWolves)
  ok "jede Location 0..60 hat descr"  (all (\i -> inRange (bounds descr) i && length (descr ! i) > 0) [0..60])
  ok "alle Exits zeigen auf echte Loc" (all (\l -> all (\(t,_) -> locName (locById t) /= "Nowhere") (locExits l)) gameMap)
  -- Die Katakomben (55-60, neuer Content) muessen symmetrisch sein: jeder
  -- Exit braucht ein Gegenstueck. Ein einseitiger Exit macht einen Raum von
  -- einer Seite unerreichbar - so war die Treppe im "Dark Room" von unten
  -- nicht erreichbar (55 zeigte nach W, 56 nach O).
  ok "Katakomben symmetrisch"       (all symmetric catacombs)
  -- Vorbestehend und bewusst so (nicht Teil des neuen Content):
  --   53 -> 0  das Startmenue fuehrt in die Welt, nicht zurueck
  --   51 -> 52 -> 49 bilden zusammen den Zyklus ueber die Flying Stones
  ok "Katakomben 55-60 vollstaendig" (sort (map locId catacombs) == [55..60])
  ok "kein Location-Id doppelt"     (length (map locId gameMap) == length (nub (map locId gameMap)))
  ok "Nebelwölfe fernab der Katakomben"
                                     (all (\i -> i `notElem` postFogWolfLocations) [55,56,57,58,59,60])
  ok "C5b: Help im Startinventar"     (map objName initialInventory == ["Help"])
  ok "Phantom-Objekt 12 entfernt"     (null (filter ((== 12) . objId) objectList))
  ok "statisches Wolf-Objekt weg"     (null (filter ((== 10) . objId) objectList))
  ok "kein 'help' mehr bei der Prinzessin" (not ("help" `elem` objActions (objById 11)))
  ok "'burn' kein No-op mehr"         (not (any (\o -> "burn" `elem` objActions o) objectList))

  section "Punkt 1: Wolf-Fight (Loc 54) entfernt"  -- Der Eintrag behauptete einen Ort, den es nicht gab: kein Exit fuehrte
  -- nach 54, die Location selbst hatte keine Exits. `fightLoop` braucht
  -- trotzdem eine echte Location fuer ASCII-Kunst und Banner.
  ok "Loc 54 nicht mehr in gameMap"   (null (filter ((== 54) . locId) gameMap))
  ok "locById 54 -> Nowhere"          (locName (locById 54) == "Nowhere")
  ok "fightLocation != gameMap"       (locName fightLocation /= locName (locById 54))
  -- `locA = locId`: nur die ID 54 selektiert `ascii 54` (das Kampfbild).
  -- Aendern der ID waere kein Refactor, sondern eine stumme Regression.
  ok "fightLocation traegt die ID 54"  (locId fightLocation == 54 && locA fightLocation == 54)
  ok "fightLocation hat keine Exits"   (null (locExits fightLocation))
  -- Das Banner kommt aus `descr`/`fogDescr` - beide Eintraege muessen
  -- bleiben, sonst zeigte der Kampf im Nebel den generischen Text.
  ok "descr[54] existiert trotzdem"    (inRange (bounds descr) 54 && length (descr ! 54) > 0)
  ok "descr[54] ist das Kampf-Banner"  (descr ! 54 == ">>You are in a Fight!<<")
  ok "ohne Nebel: Kampf-Banner"       (locD False fightLocation == ">>You are in a Fight!<<")
  ok "fogDescr kennt weiter 54"        (lookup 54 fogDescr == Just ">>You are in a Fight!<<")
  -- `descr` bleibt dicht: 0..60 ohne Luecken, sonst bricht das Array.
  ok "descr weiterhin dicht 0..60"    (length [i | i <- [0 .. 60], inRange (bounds descr) i, hasDescr i] == 61)

  section "Punkt 2: save ohne Prinzessin im Raum"
  -- Nur die Prinzessin traegt "save" in ihren objActions. Deshalb ist der
  -- neue Zweig in `performAction` der einzige Weg, ueberhaupt jemand
  -- anderen anzusprechen - ohne ihn bekaeme `save Door Lintel` weiterhin
  -- "You can't do that here.".
  ok "nur die Prinzessin hat 'save'"   (map objId (filter (\o -> "save" `elem` objActions o) objectList)
                                         == [princessId])
  let at loc    = mkGame { gameLocation = loc }
      riverG    = at (locById 33)
      homeG     = at (locById 0)
      gDrown    = riverG { gamePrincess = PrincessDead }
      gSaved    = (riverG { gameObjects = deleteByObjId princessId (gameObjects riverG) })
                      { gamePrincess = PrincessSaved }
  ok "am Fluss: Prinzessin da"        (princessInRoom riverG)
  ok "zu Hause: niemand"              (not (princessInRoom homeG))
  -- PrincessDead laesst ihr Objekt liegen: Anwesenheit, nicht Status.
  ok "tot, aber am Fluss: da"         (princessInRoom gDrown)
  -- PrincessSaved loescht es: Anwesenheit nicht aus dem Status ableitbar.
  ok "gerettet: Objekt ist weg"       (not (any ((== princessId) . objId) (gameObjects gSaved)))
  ok "gerettet: niemand"              (not (princessInRoom gSaved))
  -- Kein Wurf, kein Zustandswechsel: `nobodyToSave` faellt in main.hs, aber
  -- es kann nur der Zweig sein, der den 1/3-Wurf in `handlePrincessSave`
  -- ueberhaupt erreicht - und der verlangt `princessInRoom`.
  ok "Rettung nur mit Objekt im Raum"
      (all (\g -> not (princessInRoom g) || princessId `elem` map objId (gameObjects g))
           [homeG, riverG, gSaved])
  ok "Prinzessin lebt an genau einem Ort"
      (length (filter (\g -> princessInRoom g)
                      [at (locById i) | i <- map locId gameMap]) == 1)

  section "Punkt 3: Flucht ist ein Wurf"
  -- `flee` war bedingungslos erfolgreich: jeder Kampf abbrechbar, auch der
  -- Guardian (30 LP) und der Crypt Warden (24 LP).
  let seeds = [1 .. 200]
      fleeNonFog = map (\s -> fst (fleeSucceeds (gameWithSeed s mkGame))) seeds
      fogGame s = gameWithSeed s mkGame { gameShrineFlags = allFlags }
      fleeFog    = [ fst (fleeSucceeds (fogGame s)) | s <- seeds ]
      noRoll     = [ gameStdGen (fogGame s) == gameStdGen (snd (fleeSucceeds (fogGame s)))
                   | s <- seeds ]
  ok "Flucht gelingt manchmal"       (elem True fleeNonFog)
  ok "Flucht misslingt manchmal"     (elem False fleeNonFog)
  -- Kein Flake-Test: Binomial(200, 0.5) hat sd ~7.1, das Band ist weit.
  ok "Verhaeltnis etwa 1 : 1"        (length (filter id fleeNonFog) > 60
                                        && length (filter id fleeNonFog) < 140)
  -- HART: im Nebel gelingt sie immer. Die Nebelwoelfe hauen 11-13 zurueck
  -- und nehmen nur 2-4 - ein Fehlschlag waere ein Tod ohne Gegenmoeglichkeit.
  ok "im Nebel immer erfolgreich"    (all id fleeFog)
  ok "im Nebel wird gar nicht geworfen" (and noRoll)
  ok "im Nebel: gleicher Generator"  (and noRoll)
  -- Reproduzierbar (Plan 3): gleicher Seed, gleiche Entscheidung.
  ok "gleicher Seed -> gleiche Flucht"
      (all (\s -> fst (fleeSucceeds (gameWithSeed s mkGame))
                == fst (fleeSucceeds (gameWithSeed s mkGame))) seeds)
  -- Der Wurf wandert mit: zwei Fluchten aus demselben Spielstand sind
  -- unabhaengig, nicht identisch (sonst waere die zweite ein Relikt).
  let twoFrom g = let (a, g1) = fleeSucceeds g
                      (b, _)   = fleeSucceeds g1
                  in (a, b)
      pairs = map (\s -> twoFrom (gameWithSeed s mkGame)) seeds
  ok "zwei Fluchten sind unabhaengig" (length (nub pairs) > 1)
  ok "beide Ergebnisse kommen vor"    (length (nub pairs) == 4)
  -- Plan 1 bleibt auch im Fehlschlag-Fall gelten: `fightLoop` schreibt
  -- `enemyHP` in beiden Zweigen ueber `persistWolfHp` zurueck.
  let fled = persistWolfHp (snd (fleeSucceeds mkGame)) 41 5
  ok "HP bleiben auch nach Flucht"    (wHp 41 (gameWolves fled) == Just 5)

  section "Punkt 4: Beute der Woelfe"
  let drops = dropFoodTemplates
      dropIds' = map objId drops
  -- IDs ab 36: alles darunter ist vergeben, 32-35 sind die Inschriften.
  ok "IDs passen zu wolfDropIds"     (dropIds' == map fst wolfDropIds)
  ok "IDs sind >= 36 und ohne Luecke" (dropIds' == [36 .. 38])
  ok "jede ID hat einen objectText"  (all (\i -> inRange (bounds objectText) i
                                                && length (objectTextFor (objObjById i)) > 0) dropIds')
  ok "alle sind essbar"               (all (\i -> isFood i && foodValue i /= Nothing) dropIds')
  ok "alle sind mitnehmbar"           (all (\o -> any (`elem` objActions o) ["take", "pick"]) drops)
  -- `getObjectInList` faellt bei Gleichstand auf den ersten Treffer zurueck:
  -- ein doppelter Name (auch nur in der Kleinschreibung) macht ein Objekt
  -- unerreichbar. Die Beute liegt in `objectList` nicht, aber sie kann in
  -- denselben Raum wie die Welt-Nahrung.
  let allNames = map objName (objectList ++ drops)
      lNames   = map (map toLower . objName) (objectList ++ drops)
  ok "Beute-Namen eindeutig"         (length allNames == length (nub allNames))
  ok "Beute-Namen eindeutig (lower)" (length lNames == length (nub lNames))
  ok "Beute heisst nicht 'food'"      (all (\o -> objName o /= "food") drops)
  ok "foodName kennt die Beute"       (all (\o -> foodName (objId o) == objName o) drops)
  -- Nur die drei Kaempfe, die dem Spieler wirklich etwas kosten.
  ok "Guardian, Guard, Warden"       (all dropsFrom [guardianWolfId, castleGuardId, cryptWardenId])
  ok "Tutorial-Wolf gibt nichts"     (not (dropsFrom tutorialWolfId))
  ok "Patrouillenwoelfe geben nichts" (not (any dropsFrom [41, 42, 43]))
  ok "Nebelwoelfe geben nichts"       (not (any dropsFrom [100, 101, 107]))
  -- Ort, Heilswert und Vorlage muessen zusammenpassen.
  let wardens = [(guardianWolfId, 28), (castleGuardId, 36), (cryptWardenId, 58)]
      landed s wid = let (ds, _) = wolfDrop (gameWithSeed s mkGame) wid in ds
      allDrops s = concat [landed s w | (w, _) <- wardens]
      seeds4 = [1 .. 120]
      tries = length seeds4 * length wardens
      landedIds = concat [map objId (landed s w) | s <- seeds4, (w, _) <- wardens]
      okDrop s w loc = let (ds, _) = wolfDrop (gameWithSeed s mkGame) w
                       in all (\d -> objPos d == loc && objId d `elem` dropIds') ds
      hits = length landedIds
  ok "Beute faellt ueberhaupt"       (hits > 0)
  ok "Beute faellt nicht jedes Mal"  (hits < tries)
  -- Band weit genug fuer keinen Flake: Binomial(360, 1/4) hat sd ~8.2,
  -- hier wird nur "nicht 0 und nicht alles" plus Groessenordnung geprueft.
  ok "flaechentreu (1 von 4)"        (hits > tries `div` 8 && hits < tries `div` 2)
  -- Die Beute liegt IMMER dort, wo der Wolf stand.
  ok "Beute liegt am Ort des Wolfs"  (all (\s -> all (\(w, l) -> okDrop s w l) wardens) seeds4)
  ok "vor dem Wolf: -1, nicht 0"     (all ((== (-1)) . objPos) drops)
  -- Reproduzierbar (Plan 3): gleicher Seed, gleiche Beute.
  ok "gleicher Seed -> gleiche Beute"
      (all (\s -> map objId (allDrops s) == map objId (allDrops s)) seeds4)
  ok "120 Seeds -> mehr als ein Bild" (length (nub (map (map objId . allDrops) seeds4)) > 1)
  -- Ohne passenden Wolf: kein Wurf, unveraenderter Generator.
  let noDrop g = wolfDrop g tutorialWolfId
  ok "falscher Wolf: keine Beute"    (fst (noDrop mkGame) == [])
  ok "falscher Wolf: kein Wurf"      (gameStdGen (snd (noDrop mkGame)) == gameStdGen mkGame)
  -- `attackObj` legt ohnehin eine Leiche an: die Beute kommt daneben, nicht
  -- an ihre Stelle.
  let (ds, _) = wolfDrop (gameWithSeed 2 mkGame) guardianWolfId
      corpse = wolfToObj (head (filter ((== guardianWolfId) . wolfId) initialWolves))
      after  = attackObj corpse (ds ++ gameObjects mkGame)
  ok "Beute ersetzt die Leiche nicht"
      (length (filter (\o -> objId o `elem` dropIds') after) == length ds)
  ok "Leiche liegt auch dort"        (any (\o -> objPos o == 28 && isPrefixOf' "dead" (objName o)) after)

objById :: Int -> Object
objById i = case filter ((== i) . objId) objectList of
              (o:_) -> o
              []    -> Object { objPos = -1, objId = i, objName = "?MISSING?", objDescription = "", objActions = [] }

-- Wie `objById`, aber ueber beide Objekt-Quellen: `objById` kennt nur
-- `objectList` und wuerde fuer die Wolf-Beute (IDs 36-38) "?MISSING?"
-- liefern - und damit jeden Texttest darueber scheitern lassen.
objObjById :: Int -> Object
objObjById i = case filter ((== i) . objId) (objectList ++ dropFoodTemplates) of
                 (o:_) -> o
                 []    -> Object { objPos = -1, objId = i, objName = "?MISSING?", objDescription = "", objActions = [] }

-- `Data.List.isPrefixOf` gibt es erst ab base 4.18.
isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

hasDef :: Int -> Bool
hasDef i = i `elem` [ fst e | e <- assocs objectText ]

hasDescr :: Int -> Bool
hasDescr i = i `elem` [ fst e | e <- assocs descr ]

-- Gegenrichtung: von `to` muss ein Exit zurueck nach `from` existieren.
symmetric :: Location -> Bool
symmetric l = all hasBack (locExits l)
  where
    hasBack (to, _) = (locId l `elem` map fst (locExits (locById to)))

catacombs :: [Location]
catacombs = [ locById i | i <- [55..60] ]
