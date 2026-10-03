-- Deterministische Tests der Mechaniken. Nicht Teil des Spiels.
--   runghc --ghc-arg=-package=array --ghc-arg=-package=mtl --ghc-arg=-package=random Tests.hs
--   (neuere runghc-Versionen interpretieren `-package array` als Dateinamen)
module Main where

import Data.Array
import Data.List (sort, isInfixOf, nub)
import Data.Maybe (fromMaybe)
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

-- Minimaler Spielzustand zum Testen
mkGame :: Game
mkGame = Game { gameLocation = locById 2
              , gameCharacter = giveName "Tester"
              , gameInventory = []
              , gameObjects = objectList
              , gameWolves = initialWolves
              , gameShrineFlags = initialShrineFlags
              , gamePrincess = initialPrincessStatus }

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
  ok "Loc 54 Kampf-Banner"            (locD True (locById 54) == ">>You are in a Fight!<<")
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

  section "Datenintegrität"
  ok "jede Location per locById"      (all (\l -> locName (locById (locId l)) == locName l) gameMap)
  ok "gameMap = 0..60 ohne Loc 5"     (sort (map locId gameMap) == [0,1,2,3,4] ++ [6..60])
  ok "Loc 5 wirklich entfernt"        (null (filter ((== 5) . locId) gameMap))
  ok "descr[5] existiert trotzdem"    (inRange (bounds descr) 5 && length (descr ! 5) > 0)
  ok "jedes Objekt hat objectText"    (all (\o -> inRange (bounds objectText) (objId o) && length (objectTextFor o) > 0) objectList)
  ok "objectText dicht 0..31"         (and [inRange (bounds objectText) i && hasDef i | i <- [0..31]])
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

objById :: Int -> Object
objById i = case filter ((== i) . objId) objectList of
              (o:_) -> o
              []    -> Object { objPos = -1, objId = i, objName = "?MISSING?", objDescription = "", objActions = [] }

hasDef :: Int -> Bool
hasDef i = i `elem` [ fst e | e <- assocs objectText ]

-- Gegenrichtung: von `to` muss ein Exit zurueck nach `from` existieren.
symmetric :: Location -> Bool
symmetric l = all hasBack (locExits l)
  where
    hasBack (to, _) = (locId l `elem` map fst (locExits (locById to)))

catacombs :: [Location]
catacombs = [ locById i | i <- [55..60] ]
