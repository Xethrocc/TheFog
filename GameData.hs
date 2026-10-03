module GameData where

import Data.Array
import Data.List (intercalate)
import GameTypes

-- ---------------------------------------------------------------------------
-- Locations
-- ---------------------------------------------------------------------------
-- NOTE (C6): "Errorfield" (locId 5) ist aus der Liste entfernt, weil es von
-- nirgends referenziert wurde. Die `descr`-Definition bei Index 5 MUSS
-- trotzdem bestehen bleiben, sonst laeuft das Erzeugen des Arrays auf.
--
-- WICHTIG: `gameMap` wird ueber `locById` (GameUtils) nach `locId` gesucht,
-- NICHT positionell. Sonst wuerde das Entfernen eines Eintrags die
-- Location-IDs aller nachfolgenden Raeume verschieben.
--
-- Die Katakomben (55-60) haengen am "Dark Room" (30), in dem auch das
-- Schwert liegt. Sie liegen komplett abseits der vier Schreine.
-- ---------------------------------------------------------------------------
gameMap :: Map
gameMap =
 [ Location {locId = 0, locName = "Home", locExits = [(1,N),(4,S)]}
 , Location {locId = 1, locName = "Garden", locExits = [(0,S),(2,N)]}
 , Location {locId = 2, locName = "Path", locExits = [(1,S),(3,N),(31,O),(39,W)]}
 , Location {locId = 3, locName = "Path", locExits = [(6,N),(2,S)]}
 , Location {locId = 4, locName = "Basement", locExits = [(0,N)]}
 , Location {locId = 6, locName = "Path", locExits = [(12,N),(7,O),(8,W),(3,S)]}
 , Location {locId = 7, locName = "Barn", locExits = [(6,W)]}
 , Location {locId = 8, locName = "Graveyard", locExits = [(11,N),(9,W),(6,O)]}
 , Location {locId = 9, locName = "Graveyard", locExits = [(10,N),(8,O)]}
 , Location {locId = 10, locName = "Graveyard", locExits = [(9,S),(11,O),(29,SE)]}
 , Location {locId = 11, locName = "Graveyard", locExits = [(10,W),(8,S)]}
 , Location {locId = 12, locName = "Forest", locExits = [(13,N),(6,S)]}
 , Location {locId = 13, locName = "Forest", locExits = [(12,S),(14,O),(15,W)]}
 , Location {locId = 14, locName = "Forest", locExits = [(13,W),(18,N)]}
 , Location {locId = 15, locName = "Forest", locExits = [(13,O),(16,N)]}
 , Location {locId = 16, locName = "Forest", locExits = [(21,N),(15,S)]}
 , Location {locId = 17, locName = "Forest", locExits = [(20,N),(28,SE)]}
 , Location {locId = 18, locName = "Forest", locExits = [(19,N),(14,S)]}
 , Location {locId = 19, locName = "Forest", locExits = [(20,W),(18,S),(24,O)]}
 , Location {locId = 20, locName = "Forest", locExits = [(23,N),(19,O),(17,S),(21,W)]}
 , Location {locId = 21, locName = "Forest", locExits = [(20,O),(22,W),(16,S)]}
 , Location {locId = 22, locName = "Forest", locExits = [(21,O)]}
 , Location {locId = 23, locName = "Glade", locExits = [(20,S)]}
 , Location {locId = 24, locName = "Forest", locExits = [(25,N),(19,W)]}
 , Location {locId = 25, locName = "Forest", locExits = [(26,N),(24,S)]}
 , Location {locId = 26, locName = "Forest", locExits = [(27,N),(25,S)]}
 , Location {locId = 27, locName = "Glade", locExits = [(26,S)]}
 , Location {locId = 28, locName = "Earth Shrine", locExits = [(17,S)]}
 , Location {locId = 29, locName = "Secret Grave Entrance", locExits = [(10,O),(30,W)]}
 , Location {locId = 30, locName = "Dark Room", locExits = [(29,O),(55,S)]}
 , Location {locId = 31, locName = "Path", locExits = [(2,W),(32,O)]}
 , Location {locId = 32, locName = "Path", locExits = [(31,W),(34,O),(33,S)]}
 , Location {locId = 33, locName = "River", locExits = [(32,N),(38,SE)]}
 , Location {locId = 34, locName = "Bridge", locExits = [(32,W),(35,O)]}
 , Location {locId = 35, locName = "Courtyard of the Castle", locExits = [(34,W),(36,O)]}
 , Location {locId = 36, locName = "Entrance Hall", locExits = [(35,W),(37,O)]}
 , Location {locId = 37, locName = "Throne Room", locExits = [(36,W)]}
 , Location {locId = 38, locName = "Water Shrine", locExits = [(33,N)]}
 , Location {locId = 39, locName = "Path", locExits = [(2,O),(40,W)]}
 , Location {locId = 40, locName = "Path", locExits = [(39,O),(41,W),(46,S)]}
 , Location {locId = 41, locName = "Path", locExits = [(40,O),(42,W)]}
 , Location {locId = 42, locName = "Stairs", locExits = [(41,O),(43,W)]}
 , Location {locId = 43, locName = "Stairs", locExits = [(42,O),(44,W)]}
 , Location {locId = 44, locName = "Top Of The Mountain", locExits = [(43,O),(45,SE)]}
 , Location {locId = 45, locName = "Fire Shrine", locExits = [(44,S)]}
 , Location {locId = 46, locName = "Path", locExits = [(40,N),(47,O),(48,W),(49,S)]}
 , Location {locId = 47, locName = "Dead End", locExits = [(46,W)]}
 , Location {locId = 48, locName = "Dead End", locExits = [(46,O)]}
 , Location {locId = 49, locName = "Canyon", locExits = [(46,N),(50,SE)]}
 , Location {locId = 50, locName = "Flying Stones", locExits = [(49,N),(51,SE)]}
 , Location {locId = 51, locName = "Flying Stones", locExits = [(50,N),(52,S)]}
 , Location {locId = 52, locName = "Air Shrine", locExits = [(49,N)]}
 , Location {locId = 53, locName = "Start Menue", locExits = [(0,SE)]}
 , Location {locId = 54, locName = "Wolf-Fight", locExits = []}
 -- Die Katakomben unter dem "Dark Room"
 , Location {locId = 55, locName = "Lower Stair", locExits = [(30,N),(56,O)]}
 , Location {locId = 56, locName = "Ossuary", locExits = [(55,O),(57,S)]}
 , Location {locId = 57, locName = "Flooded Tunnel", locExits = [(56,N),(58,S)]}
 , Location {locId = 58, locName = "Crypt of the Forgotten", locExits = [(57,N),(59,O)]}
 , Location {locId = 59, locName = "Offering Chamber", locExits = [(58,W),(60,S)]}
 , Location {locId = 60, locName = "Sealed Vault", locExits = [(59,N)]}
 ]

-- ---------------------------------------------------------------------------
-- Woelfe
-- ---------------------------------------------------------------------------
-- WICHTIG: Woelfe-IDs liegen bewusst in 40-46, Objekt-IDs in 0-31. Vorher
-- teilten sie sich den Nummernraum, mit der Folge, dass `examine Wolf
-- (Guardian)` den Text von Object 0 (Paper = die Prophezeiung) ausgab -
-- `objectTextFor` schlug ueber wolfId nach und die beiden Listen kollidier-
-- ten bei 0, 1, 2 und 3.
--
-- A5: Der Tutorial-Wolf ist jetzt eine echte Entitaet (ID 44, Loc 3) statt
-- eines statischen Objekts. Damit matcht "attack wolf" in Loc 3 ihn und
-- nicht mehr den Guardian in Loc 28 (Befund 8).
--
-- A5: Die drei Patrouillenwoelfe sind nicht mehr identisch - leicht
-- differenzierte Werte, damit sie nicht austauschbar wirken.
--
-- C3: Castle Guard (ID 45, Loc 36) macht `descr 36` ("many guards in front
-- of it") endlich wahr - der Text bleibt unangetastet, statt ihn zu
-- entschaerfen.
--
-- NEU: Crypt Warden (ID 46, Loc 58) bewacht den letzten Katakombenraum.
--
-- Ein leerer wolfPath bedeutet: steht still. Das nutzen die Nebelwoelfe
-- (IDs 100-107), die statische Gefahren an ihren Spawn-Orten sind.
-- ---------------------------------------------------------------------------
initialWolves :: [Wolf]
initialWolves =
  [ Wolf 40 28 []                              0 True  30  8  2 False  -- Guardian, Earth Shrine
  , Wolf 41 13 [13,15,16,21,20,19,18,14]       0 False 12  8  1 False
  , Wolf 42 17 [17,20,21,16,15,13,14,18,19,20] 0 False 12  9  1 False
  , Wolf 43 23 [23,20,19,18,14,13,15,16,21,20] 0 False 12  7  2 False
  , Wolf 44  3 []                              0 False 12  8  1 False  -- Tutorial-Wolf auf dem Weg
  , Wolf 45 36 []                              0 False 20  9  2 False  -- Castle Guard
  , Wolf 46 58 []                              0 False 24 10  3 False  -- Crypt Warden
  ]

-- ---------------------------------------------------------------------------
-- Wolfs-IDs (trennen den Wolfs- vom Objekt-Nummernraum, siehe oben)
-- ---------------------------------------------------------------------------
guardianWolfId, tutorialWolfId, castleGuardId, cryptWardenId :: ObjectID
guardianWolfId   = 40
tutorialWolfId   = 44
castleGuardId    = 45
cryptWardenId    = 46

-- Spawnorte der Nebelwoelfe (B2). Bewusst keiner davon liegt in den
-- Katakomben - die sind die einzige Zuflucht im Todesmarsch.
postFogWolfLocations :: [Int]
postFogWolfLocations = [0, 6, 8, 9, 10, 33, 35, 36]

postFogWolfIdBase :: Int
postFogWolfIdBase = 100

initialShrineFlags :: (Bool, Bool, Bool, Bool)
initialShrineFlags = (False, False, False, False)

initialPrincessStatus :: PrincessStatus
initialPrincessStatus = PrincessAlive

-- ---------------------------------------------------------------------------
-- Objekte
-- ---------------------------------------------------------------------------
-- A5: Statisches Objekt "Wolf" (objId 10) entfernt - der Wolf ist jetzt eine
--      Entitaet mit ID 44. Die `objectText`-Eintraege 10 und 12 bleiben
--      trotzdem belegt: `objectText` ist ein dichtes Array, ein fehlender
--      Index waere ein undefiniertes Element und wuerde `!` zur Laufzeit
--      werfen.
-- C5: Phantom-Objekt 12 (objName = "") entfernt. `objectText`-Eintrag 12
--      bleibt aus Array-Bounds-Gruenden bestehen.
-- C3/C1/C2/C4: Neuer Content, ausschliesslich IDs >= 18.
-- C9: "burn" ist ein No-op - deshalb aus den objActions entfernt, statt den
--      Help-Text zu luegen.
--
-- "help" ist nicht mehr in den objActions der Prinzessin (Befund 9):
-- `help Princess` hat dadurch einen Rettungswurf ausgeloest.
-- ---------------------------------------------------------------------------
objectList :: ObjectList
objectList =
       [
       Object {objPos = 0, objId = 0, objName = "Paper", objDescription = "A note with a prophecy for this Land", objActions = ["read","examine"]},
       Object {objPos = 0, objId = 1, objName = "Map", objDescription = "It's a simple map, you can 'read' it.", objActions = ["read","examine"]},
       Object {objPos = 1, objId = 2, objName = "Apple", objDescription = "It's an Apple from your Appletree", objActions = ["examine","take","pick"]},
       Object {objPos = 4, objId = 3, objName = "Shield", objDescription = "It's a shield", objActions = ["examine","take","pick"]},
       Object {objPos = 4, objId = 4, objName = "Crystal", objDescription = "It's a slightly glowing Crystal", objActions = ["examine","take","pick"]},
       Object {objPos = 38, objId = 5, objName = "Water-Shrine", objDescription = "looks like a stone fountain", objActions = ["examine","activate"]},
       Object {objPos = 45, objId = 6, objName = "Fire-Shrine", objDescription = "looks like a burning pillar", objActions = ["examine","activate"]},
       Object {objPos = 52, objId = 7, objName = "Air-Shrine", objDescription = "A big Shrine made of marble", objActions = ["examine","activate"]},
       Object {objPos = 28, objId = 8, objName = "Earth-Shrine", objDescription = "looks like a simple shrine made of wood", objActions = ["examine","activate"]},
       Object {objPos = 30, objId = 9, objName = "Sword", objDescription = "The Sword Of A Thousand Truths", objActions = ["examine","take"]},
       Object {objPos = 33, objId = 11, objName = "Princess", objDescription = "Looks like she fell in the river.. Try to 'save' her", objActions = ["examine","save"]},
       Object {objPos = 53, objId = 13, objName = "Help", objDescription = "This is the Help", objActions = ["read","examine"]},
       Object {objPos = 99, objId = 14, objName = "activated-Water-Shrine", objDescription = "It's the activated Water Shrine", objActions = ["examine","activate"]},
       Object {objPos = 99, objId = 15, objName = "activated-Fire-Shrine", objDescription = "It's the activated Fire Shrine", objActions = ["examine","activate"]},
       Object {objPos = 99, objId = 16, objName = "activated-Air-Shrine", objDescription = "It's the activated Air Shrine", objActions = ["examine","activate"]},
       Object {objPos = 99, objId = 17, objName = "activated-Earth-Shrine", objDescription = "It's the activated Earth Shrine", objActions = ["examine"]},

       -- C1 Wald (12-27): Nahrung, die das angehobene Max-HP ausgleicht
       Object {objPos = 14, objId = 18, objName = "Berries", objDescription = "A handful of dark berries, still wet with dew", objActions = ["examine","take","pick"]},
       Object {objPos = 16, objId = 19, objName = "Roots", objDescription = "Some thick roots, dug out of the ground by something", objActions = ["examine","take","pick"]},
       Object {objPos = 22, objId = 20, objName = "Mushroom", objDescription = "A pale mushroom growing out of the moss", objActions = ["examine","take","pick"]},
       Object {objPos = 27, objId = 21, objName = "Honeycomb", objDescription = "A comb of dark honey, wedged into the bark", objActions = ["examine","take","pick"]},

       -- C2 Barn (7): Atmosphaere, kein Nachschub - `descr` 7 sagt ausdruecklich
       --    "probably nothing useful", und das soll nicht zur Luege werden.
       Object {objPos = 7, objId = 22, objName = "Rusty Horseshoe", objDescription = "A horseshoe, green with rust", objActions = ["examine","take","pick"]},
       Object {objPos = 7, objId = 23, objName = "Torn Blanket", objDescription = "A wool blanket, eaten through in places", objActions = ["examine","take","pick"]},

       -- C3 Castle (34-37): Throne Room
       Object {objPos = 37, objId = 24, objName = "Torn Banner", objDescription = "A banner from the castle wall, the crest worn away", objActions = ["examine","take","pick"]},
       Object {objPos = 37, objId = 25, objName = "Rusty Crown", objDescription = "A small crown. Nobody has worn it in a long time", objActions = ["examine","take","pick"]},

       -- C4 Dead Ends (47/48): zwei symmetrische Steintafeln, jetzt mit Inhalt
       Object {objPos = 47, objId = 26, objName = "Stone Tablet", objDescription = "A weathered stone tablet, covered in carved letters", objActions = ["read","examine","take","pick"]},
       Object {objPos = 48, objId = 27, objName = "Broken Tablet", objDescription = "A stone tablet, snapped cleanly in two", objActions = ["read","examine","take","pick"]},

       -- Katakomben (55-60)
       Object {objPos = 56, objId = 28, objName = "Pale Fungus", objDescription = "A fungus growing out of the bone dust", objActions = ["examine","take","pick"]},
       Object {objPos = 58, objId = 29, objName = "Tomb Tablet", objDescription = "A tablet propped against the tomb, face down", objActions = ["read","examine","take","pick"]},
       Object {objPos = 60, objId = 30, objName = "Sack of Grain", objDescription = "A sack of grain, packed and left to keep", objActions = ["examine","take","pick"]},
       Object {objPos = 60, objId = 31, objName = "Old Coin", objDescription = "A coin from a country nobody here has heard of", objActions = ["examine","take","pick"]}
       ]

-- C5b: Help liegt ab Spielstart im Inventar und ist damit ueberall lesbar.
-- Vorher steckte der einzige Help-Text auf dem Startmenue (53), das man
-- nach dem ersten Schritt nie wiedersehen konnte.
initialInventory :: Inventory
initialInventory = [ o | o <- objectList, objId o == 13 ]

-- ---------------------------------------------------------------------------
-- Objekt-Texte. Bounds (0,31): JEDER Index muss belegt sein, weil
-- `objectTextFor` sonst ein undefiniertes Array-Element trifft.
-- ---------------------------------------------------------------------------
objectText :: Array ObjectID String
objectText = array (0,31) [
       (0,"           .-.---------------------------------------.-.  "
        ++ "\n          ((o))                                         ) "
        ++ "\n           \\U/_________          _______         ______/  "
        ++ "\n             |                                        |  "
        ++ "\n             |                                        |  "
        ++ "\n             |             The Prophecy               |  "
        ++ "\n             |            ‾‾‾‾‾‾‾‾‾‾‾‾‾‾              |  "
        ++ "\n             |  'It was foretold that one day         |  "
        ++ "\n             |  a fog will set over this land,        |  "
        ++ "\n             |  it will bring evil on the people,     |  "
        ++ "\n             |  it will lure the monsters             |  "
        ++ "\n             |  out of the darkness                   |  "
        ++ "\n             |  and bring a century of destruction.   |  "
        ++ "\n             |                                        |  "
        ++ "\n             |  And that the hero who wields the      |  "
        ++ "\n             |  sword will turn it back.              |  "
        ++ "\n             |                                        |  "
        ++ "\n             |   The magic crystal can activate       |  "
        ++ "\n             |   the shrines of the four elements.    |  "
        ++ "\n             |                                        |  "
        ++ "\n             |_____    ________    ___  ______    ____|  "
        ++ "\n            /A\\                                        \\ "
        ++ "\n           ((o))                                        )"
        ++ "\n            '-'----------------------------------------' "),
       (1,"\n ____________________________________________________________________\n |    The map shows you the most important places in the country.   |\n |                                                                  |\n | The forests in the north, formerly called the forest of Elwyn.   |\n |      In the east, the Great Castle Minas..something.             |\n |               A Grand Canyon in the south.                       |\n |   In the west is a big mountain, it's called Mountain Doom.      |\n |                                                                  |\n '------------------------------------------------------------------'"
        ++ ("\n                                `:++++++//+++o`                     "
        ++ "\n                             .++/. `  ``  .yy+y                     "
        ++ "\n                           `s/                :/s:                  "
        ++ "\n                          `h.     --FOREST--    :s:                   "
        ++ "\n                          s:                      -o+-                "
        ++ "\n                         -y                         :+++/:.          "
        ++ "\n                        -d______    :` `-`                .:/++++/`   "
        ++ "\n                      `ooy Grave|   o                            `+o` "
        ++ "\n   ````./+++///////++os` | yard |`                                  `h`"
        ++ "\n    :++:`                |o`o-.s|    `              `      CASTLE   o:"
        ++ "\n  +o.   ____`           `|______|                                 .o+ "
        ++ "\n y:    /    \\`                   `                              :o/`  "
        ++ "\n`h    /      \\                               .~~~~~.       .:/++:     "
        ++ "\n y`  /  MOUNTAIN          `   |-----|    `  - RIVER -  .+++:`        "
        ++ "\n .s`/__________\\              |HOUSE|        '~~~~~' :++:             "
        ++ "\n  ++                          |_____|   ``        +o.               "
        ++ "\n   /o`                       `                   o+                   "
        ++ "\n    .o/                                   `    .s:                    "
        ++ "\n      -s-                            ```     .o+                     "
        ++ "\n        :o/                    `##########+o+`                     "
        ++ "\n          .+o.        #####--CANYON--#### /s`                        "
        ++ "\n             /+++###############:.       -s:                           "
        ++ "\n                ./s.```               `o+                            "
        ++ "\n                  `//+o-             :s.  `                           "
        ++ "\n                       /y`        `+o-                                "
        ++ "\n                        `/++:--//+/`                                  "
        ++ "\n                            `..`                                      ")),
       (2,"It's an Apple from your Appletree. A healthy snack for the road."),
       (3,"A simple wooden shield. If you 'take' it, it might protect you."),
       (4,"A slightly glowing Crystal. According to the prophecy it can activate the shrines of the four elements."),
       (5,"A stone fountain, the Water-Shrine. The prophecy says the magic Crystal can 'activate' it.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (6,"A burning pillar, the Fire-Shrine. The prophecy says the magic Crystal can 'activate' it.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (7,"A big Shrine made of marble, the Air-Shrine. The prophecy says the magic Crystal can 'activate' it.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (8,"A simple shrine made of wood, the Earth-Shrine. The prophecy says the magic Crystal can 'activate' it.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (9,"The Sword Of A Thousand Truths. It radiates an old and powerful magic."),
       (10,"A dark Wolf with glowing eyes. It watches your every move. Maybe you can 'attack' him.."),
       (11,"The Princess of the castle! She fell into the river and is about to drown. Try to 'save' her!"),
       (12,"Nothing special to see here."),
       (13,"\n_____________________________________________________________________\n|               ---This is The Help Section---                      |\n| To move your Character type n,e,s,w or north, east, south, west   |\n| To use Objects type *Type of use* + Object for example 'read Help'|\n| Some objects have a special kind of use, some are just there.     |\n|                                                                   |\n| Possible Actions: examine, read, take, attack/hit, save, help,     |\n|                  activate, show Inventory, flee                    |\n|                                                                   |\n| Watch out. Time is not free here. Every step you take, and every   |\n| round you fight, the world moves on without you.                   |\n|___________________________________________________________________|"),
       (14,"The Water-Shrine is activated. Water flows calmly over its rim.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (15,"The Fire-Shrine is activated. Its flames burn steady and bright.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (16,"The Air-Shrine is activated. A soft wind swirls around it.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (17,"The Earth-Shrine is activated. The ground beneath it hums gently.\nCut into the base, worn but still readable: 'Four that wake, one that comes.'"),
       (18,"A handful of dark berries. Small, and worth more than they look."),
       (19,"Thick roots, dug up by something with claws. Edible, if you are desperate."),
       (20,"A pale mushroom out of the moss. It smells faintly of the rain."),
       (21,"A comb of dark honey. Still sealed, so nothing has been at it."),
       (22,"A horseshoe gone green with rust. Somebody nailed it up here years ago and never came back for it."),
       (23,"A wool blanket, eaten through in places. It smells of the hay it was stored under."),
       (24,"A banner from the castle wall. The crest has worn down to nothing, but the cloth is heavy."),
       (25,"A small crown. Somebody kept it polished for a very long time before they stopped."),
       (26,"The letters are worn almost smooth, but you can still make out a list of names.\nThe last name on the list is the same as the one on the castle banner in the Throne Room.\nBelow the names, cut deeper than the rest, someone has added a single line:\n  'He was never asked to come back.'"),
       (27,"The tablet is snapped in two. The surviving half carries a date, and one word:\n  'here'"),
       (28,"A fungus out of the bone dust. It gives off a faint, cold light."),
       (29,"A tablet propped against the tomb, face down, as if someone did not want to read it.\nThe dust around it is disturbed.\nSomeone turned it over, read it, and turned it back."),
       (30,"A sack of grain, packed and left to keep. The rats went through the top of it and no further.\nUnderneath, the grain is dry. Someone was preparing for a long wait."),
       (31,"A coin from a country nobody here has heard of. The face on it has been worn smooth by somebody's thumb.")
       ]

-- ---------------------------------------------------------------------------
-- Ortsbeschreibungen. Bounds (0,60) - Index 5 MUSS bestehen bleiben,
-- obwohl es keine Location mehr gibt (siehe gameMap).
-- ---------------------------------------------------------------------------
descr :: Array Int String
descr = array (0,60) [
       (0,"You are at Home, there is a Forge, a Table with a Candle on top. \nAt North is the Entrance, to the South there's flap that leads to your basement."),
       (1,"Your are in your garden, some flowers and an apple Tree are here. \nAt the Northern site is a Way to a street,\nto the South is your House"),
       (2,"You are on an old street, You can go to North, East, West. \nTo the South is Your garden. There is a post with a few signs pointing to the west, north and east."),
       (3,"Your are on a Street. You can go to North or South. In the North you can see many treetops"),
       (4,"You are in your Basement, there are shelves of potatoes, apples, pickles and all kinds of stuff you do not need right now. \nThere's a chest in the corner, maybe you could use that."),
       (5,"You are in an Error"),
       (6,"You are on a road, you can go north, east, south and west. In the north you can see a path in the giant forest. \nIn the east is a small hut, it seems no one has been there for a long time. \nIn the south is a road, somewhere in the direction is your house. In the west you see an old rotten metal gate, looks like a cemetery."),
       (7,"You are standing in front of a small abandoned barn, no one is here, probably nothing useful. \nYou can go back to West"),
       (8,"You are at the entrance of the cemetery, you can go north, east and west. \nIn the north are a few old spooky trees and dilapidated tombs. \nIt looks the same in the West. \nIn the east you come to the street and out of the creepy graveyard."),
       (9,"You are standing in the middle of the cemetery, surrounded by dilapidated tombs and spooky trees. You can go north and east. \nEastwards is the entrance of the cemetery. \nTo the north is a large mausoleum. Disintegrated, like the rest here."),
       (10,"They are standing in front of a ruined mausoleum. The entrance is open. You can go east and south. \nIn the East, there are only graves and all that scary stuff. \nIt looks the same in the south. But the tomb is still there, maybe you can ¸Raid the Tomb¸."),
       (11,"You're still in the cemetery, it's creepy. It goes to the west and south. \nIn the west is a big old tomb. In the south is the entrance."),
       (12,"You are in the woods. You still see the light appearing between the treetops and trunks. \nYou can head north, deeper into the forest, or south, onto the road."),
       (13,"You are in the middle of the forest, it is dark and you hear strange noises. \nYou can go east, south and west. \nIn the south you can see the end of the forest. \nThe other directions only lead deeper into the forest."),
       (14,"It is dark, wet and cold. And that owl above you is staring at you all the time. \nYou can go north and west. There is forest everywhere."),
       (15,"It is dark, wet and cold. And that owl above you is staring at you all the time. \nYou can go north and East. There is forest everywhere."),
       (16,"Forest, only forest and hardly any light. You hear how some of the animals come closer. \nIt goes to the north and south. In both directions is forest."),
       (17,"You are standing in the middle of a clearing. \nThe forest surrounds you from all sides. \nYou hear strange noises as if wind blows through a cave."),
       (18,"Forest, only forest and hardly any light. You hear how some of the animals come closer. \nIt goes to the north and south. In both directions is forest."),
       (19,"Forest. It goes to the east, south and west. Everything looks the same."),
       (20,"Forest .. It goes to the north, east, south and west. In the north and south you see some light, otherwise there is forest everywhere. how surprising."),
       (21,"If you want to know, you are in the forest. It goes to the east, south and west. In all directions are only trees, shrubs and wild animals."),
       (22,"Forest, wild animals and you are in the middle of a dead end, great. You can only go east."),
       (23,"A clearing, but also a dead end, it goes only south again into the forest."),
       (24,"Forest, forest and forest. You can go north and west."),
       (25,"You're still in the woods. It goes to the north and south."),
       (26,"You're still in the woods. It goes to the north and south. In the south it looks like there's some sunlight."),
       (27,"You are standing in a clearing, the sunlight is bright and warm. \nIn the middle of the clearing stands a huge old tree. \nA sign in front of it says something about his age and a name. \nYou can not see much. Something about tree ... beard .. Ent. \nHead south to the Forest, the only way."),
       (28,"In the middle of the forest stands a large shining shrine. You do not hear animals and other things, not even the leaves in the wind. \n~You can not take your eyes off the shrine.~ \nThe only way goes back to the south."),
       (29,"You are in a grave, great.. Around you are tombs, dead and cobwebs. \nThe wind howls through the whole vault. You can go west and east. \nHeading east, it goes back up. In the west, it goes even deeper into the creepy tomb."),
       (30,"You are even deeper in the grave. It's wet, cold and dark. You can just feel where you are going. \nYour whole body is wet and full of cobwebs. You have lost your orientation. \n~Was it a good idea to go here?~ \nThere is a stair in the floor, going down."),
       (31,"You are on a simple street. It goes east and west. \nThe road continues westwards. \nFar to the east you can see the battlements of a large castle."),
       (32,"You are on a street. It goes to the east and south. \nIn the east there is a drawbridge which leads to a huge castle. \nIn the south is a river."),
       (33,"You are standing in front of a big river with a Waterfall, it is very wide and you hear the loud noise of the water. \n Theres something like a cave behind the River, maybe you can 'swim' to it \nYou can not cross it, it goes north to the road."),
       (34,"You are standing on a huge drawbridge, in front of you is a huge stone castle with large towers. \nThe gate to the courtyard is open, two guards guard the entrance. \nYou can go east, to the yard or west to the street."),
       (35,"You are standing in the middle of the castle courtyard. \nTo the east is the entrance to the castle and to the south is the drawbridge."),
       (36,"You are in the entrance hall of the castle. \nTo the east is the entrance to the throne room, but there are many guards in front of it. \nWest leads to the courtyard."),
       (37,"You are standing in the throne room. A huge throne made of swords. \nThe light shines in through the huge glass windows and illuminates the whole room."),
       (38,"You're in a dark cave the only source of light is a peculiarly bluish shrine. \nMaybe you can 'activate' it \nFar to the north is the cave entrance."),
       (39,"You are on a street. It goes to the east and to the north. \nOn both sides the road continues."),
       (40,"You are back on a road, going east, south and west. \nIn the south is flatland and far to the west you see a mountain."),
       (41,"You are on a path. \nIn the west you see endless steps leading up to a mountain. \nIn the east, the road continues."),
       (42,"You are halfway up the mountain, in front of you and behind you are still countless steps. \nHead east to the path. \nIn the west you continue up the stairs."),
       (43,"You are almost up on the mountain. \nIt goes down to the east. To the west is the summit of the mountain. \nBirds circle high in the sky, they scream in a weird way and it seems like they are watching you."),
       (44,"You are on the top of the mountain. a large lava lake extends in front of you. \nBehind a scree pile is a small entrance, maybe you can 'crawl' into it."),
       (45,"You are standing in a lava-lit room. \nIn front of you is a large stone pillar with glowing flames on it. It looks like it's hot.\nTo South is the exit"),
       (46,"You're back on the road. \nIt goes to the north, east, south and west. \nThere is a road in the north. \nit goes a bit further west and south, but it looks like the road ends there. \nThe plain ends in the south and becomes a huge gap in the earth. A hundred meter wide canyon."),
       (47,"You are at a dead end, in front of you is a weathered stone tablet. \nTo the west it goes back to the street."),
       (48,"You are at a dead end, in front of you is a weathered stone tablet. \nTo the east it goes back to the street."),
       (49,"You are standing right in front of the Giant Canyon, you can barely see the ground. \nIn the north is the road. In the south you can see a few floating stones directly above the canyon. \nTwo big ones float right at the canyon, right and left of you."),
       (50,"You are standing on a floating stone. \nIn the south are again two stones on which you could jump. \nHead north to get back to the path."),
       (51,"You are standing on a floating stone. Something like a floating island in the south. \nYou can also go back north, but the rocks look even more wobbly than before."),
       (52,"You are standing on a small floating island. in front of you is a marble shrine, a big symbol on it. \nIt looks like it changes with every gust of wind."),
       (53,"Welcome to ~~The Fog~~ To enter the game enter 'game' \n\n                    For a little help type 'read Help'"),
       (54,">>You are in a Fight!<<"),

       -- Die Katakomben unter dem "Dark Room"
       (55,"A stair of black stone, worn smooth by hands that are no longer here. The air that comes up is old and cold. \nTo the north is the way you came. \nThe stair carries on down to the east."),
       (56,"Row after row of niches, each one just big enough for a skull. Somebody stacked the dead here like firewood. \nEast is the stair. \nTo the south the floor drops away into a lower passage."),
       (57,"Water stands ankle deep and does not run anywhere. It has been still long enough to grow a pale skin over it. \nNorth back to the niches. \nThe tunnel carries on south, low enough that you have to stoop."),
       (58,"A single tomb, far larger than the niches, and the lid has been pushed aside from within. \nYou would rather it had not been. \nNorth to the water. East into the dark, where the air moves."),
       (59,"Things were left here once: bowls, coins, a child's shoe. The birds have been through all of it but the stone. \nWest is the tomb. \nA stair goes down to the south, and this one is newer than the rest."),
       (60,"A small room, shut tight enough that the fog has not found its way in here. \nSomebody was preparing for a long wait, and did not get far enough into it. \nThe only way out is north.")
       ]

-- ---------------------------------------------------------------------------
-- Nebel-Phase (B3/B4)
-- ---------------------------------------------------------------------------
-- `fogDescr` enthaelt nur die Sonderorte mit eigenem Text. Fehlt ein
-- Eintrag, erzeugt `locD` (GameUtils) den generischen Nebeltext aus
-- `describeExits`.
--
-- WICHTIG: bewusst eine Lookup-Liste und KEIN Array. Ein duennes Array
-- haette scheinbar funktioniert: `inRange (bounds fogDescr) 20` ist True,
-- `fogDescr ! 20` wirft aber "undefined array element". `inRange` prueft
-- nur die Bounds, nicht ob der Index belegt ist. `descr` ist dicht
-- (0..60) und damit unkritisch, `fogDescr` ist es nicht.
--
-- Loc 54 braucht zwingend einen Eintrag: `fightLoop` zeigt `locD` der
-- virtuellen Kampf-Location. Ohne ihn stuende im Nebel "no way out" da,
-- statt dem Kampf-Banner.
-- ---------------------------------------------------------------------------
fogDescr :: [(Int, String)]
fogDescr =
       [ (0, "You recognise your home all the same, though the dense fog wraps it. The candle is out."),
       (34, "The drawbridge is still there. Whether it still leads anywhere is a question the fog has taken away."),
       (35, "The courtyard. The shapes of the walls are the same as you remember, and that is all."),
       (36, "The entrance hall. The shape of the arch, and the cold coming off the stone."),
       (37, "The throne room. The throne is a dark shape made of many dark shapes, and you would rather not look at it closely."),
       (28, "The Earth-Shrine hums through the fog, low and constant. It is the only thing out here that still sounds like it is waiting for something."),
       (38, "The Water-Shrine glows bluish through the fog, weaker than before, as if the fog were drinking it."),
       (45, "The Fire-Shrine still burns, but the fog comes off it in thick grey sheets, and the light does not reach the walls."),
       (52, "The Air-Shrine is a pale shape in the fog, and the wind moves the symbol on it faster than it should."),
       (54, ">>You are in a Fight!<<"),
       (55, "The stair. Black stone, worn smooth, going down into a fog that has already got below you."),
       (56, "The niches. Fog sits in them in a slow drift, and it has taken the shape of what was stacked there."),
       (57, "The water is still exactly as deep as it was. The fog lies on it flat and unbroken and gives nothing away."),
       (58, "The tomb stands open. The fog pours out of it in a steady current, the way smoke pours out of a chimney."),
       (59, "The offerings are gone under it. All of them, evenly, as if the fog had counted them."),
       (60, "The vault is still shut tight. The fog presses against the doorway and does not come in.")
       ]

-- ---------------------------------------------------------------------------
-- B3: Richtungsnamen und Exit-Generator fuer den Nebeltext.
-- `SE` wird bewusst NICHT gelistet - es ist keine sichtbare Passage,
-- sondern verb-gesteuert (game/dig/crawl/swim).
-- ---------------------------------------------------------------------------
directionName :: Direction -> String
directionName N  = "North"
directionName O  = "East"
directionName S  = "South"
directionName W  = "West"
directionName SE = "South-East"

describeExits :: Location -> String
describeExits loc
    | null names = "no way out"
    | otherwise  = "the exits: [" ++ intercalate ", " names ++ "]"
  where
    names = exitNames loc

-- Nur die sichtbaren Richtungen. `SE` ist keine sichtbare Passage,
-- sondern verb-gesteuert (game/dig/crawl/swim).
exitNames :: Location -> [String]
exitNames loc = map directionName (map snd (filter (\(_, d) -> d /= SE) (locExits loc)))
