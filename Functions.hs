module Functions where

import Data.Array
import Data.List
import Data.Maybe
import System.Console.ANSI

data Direction = N|O|S|W|SE deriving (Eq,Show,Enum,Ord) -- Datentyp für Mögliche Richtungen N O S W und SE für spezielle Richtungen
type LocString = String
type Location  = (Int,LocString,[(Int,Direction)])
type Map       = [Location]

type PossibleActions = [String] 

type ObjectID   = Int
type ObjString  = String
type ObjDescri  = String
type Object     = (Int,ObjectID,ObjString,ObjDescri,PossibleActions)
type ObjectList = [Object]

type Life       = Int

type Inventory  = ObjectList


type Character = (String,Steps,Ang,Def,Life)

type Steps     = Int
type Ang       = Int
type Def       = Int

locS :: Location -> LocString -- Holt den Location String aus dem Location-Type
locS (a,location,xs) = location

locD :: Location -> String -- Holt die Beschreibung der Location aus dem Array descr
locD (a,location,xs) = descr! a

locI :: Direction -> Location -> Int -- Gibt den Index der nächsten Location für eine bestimmte Himmelsrichtung an, falls die Richtung nicht vorhanden ist, bleibt man an derselben Location.
locI x (a,location,[(d,e)]) = if (e == x) then d else a
locI x (a,location,(y:ys)) =  if (dir == x) then loc else locI x (a,location,(ys))
                              where 
                               (loc,dir) = y

-- Gibt den Int der Aktuellen Position aus 
locA :: Location -> Int
locA (a,location,xs) =  a

-- Gibt die Position des Objekts aus
getObjPos :: Object -> Int
getObjPos (pos,b,c,d,es) = pos

-- Gibt die Objekt-ID aus
getObjID :: Object -> ObjectID
getObjID (a,id,c,d,es) = id

-- Gibt den Namen des Objekts aus
getObjStr :: Object -> ObjString
getObjStr (a,b,string,d,es) = string

-- Gibt die Beschreibung des Objekts aus
getObjDes :: Object -> ObjDescri
getObjDes (a,b,c,description,es) = description

-- Gibt die Möglichen Aktionen aus
getObjActions :: Object -> PossibleActions
getObjActions (a,b,c,d,actions) = actions

-- Sagt ob eine Aktion auf dieses Objekt anwendbar ist
isActionPossible :: String -> Object -> Bool
isActionPossible x (a,b,c,d,actions) 
                        | x `elem` actions = True
                        | otherwise        = False

-- kleine Funktion zum splitten der String Eingaben --
splitString x = case break (==' ') x of
                (a, ' ':b) -> a : splitString b
                (a, "")    -> [a]

-- Sieht nach ob ein Objekt an einer Location ist
isObjHere :: Object -> Location -> Bool
isObjHere (a,b,c,d,e) (f,location,xs) = if (a==f) then True else False

getObjHere :: ObjectList -> Location -> String
getObjHere [] xs = ""
getObjHere (x:xs) loc = obj ++ (getObjHere xs loc)
                              where
                               obj = if (isObjHere x loc) then (getObjStr x) ++ "\n" else ""


-- Ein Objekt durch einen String finden
getObject :: String -> ObjectList -> Object
getObject x []     = (90,90,"","",[])
getObject x (y:ys) = if (x == string) then y else getObject x ys
                       where string = getObjStr y
                       

-- Den String aus Character bekommen 
getString :: Character -> String
getString (a,b,c,d,l) = a
-- Die Steps bekommen
getSteps ::  Character -> Steps
getSteps (a,b,c,d,l)  = b
-- Den Sngriffswert bekommen
getAtk ::    Character -> Ang
getAtk (a,b,c,d,l)    = c
-- Den Verteidigungswert bekommen
getDef ::    Character -> Def
getDef (a,b,c,d,l)    = d
-- Die Lebenspunkte bekommen
getHP ::    Character -> Life
getHP (a,b,c,d,l)    = l
-- Den Namen in einen leeren Character Schreiben
giveName :: String -> Character
giveName x = (x,0,1,1,10)
-- Addstep
addSteps :: Character -> Character
addSteps (a,b,c,d,l) = (a,b+1,c,d,l)
-- AddAngr u defense
addAng :: Inventory -> Character -> Character
addAng inv (a,b,c,d,l) = if "Sword" `elem`(map getObjStr inv) then (a,b,10,d,l) else (a,b,c,d,l)

addDef :: Inventory -> Character -> Character
addDef inv (a,b,c,d,l) = if "Shield" `elem`(map getObjStr inv) then (a,b,c,10,l) else (a,b,c,d,l)

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
                           
-- Prinzessin retten (oder auch nicht)
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

-- Hilfsfunktion damit bei Richtungseingaben nicht "Press 'Enter' to continue" angezeigt wird                        
ifThereIsAnyDirection :: String -> [String] -> IO()
ifThereIsAnyDirection x y = if (x `elem` y) 
                            then do putStrLn("") 
                            else do putStrLn("Press 'Enter' to continue.")
                                    enter <- getLine
                                    putStrLn("")

-- Sortierungsfunktionen

selsort :: (Eq a,Ord a) => [a] -> [a]
selsort [] = []
selsort xs = let 
               min  = minimum xs
               rest = delete min xs
             in
               min : selsort rest  

qsort :: (Eq a,Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = let
                min  = qsort [m | m <- (x:xs), m <  x]
                same =       [m | m <- (x:xs), m == x]
                max  = qsort [m | m <- (x:xs), m >  x] 
               in
                min ++ same ++ max   
                

-- PseudoRandom-Funktionen
randomInts = [8, 9, 1, 6, 10, 4, 4, 8, 10, 5, 10, 10, 2, 7, 8, 7, 4, 5, 8, 7, 8, 2, 7, 5, 4, 1, 3, 7, 5, 5, 3, 9, 10, 2, 7, 5, 3, 6, 4, 10, 9, 8, 8, 3, 9, 5, 1, 4, 3, 4, 1, 9, 2, 5, 4, 5, 7, 2, 5, 9, 3, 10, 1, 5, 1, 10, 7, 10, 3, 5, 5, 6, 4, 7, 9, 2, 7, 9, 1, 4, 6, 5, 4, 1, 8, 3, 10, 1, 2, 1, 7, 6, 1, 1, 5, 1, 7, 10, 9, 8, 7, 7, 1, 2, 3, 9, 5, 3, 3, 8, 7, 2, 6, 4, 1, 1, 7, 4, 3, 3, 3]
randomInts2 = [29, 12, 51, 42, 51, 15, 45, 28, 32, 12, 4, 25, 8, 30, 28, 40, 16, 27, 31, 32, 4, 38, 5, 44, 7, 32, 23, 45, 48, 32, 34, 36, 45, 52, 19, 33, 52, 16, 49, 24, 48, 42, 41, 29, 46, 4, 6, 39, 35, 43, 23, 34, 18, 42, 35, 46, 41, 17, 50, 52, 11, 15, 32, 9, 20, 34, 30, 49, 37, 40, 35, 14, 43, 9, 43, 47, 40, 46, 15, 15, 22, 37, 7, 21, 42, 28, 47, 16, 14, 21]

-- Einfache Random Zahl von 1-10
random :: Int -> Int
random x 
      | x <= 120 = randomInts !! x
      | x > 120  = random $ x-120

-- Erstellt ein neues Objekt an einer Zufallsposition      
randomObj :: Int -> ObjectList -> Object -> ObjectList
randomObj x oLs (pos,b,c,d,es) 
                   | x <= 120 = ((randomInts2 !! x),b,c,d,es):oLs
                   | x  > 120 = randomObj (x-90) oLs (pos,b,c,d,es)
                   
-- Ausgabe für Aktionen die auf random zugreifen
randText :: Int -> String -> IO()
randText int y = if ((random int) > 5) then putStrLn ("You "++ y ++"ed it") else putStrLn ("That was not working.. try again")
-----------------------------------------------------------------------

-- Die Liste aller Objekte die am Anfang eingelesen wird
objectList :: ObjectList
objectList = 
       [
       (0,0,"Paper","A note with a prophecy for this Land",["read","examine","burn"]),
       (0,1,"Map","It's a simple map, you can 'read' it.",["read","examine","burn"]),
       (1,2,"Apple","It's an Apple from your Appletree",["examine","take","pick"]),
       (4,3,"Shield","It's a shield",["examine","take","pick"]),
       (4,4,"Crystal","It's a slightly glowing Crystal",["examine","take","pick"]),
       (38,5,"Water-Shrine","looks like a stone fountain",["examine","activate"]),
       (45,6,"Fire-Shrine","looks like a burning pillar",["examine","activate"]),
       (52,7,"Air-Shrine","A big Shrine made of marble",["examine","activate"]),
       (28,8,"Earth-Shrine","looks like a simple shrine made of wood",["examine","activate"]),
       (30,9,"Sword","The Sword Of A Thousand Truths",["examine","take"]),
       (3,10,"Wolf","It's a dark Wolf, maybe you can 'attack' him..",["examine","attack","fight"]),
       (33,11,"Princess","Looks like she fell in the river.. Try to 'save' her",["examine","save","help"]),
       (0,12,"","",["examine"]),
       (53,13,"Help","This is the Help",["read","examine","burn"]),
       (99,14,"activated-Water-Shrine","It's the activated Water Shrine",["examine","activate"]),
       (99,15,"ativated-Fire-Shrine","It's the activated Fire Shrine",["examine","activate"]),
       (99,16,"ativated-Air-Shrine","It's the activated Air Shrine",["examine","activate"]),
       (99,17,"ativated-Earth-Shrine","It's the activated Earth Shrine",["examine"])
       
       
       ]
       
-- Die Beschreibungstexte, falls man ein Objekt lesen kann      
objectText :: Array (ObjectID) String 
objectText = array (0,13) [
      
      ((0),(("           .-.---------------------------------------.-.  "
       ++ "\n          ((o))                                         ) "
       ++ "\n           \\U/_________          _______         ______/  "
       ++ "\n             |                                        |  "
       ++ "\n             |                                        |  "
       ++ "\n             |             The Prophecy               |  "
       ++ "\n             |            ‾‾‾‾‾‾‾‾‾‾‾‾‾‾              |  "
       ++ "\n             |    'It was foretold, that one day,     |  "
       ++ "\n             |      heroes who could wield the        |  "
       ++ "\n             |     sword might reveal themselves.'    |  "
       ++ "\n             |                                        |  "
       ++ "\n             |    A fog will set over this land,      |  "
       ++ "\n             |    it will bring evil on the people,   |  "
       ++ "\n             |    it will lure the monsters           |  "
       ++ "\n             |    out of the darkness                 |  "
       ++ "\n             |    and bring a century of destruction. |  "
       ++ "\n             |                                        |  "
       ++ "\n             |     The magic crystal can activate     |  "
       ++ "\n             |     the shrines of the four elements.  |  "
       ++ "\n             |                                        |  "
       ++ "\n             |_____    ________    ___  ______    ____|  "
       ++ "\n            /A\\                                        \\ "
       ++ "\n           ((o))                                        )"
       ++ "\n            '-'----------------------------------------' "))),
      ((1),("      \n ____________________________________________________________________\n |    The map shows you the most important places in the country.   |\n |                                                                  |\n | The forests in the north, formerly called the forest of Elwyn.   |\n |      In the east, the Great Castle Minas..something.             |\n |               A Grand Canyon in the south.                       |\n |   In the west is a big mountain, it's called Mountain Doom.      |\n |                                                                  |\n '------------------------------------------------------------------'"++(("\n                                `:++++++//+++o`                     "
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
       ++ "\n                            `..`                                      ")))),
      ((13),"\n_____________________________________________________________________\n|               ---This is The Help Section---                      |\n| To move your Character type n,e,s,w or north, east, south, west   |\n| To use Objects type *Type of use* + Object for example 'read Help'| \n| Some objects have a special kind of use, some are just there.     |\n|                                                                   |\n| Possible Actions: examine, read, take, burn, attack/hit, show ... |\n|___________________________________________________________________|")
      
 ]


descr :: Array (Int) String 
descr = array (0,54) distances where
      distances = [
              ((0),"You are at Home, there is a Forge, a Table with a Candle on top. \nAt North is the Entrance, to the South there's flap that leads to your basement."),
              ((1),"Your are in your garden, some flowers and an apple Tree are here. \nAt the Northern site is a Way to a street,\nto the South is your House"),
              ((2),"You are on an old street, You can go to North, East, West. \nTo the South is Your garden. There is a post with a few signs pointing to the west, north and east."),
              ((3),"Your are on a Street. You can go to North or South. In the North you can see many treetops"),
              ((4),"You are in your Basement, there are shelves of potatoes, apples, pickles and all kinds of stuff you do not need right now. \nThere's a chest in the corner, maybe you could use that."),
              ((5),"You are in an Error"),
              ((6),"You are on a road, you can go north, east, south and west. In the north you can see a path in the giant forest. \nIn the east is a small hut, it seems no one has been there for a long time. \nIn the south is a road, somewhere in the direction is your house. In the west you see an old rotten metal gate, looks like a cemetery."),
              ((7),"You are standing in front of a small abandoned barn, no one is here, probably nothing useful. \nYou can go back to West"),
              ((8),"You are at the entrance of the cemetery, you can go north, east and west. \nIn the north are a few old spooky trees and dilapidated tombs. \nIt looks the same in the West. \nIn the east you come to the street and out of the creepy graveyard."),
              ((9),"You are standing in the middle of the cemetery, surrounded by dilapidated tombs and spooky trees. You can go north and east. \nEastwards is the entrance of the cemetery. \nTo the north is a large mausoleum. Disintegrated, like the rest here."),
              ((10),"They are standing in front of a ruined mausoleum. The entrance is open. You can go east and south. \nIn the East, there are only graves and all that scary stuff. \nIt looks the same in the south. But the tomb is still there, maybe you can ¸Raid the Tomb¸."),
              ((11),"You're still in the cemetery, it's creepy. It goes to the west and south. \nIn the west is a big old tomb. In the south is the entrance."),
              ((12),"You are in the woods. You still see the light appearing between the treetops and trunks. \nYou can head north, deeper into the forest, or south, onto the road."),
              ((13),"You are in the middle of the forest, it is dark and you hear strange noises. \nYou can go east, south and west. \nIn the south you can see the end of the forest. \nThe other directions only lead deeper into the forest."),
              ((14),"It is dark, wet and cold. And that owl above you is staring at you all the time. \nYou can go north and west. There is forest everywhere."),
              ((15),"It is dark, wet and cold. And that owl above you is staring at you all the time. \nYou can go north and East. There is forest everywhere."),
              ((16),"Forest, only forest and hardly any light. You hear how some of the animals come closer. \nIt goes to the north and south. In both directions is forest."),
              ((17),"You are standing in the middle of a clearing. \nThe forest surrounds you from all sides. \nYou hear strange noises as if wind blows through a cave."),
              ((18),"Forest, only forest and hardly any light. You hear how some of the animals come closer. \nIt goes to the north and south. In both directions is forest."),
              ((19),"Forest. It goes to the east, south and west. Everything looks the same."),
              ((20),"Forest .. It goes to the north, east, south and west. In the north and south you see some light, otherwise there is forest everywhere. how surprising."),
              ((21),"If you want to know, you are in the forest. It goes to the east, south and west. In all directions are only trees, shrubs and wild animals."),
              ((22),"Forest, wild animals and you are in the middle of a dead end, great. You can only go east."),
              ((23),"A clearing, but also a dead end, it goes only south again into the forest."),
              ((24),"Forest, forest and forest. You can go north and west."),
              ((25),"You're still in the woods. It goes to the north and south."),
              ((26),"You're still in the woods. It goes to the north and south. In the south it looks like there's some sunlight."),
              ((27),"You are standing in a clearing, the sunlight is bright and warm. \nIn the middle of the clearing stands a huge old tree. \nA sign in front of it says something about his age and a name. \nYou can not see much. Something about tree ... beard .. Ent. \nHead south to the Forest, the only way."),
              ((28),"In the middle of the forest stands a large shining shrine. You do not hear animals and other things, not even the leaves in the wind. \n~You can not take your eyes off the shrine.~ \nThe only way goes back to the south."),
              ((29),"You are in a grave, great.. Around you are tombs, dead and cobwebs. \nThe wind howls through the whole vault. You can go west and east. \nHeading east, it goes back up. In the west, it goes even deeper into the creepy tomb."),
              ((30),"You are even deeper in the grave. It's wet, cold and dark. You can just feel where you are going. \nYour whole body is wet and full of cobwebs. You have lost your orientation. \n~Was it a good idea to go here?~ \n"),
              ((31),"You are on a simple street. It goes east and west. \nThe road continues westwards. \nFar to the east you can see the battlements of a large castle."),
              ((32),"You are on a street. It goes to the east and south. \nIn the east there is a drawbridge which leads to a huge castle. \nIn the south is a river."),
              ((33),"You are standing in front of a big river with a Waterfall, it is very wide and you hear the loud noise of the water. \n Theres something like a cave behind the River, maybe you can 'swim' to it \nYou can not cross it, it goes north to the road."),
              ((34),"You are standing on a huge drawbridge, in front of you is a huge stone castle with large towers. \nThe gate to the courtyard is open, two guards guard the entrance. \nYou can go east, to the yard or west to the street."),
              ((35),"You are standing in the middle of the castle courtyard. \nTo the east is the entrance to the castle and to the south is the drawbridge."),
              ((36),"You are in the entrance hall of the castle. \nTo the east is the entrance to the throne room, but there are many guards in front of it. \nWest leads to the courtyard."),
              ((37),"You are standing in the throne room. A huge throne made of swords. \nThe light shines in through the huge glass windows and illuminates the whole room."),
              ((38),"You're in a dark cave the only source of light is a peculiarly bluish shrine. \nMaybe you can 'activate' it \nFar to the north is the cave entrance."),
              ((39),"You are on a street. It goes to the east and to the north. \nOn both sides the road continues."),
              ((40),"You are back on a road, going east, south and west. \nIn the south is flatland and far to the west you see a mountain."),
              ((41),"You are on a path. \nIn the west you see endless steps leading up to a mountain. \nIn the east, the road continues."),
              ((42),"You are halfway up the mountain, in front of you and behind you are still countless steps. \nHead east to the path. \nIn the west you continue up the stairs."),
              ((43),"You are almost up on the mountain. \nIt goes down to the east. To the west is the summit of the mountain. \nBirds circle high in the sky, they scream in a weird way and it seems like they are watching you."),
              ((44),"You are on the top of the mountain. a large lava lake extends in front of you. \nBehind a scree pile is a small entrance, maybe you can 'crawl' into it."),
              ((45),"You are standing in a lava-lit room. \nIn front of you is a large stone pillar with glowing flames on it. It looks like it's hot.\nTo South is the exit"),
              ((46),"You're back on the road. \nIt goes to the north, east, south and west. \nThere is a road in the north. \nit goes a bit further west and south, but it looks like the road ends there. \nThe plain ends in the south and becomes a huge gap in the earth. A hundred meter wide canyon."),
              ((47),"You are at a dead end, in front of you is a weathered stone tablet. \nTo the west it goes back to the street."),
              ((48),"You are at a dead end, in front of you is a weathered stone tablet. \nTo the east it goes back to the street."),
              ((49),"You are standing right in front of the Giant Canyon, you can barely see the ground. \nIn the north is the road. In the south you can see a few floating stones directly above the canyon. \nTwo big ones float right at the canyon, right and left of you."),
              ((50),"You are standing on a floating stone. \nIn the south are again two stones on which you could jump. \nHead north to get back to the path."),
              ((51),"You are standing on a floating stone. Something like a floating island in the south. \nYou can also go back north, but the rocks look even more wobbly than before."),
              ((52),"You are standing on a small floating island. in front of you is a marble shrine, a big symbol on it. \nIt looks like it changes with every gust of wind."),
              ((53),"Welcome to ~~The Fog~~ To enter the game enter 'game' \n\n                    For a little help type 'read Help'"),
              ((54),">>You are in a Fight!<<")  
               ]   
       
   
asciitest :: IO()
asciitest = putStrLn  (("                              __"
       ++ "\n                            .d$$b"
       ++ "\n                          .' TO$;\\"
       ++ "\n                         /  : TP._;"
       ++ "\n                        / _.;  :Tb|"
       ++ "\n                       /   /   ;j$j"
       ++ "\n                   _.-\"       d$$$$"
       ++ "\n                 .' ..       d$$$$;"
       ++ "\n                /  /P'      d$$$$P. |\\"
       ++ "\n               /   \"      .d$$$P' |\\^\"l"
       ++ "\n             .'           `T$P^\"\"\"\"\"  :"
       ++ "\n         ._.'      _.'                ;"
       ++ "\n      `-.-\".-'-' ._.       _.-\"    .-\""
       ++ "\n    `.-\" _____  ._              .-\""
       ++ "\n   -(.g$$$$$$$b.              .'"
       ++ "\n     \"\"^^T$$$P^)            .(:"
       ++ "\n       _/  -\"  /.'         /:/;"
       ++ "\n    ._.'-'`-'  \")/         /;/;"
       ++ "\n `-.-\"..--\"\"   \" /         /  ;"
       ++ "\n.-\" ..--\"\"        -'          :"
       ++ "\n..--\"\"--.-\"         (\\      .-(\\"
       ++ "\n  ..--\"\"              `-\\(\\/;`"
       ++ "\n    _.                      :"
       ++ "\n                            ;`-"
       ++ "\n                           :\\"
       ++ "\n                           ;  "))
