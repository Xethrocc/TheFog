import Ascii
import Functions
import System.Console.ANSI

main = do 
 game
              
gameLoop :: Game -> IO Game
gameLoop (location, character, s, obj) = do
    setTitle ("THE FOG - " ++ (locS location))
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")

    ascii $ locA location
    putStr ("                    You are in " )
    (bold $ locS location) -- ++ location
    putStr ("\n                    You are " )
    (bold $ getString character) -- charater
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
    putStrLn ("Usable Objects:\n")
    setSGR [SetColor Foreground Vivid Red]
    putStrLn (getObjHere obj location)
    setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Green]
    putStrLn ("\nYour Attack:  " ++ show(getAtk character))
    putStrLn ("Your Defense: " ++ show(getDef character))
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("Your HP:      "  ++ show(getHP character))
    setSGR [Reset]
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    input <- getLine
    let defense      = addDef s character
        attack       = addAng s defense
        newcharacter = addSteps attack
        split     = splitString input             -- Splittet den String an den Leerzeichen in einzelne Strings auf 
        obje      = last split                    -- Gibt den zweiten String aus (der das Objekt sein sollte)
        actio     = head split                    -- Gibt den ersten String aus (der die Aktion sein sollte)
        object    = getObject obje (obj++s)       -- Gibt anhand des Strings das Objekt aus
        possBool  = isActionPossible actio object -- Boolean ob eine Aktion auf das Objekt möglich ist
        hereBool  = isObjHere object location     -- Boolean ob das Objekt auch an der aktuellen Position ist
        actioBool = possBool && hereBool          -- Boolean ob zwei Booleans True sind (Objekt und Objektliste)
        randInt   = random (getSteps character)
        locInt -- Integer der nächsten Position --
            | input `elem` secret = locI SE location -- Spezielle "Richtungen"
            | input `elem` north  = locI N location  -- Norden
            | input `elem` east   = locI O location  -- Osten
            | input `elem` south  = locI S location  -- Süden
            | input `elem` west   = locI W location  -- Westen
            | otherwise           = locA location
        newLoc = (gameMap !! locInt)                 -- Die neue Location anhand der locInt
        simpleAction   -- Einfache Aktionen die nur Strings bzw IO() Strings ausgeben
            | actio == "read"    && actioBool     = readObj
            | actio == "examine" && actioBool     = examineObj
            | otherwise                           = doNothinSimple ------------- wenn nichts zutrifft
        changingAction -- komplexere Aktionen, die Objekte verändern
            | actio == "burn"    && actioBool     = burnObj object obj randInt
            | actio == "activate" && actioBool    = activateObj object obj s
            | actio `elem ` savehelp && actioBool = savePrincess object obj randInt
            | otherwise                           = doNothin object obj -------- wenn nichts zutrifft
        takeAction     -- take Aktion, welche einen Geganstand aus der Map nimmt und in das Inventar überträgt
            | actio `elem` ["take","pick"]    && actioBool     = takeObj object obj s
            | otherwise                           = doNothinExt object obj s --- wenn nichts zutrifft
        showInvAction
            | input `elem` invActions             = showInventory s
            | otherwise                           = putStrLn ("")

    if input `elem` quitCharacter -- user wants to quit the game
     then do
     let  g = (location, character ,s, obj)
     return (g)
    else do
      let inv      = snd takeAction -- Geändertes   Inventar  durch die Take-Aktion
          chngdObL = fst takeAction -- Geänderte  Objektliste durch die Take-Aktion
      simpleAction object           -- Die einfache Aktion auf das Objekt anwenden
      showInvAction -- Zeigt das Inventar
      -----
      if (actio `elem` changinAction && actioBool) then do randText randInt actio
                                                           ifThereIsAnyDirection actio alldir
                                                           fightLoop((54,"Wolf-Fight",[]), newcharacter, s, changingAction) location object 10
      else if (actio == "activate" && actioBool) then do putStrLn ("You activated it\n")
                                                         ifThereIsAnyDirection actio alldir
                                                         gameLoop(newLoc, attack, s, changingAction)
      else if (actio `elem` ["take","pick"] && actioBool) then do putStrLn ("You picked it up, it's in you Invenory\n") 
                                                                  ifThereIsAnyDirection actio alldir
                                                                  gameLoop(newLoc, attack, inv, chngdObL)
      else do ifThereIsAnyDirection actio alldir
              gameLoop (newLoc, newcharacter, s, obj)
    

fightLoop :: Game -> Location -> Object -> Life -> IO Game
fightLoop (location, character, inv, objList) loc object x = do

    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")
    ascii $ locA location

    putStrLn ("                    You are fighting a " ++ (getObjStr object))
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
    putStrLn ("\nYour Atk: " ++ show(getAtk character))
    putStrLn ("Your Def: " ++ show(getDef character))
    putStrLn ("Your HP:  " ++ show(getHP character))
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("\nHP of the " ++ (getObjStr object) ++ ": " ++ show(x))
    putStrLn ("\n\nYou can 'attack' or try to 'flee'..\n What will u do?")
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    
    input <- getLine
    
    let 
     steps     = getSteps character
     randInt   = random (getSteps character)
     whenKill  = attackObj object objList -- Wenn erfolgreich, dann wird Objekt in der Liste geändert/als besiegt markiert
     newObjList= randomObj steps whenKill object -- erzeugt das besiegte Objekt nochmal an einer neuen Position
    if (x<=0) then do putStrLn ("You Won! press 'enter' to continue")
                      blabla <- getLine
                      gameLoop (loc, character, inv, newObjList)
    else if input `elem` quitFight 
     then do gameLoop (loc, character, inv, objList)
     else do fightLoop (location, character, inv, objList) loc object (x-(getAtk character + randInt))
    
-- starts the game loop with the initial Game
game :: IO ()
game = do
    putStrLn ("Whats Your (Character) Name?")
    charName <- getLine
    gameLoop ((gameMap !! 53) , (giveName charName), [] ,objectList)
    return ()

-- input that exits the game
quitCharacter = [":q", ":Q", ":e", ":E"] 
quitFight     = ["flee","escape","run","Flee","Escape","Run"]
-- Inputs für die Richtungen u. anderes
north         = ["North","north","N"     ,"n"         ]
east          = ["East" ,"east" ,"O"     ,"o" ,"E","e"]
south         = ["South","south","S"     ,"s"         ]
west          = ["West" ,"west" ,"W"     ,"w"         ]
secret        = ["game","dig","crawl","swim"          ]
alldir        = north++west++south++east++secret++[""]
actions       = ["take","pick","save","help","attack","hit","examine","read","activate"]
changinAction = ["attack","burn","hit","save","help","fight"]
invActions    = ["show Inventory","show inventory","Inventory","inventory","show Items","show items","Items","items"]
savehelp      = ["save","help"]




-- ------------------- GAME ---------------------

type Game  = (Location, Character, Inventory, ObjectList) -- you can modify the type

gameMap :: Map
gameMap = 
 [
  (0,"Home",[(1,N),(4,S)]),
  (1,"Garden",[(0,S),(2,N)]),
  (2,"Path",[(1,S),(3,N),(31,O),(39,W)]),
  (3,"Path",[(6,N),(2,S)]),
  (4,"Basement",[(0,N)]),
  (5,"Errorfield",[]),
  (6,"Path",[(12,N),(7,O),(8,W),(3,S)]),
  (7,"Barn",[(6,W)]),
  (8,"Graveyard",[(11,N),(9,W),(6,O)]),
  (9,"Graveyard",[(10,N),(8,O)]),
  (10,"Graveyard",[(9,S),(11,O),(29,SE)]),
  (11,"Graveyard",[(10,W),(8,S)]),
  (12,"Forest",[(13,N),(6,S)]),
  (13,"Forest",[(12,S),(14,O),(15,W)]),
  (14,"Forest",[(13,W),(18,N)]),
  (15,"Forest",[(13,O),(16,N)]),
  (16,"Forest",[(21,N),(15,S)]),
  (17,"Forest",[(20,N),(28,SE)]),
  (18,"Forest",[(19,N),(14,S)]),
  (19,"Forest",[(20,W),(18,S),(24,O)]),
  (20,"Forest",[(23,N),(19,O),(17,S),(21,W)]),
  (21,"Forest",[(20,O),(22,W),(16,S)]),
  (22,"Forest",[(21,O)]),
  (23,"Glade",[(20,S)]),
  (24,"Forest",[(25,N),(19,W)]),
  (25,"Forest",[(26,N),(24,S)]),
  (26,"Forest",[(27,N),(25,S)]),
  (27,"Glade",[(26,S)]),
  (28,"Earth Shrine",[(17,S)]),
  (29,"Secret Grave Entrance",[(10,O),(30,W)]),
  (30,"Dark Room",[(29,O)]),
  (31,"Path",[(2,W),(32,O)]),
  (32,"Path",[(31,W),(34,O),(33,S)]),
  (33,"River",[(32,N),(38,SE)]),
  (34,"Bridge",[(32,W),(35,O)]),
  (35,"Courtyard of the Castle",[(34,W),(36,O)]),
  (36,"Entrance Hall",[(35,W),(37,O)]),
  (37,"Throne Room",[(36,W)]),
  (38,"Water Shrine",[(33,N)]),
  (39,"Path",[(2,O),(40,W)]),
  (40,"Path",[(39,O),(41,W),(46,S)]),
  (41,"Path",[(40,O),(42,W)]),
  (42,"Stairs",[(41,O),(43,W)]),
  (43,"Stairs",[(42,O),(44,W)]),
  (44,"Top Of The Mountain",[(43,O),(45,SE)]),
  (45,"Fire Shrine",[(44,S)]),
  (46,"Path",[(40,N),(47,O),(48,W),(49,S)]),
  (47,"Dead End",[(46,W)]),
  (48,"Dead End",[(46,O)]),
  (49,"Canyon",[(46,N),(50,SE)]),
  (50,"Flying Stones",[(49,N),(51,SE)]),
  (51,"Flying Stones",[(50,N),(52,S)]),
  (52,"Air Shrine",[(49,N)]),
  (53,"Start Menue",[(0,SE)]),
  (54,"Wolf-Fight",[])  
 ]


