import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants

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




-- ------------------- GAME ---------------------




