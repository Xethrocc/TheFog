import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants
import Data.Maybe (isJust)
import Data.List (find, delete)
import System.Random (newStdGen, randomR)

main :: IO ()
main = do
  game

drawBar :: Int -> Int -> String
drawBar current maxHP
    | maxHP <= 0 = "                    ]"
    | otherwise =
        let filled = min 10 (current * 10 `div` maxHP)
            empty = 10 - filled
        in (replicate filled '█') ++ (replicate empty ' ')

-- (Fix 2) Richtung aus Eingabe parsen -> Nothing statt `error`-Crash
stringToDirection :: String -> Maybe Direction
stringToDirection s
    | s `elem` north = Just N
    | s `elem` east  = Just O
    | s `elem` south = Just S
    | s `elem` west  = Just W
    | s `elem` secret = Just SE
    | otherwise = Nothing

-- (Fix 17 / UX) Pause nach Aktionen, die Text ausgeben
pause :: IO ()
pause = do
    putStrLn "Press 'Enter' to continue."
    _ <- getLine
    putStrLn ""

-- (Fix 2/13/25) Bewegung nur bei gueltiger Richtung UND vorhandenem Exit.
-- Bei ungueltiger Eingabe: Meldung, Spielzustand unveraendert (keine
-- Wolf-Bewegung, kein Step-Counter).
handleMovement :: Game -> String -> IO Game
handleMovement game direction =
    case stringToDirection direction of
        Nothing -> do
            putStrLn "You can't go that way.\n"
            return game
        Just dir -> case locI dir (gameLocation game) of
            Nothing -> do
                putStrLn "You can't go that way.\n"
                return game
            Just nextLocId -> do
                let newStep = gameStepCounter game + 1
                    newWolves = moveWolves newStep (gameWolves game)
                    wolvesWithNew = spawnPostFogWolves newWolves (gameShrineFlags game) newStep
                    -- (Fix 20) Steps-Zaehler des Charakters mitfuehren
                    movedGame = game { gameLocation = gameMap !! nextLocId
                                     , gameWolves = wolvesWithNew
                                     , gameStepCounter = newStep
                                     , gameCharacter = addSteps (gameCharacter game) }
                putStrLn ""
                -- (Fix 22) Woelfe sind gefaehrlich: Biss beim Betreten moeglich
                maybeBite movedGame

-- (Fix 11) Die Rettung ist jetzt deterministisch (kein mod-3-Glueck mehr)
-- und die Prinzessin verschwindet danach aus der Objektliste.
-- (Fix 22) Beim Betreten einer Location mit (lebendem) Wolf: 50% Chance
-- auf einen Biss (1-3 Schaden). Tod wird vom gameLoop-Guard abgefangen.
maybeBite :: Game -> IO Game
maybeBite game
    | any (\w -> wolfLoc w == locId (gameLocation game) && not (wolfDead w)) (gameWolves game) = do
        chance <- roll 0 1
        if chance == (1 :: Int)
            then do
                dmg <- roll 1 3
                let char = gameCharacter game
                putStrLn ("A wolf lunges from the shadows and bites you for " ++ show dmg ++ "!\n")
                return game { gameCharacter = char { charLife = getHP char - dmg } }
            else return game
    | otherwise = return game

handlePrincessSave :: Game -> Object -> IO Game
handlePrincessSave game obj = do
    putStrLn "You dive into the river and guide the princess to safety!\n"
    return game { gamePrincess = PrincessSaved
                , gameObjects = delete obj (gameObjects game) }

-- (Fix 5) Crystal-Check korrigiert: es wird geprueft, ob ein Objekt mit
-- objId 4 (Crystal) im Inventar liegt - nicht mehr, ob das Shrine-Objekt
-- selbst im Inventar ist (war immer False).
handleActivate :: Game -> Object -> IO Game
handleActivate game obj = do
    let hasCrystal = any (\t -> objId t == 4) (gameInventory game)
    if hasCrystal then do
        let updatedObjList = activateObj obj (gameObjects game) (gameInventory game)
            updatedFlags = updateShrineFlags obj (gameShrineFlags game)
        putStrLn "You activated it!\n"
        pause
        return game { gameObjects = updatedObjList, gameShrineFlags = updatedFlags }
    else do
        putStrLn "You need the Crystal to activate this shrine.\n"
        pause
        return game

-- (Fix 9/20) Inventar-Boni werden direkt auf den Charakter angewendet
-- (Schild -> Defense 10, Schwert -> Attack 10)
handleTake :: Game -> Object -> IO Game
handleTake game obj = do
    let (updatedObjList, updatedInv) = takeObj obj (gameObjects game) (gameInventory game)
        updatedChar = addDef updatedInv (addAng updatedInv (gameCharacter game))
    putStrLn "You picked it up, it's in your Inventory\n"
    pause
    return game { gameObjects = updatedObjList
                , gameInventory = updatedInv
                , gameCharacter = updatedChar }

-- (Fix 10) Gegner-HP aus den echten Wolf-Daten (Waechter 30, Patrouille 12);
-- der Wolf stirbt erst bei Sieg (in fightLoop); nach dem Kampf zurueck zur
-- urspruenglichen Location.
handleFight :: Game -> Object -> IO Game
handleFight game obj = do
    let homeLoc = gameLocation game
        enemyHP = case find (\w -> wolfId w == objId obj) (gameWolves game) of
                    Just w  -> wolfHp w
                    Nothing -> 12   -- statisches Wolf-Objekt ("Tutorial"-Wolf)
        updatedWolfGame = game { gameLocation = Location { locId = 54, locName = "Wolf-Fight", locExits = [] } }
    result <- fightLoop updatedWolfGame obj enemyHP
    return result { gameLocation = homeLoc }

-- (Fix 9) Wuerfel-Helfer fuer variablen Schaden
roll :: Int -> Int -> IO Int
roll lo hi = do
    gen <- newStdGen
    return (fst (randomR (lo, hi) gen))

-- (Fix 9/10) Kampfsystem: nur 'attack'/'hit' verursacht Schaden, der Wolf
-- schlaegt zurueck (max. 0 => Schild kann vollstaendig schuetzen), der
-- Spieler kann sterben; Gegner-HP stammt aus den Wolf-Daten.
fightLoop :: Game -> Object -> Int -> IO Game
fightLoop game@Game{gameLocation = location, gameCharacter = character, gameObjects = objList} object enemyHP = do
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")
    ascii (locA location)

    putStrLn ("                    You are fighting a " ++ (getObjStr object))
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
    putStrLn ("\nYour Atk: " ++ show(getAtk character))
    putStrLn ("Your Def: " ++ show(getDef character))
    putStrLn ("\nYour HP:  " ++ show(getHP character) ++ " [" ++ (drawBar (getHP character) 10) ++ "]")
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("\nHP of the " ++ (getObjStr object) ++ ": " ++ show(enemyHP) ++ " [" ++ (drawBar enemyHP 30) ++ "]")
    putStrLn ("\n\nYou can 'attack' or try to 'flee'..\n What will u do?")
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")

    input <- getLine

    if input `elem` quitFight
        then do
            putStrLn "You fled from the fight!\n"
            return game
        else if input `elem` ["attack", "hit"]
            then do
                pBonus <- roll 0 2
                let playerDmg = getAtk character + pBonus
                    newEnemyHP = enemyHP - playerDmg
                if newEnemyHP <= 0
                    then do
                        putStrLn "You Won! Press 'Enter' to continue"
                        _ <- getLine
                        let updatedObjList = attackObj object objList
                            killWolf w = if wolfId w == objId object then w { wolfDead = True } else w
                        return game { gameObjects = updatedObjList, gameWolves = map killWolf (gameWolves game) }
                    else do
                        wBonus <- roll 0 2
                        let (wAtk, _) = wolfCombatStats (gameWolves game) (objId object)
                            wolfDmg = max 0 (wAtk - getDef character + wBonus)
                            newHP = getHP character - wolfDmg
                        putStrLn ("You hit for " ++ show playerDmg ++ "! The " ++ getObjStr object ++ " hits back for " ++ show wolfDmg ++ "!\n")
                        if newHP <= 0
                            then return game { gameCharacter = character { charLife = 0 } }
                            else fightLoop (game { gameCharacter = character { charLife = newHP } }) object newEnemyHP
            else do
                putStrLn "You hesitate... (You can 'attack' or 'flee')\n"
                fightLoop game object enemyHP

-- (Fix 9/22) Tod-Check zentral im gameLoop: gilt fuer Kampf- und Biss-Tod.
gameLoop :: Game -> IO ()
gameLoop game
    | getHP (gameCharacter game) <= 0 = do
        putStrLn "Your vision fades... you collapse. You have died."
        putStrLn "Game Over."
    | otherwise = gameLoop' game

-- (Fix 1/3/4) Das Spiel-Herzstueck: zeigt die Location an, liest die Eingabe
-- und delegiert an `processInput`.
gameLoop' :: Game -> IO ()
gameLoop' game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList, gameWolves = wolves} = do
    setTitle ("THE FOG - " ++ (locS location))
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")

    ascii (locA location)
    putStr ("                    You are in " )
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr (locS location)
    setSGR [Reset]
    putStr ("\n                    You are " )
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr (getString character)
    setSGR [Reset]
    putStrLn ("\n                    " ++ locD location ++ "\n\n")

    if checkEncounter location wolves
        then do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "WARNING: A wolf is nearby!"
            setSGR [Reset]
            putStrLn ""
        else putStrLn ""

    putStrLn ("Usable Objects:\n")
    setSGR [SetColor Foreground Vivid Red]
    putStrLn (getObjHere objList wolves location)
    setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Green]
    putStrLn ("\nYour Attack:  " ++ show(getAtk character))
    putStrLn ("Your Defense: " ++ show(getDef character))
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("Your HP:      "  ++ show(getHP character))
    setSGR [Reset]
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    input <- getLine

    if checkEnding (gameShrineFlags game)
        then printVictory game
        else processInput game input

-- (Fix 7/17) Eingabe-Verarbeitung in klarer Reihenfolge:
-- 1. leere Eingabe ignorieren  2. quit  3. Inventory  4. Bewegung  5. Aktion
processInput :: Game -> String -> IO ()
processInput game input
    | null input = gameLoop game
    | input `elem` quitCharacter = putStrLn "Bye!"   -- (Fix 16) beendet das Spiel wirklich
    | input `elem` invActions = showInventory (gameInventory game) >> pause >> gameLoop game
    | isJust (stringToDirection input) = handleMovement game input >>= gameLoop
    | otherwise =
        let splitInput = splitString input
            actionStr = head splitInput
            objectName = unwords (tail splitInput)
            targetObject = if null objectName
                               then Nothing
                               else getObject objectName (gameObjects game ++ gameInventory game) (gameWolves game)
        in case targetObject of
            Nothing -> do
                putStrLn "You can't do that. (Type 'read Help' for a list of commands.)\n"
                gameLoop game
            Just obj -> performAction game actionStr obj

-- (Fix 1/3/4) Objekt-Aktionen: Ergebnisse der Handler werden verkettet
-- (>>= gameLoop), sodass Zustandserweiterungen (Inventar, Flags, Prinzessin)
-- erhalten bleiben und das Spiel nach der Aktion weiterlaeuft.
performAction :: Game -> String -> Object -> IO ()
performAction game actionStr obj
    -- (Fix 9/20) Inventar-Aktionen (Apfel essen) umgehen den isObjHere-Check
    | actionStr `elem` ["consume", "eat"] = eatApple
    | not (isActionPossible actionStr obj) = cantDo
    | not (isObjHere obj (gameLocation game)) = cantDo
    | otherwise = case actionStr of
        "read" -> readObj obj >> pause >> gameLoop game
        "examine" ->
            if objId obj == 9
                then interactSword obj >> pause >> gameLoop game
                else examineObj obj >> pause >> gameLoop game
        "save" -> saveAction
        "help" -> saveAction
        "take" -> handleTake game obj >>= gameLoop
        "pick" -> handleTake game obj >>= gameLoop   -- Alias, siehe objActions
        "activate" -> do
            g <- handleActivate game obj
            if checkEnding (gameShrineFlags g)
                then printVictory g   -- (Fix 12) alle 4 Shrines = Sieg
                else gameLoop g
        "attack" -> handleFight game obj >>= gameLoop
        "fight" -> handleFight game obj >>= gameLoop
        "hit" -> handleFight game obj >>= gameLoop   -- Alias
        _ -> doNothinSimple obj >> gameLoop game
  where
    cantDo = do
        putStrLn "You can't do that here.\n"
        gameLoop game
    -- (Fix 9) Nach dem Kampf: Tod abfragen, sonst weiter im Spiel
    -- (Fix 9/20) Apfel essen heilt 5 HP (max. 10) und verschwindet aus dem Inventar
    eatApple
        | objId obj == 2 = do
            let g2 = game { gameInventory = delete obj (gameInventory game)
                          , gameCharacter = (gameCharacter game) { charLife = min 10 (getHP (gameCharacter game) + 5) } }
            putStrLn "You eat the Apple. It restores some of your health.\n"
            pause
            gameLoop g2
        | otherwise = cantDo
    -- (Fix 11) Rettung deterministisch; Objekt wird danach entfernt
    saveAction = case gamePrincess game of
        PrincessDead -> putStrLn "The princess is already dead. You failed to save her.\n" >> gameLoop game
        PrincessSaved -> putStrLn "The princess is already saved.\n" >> gameLoop game
        PrincessAlive -> do
            g <- handlePrincessSave game obj
            pause
            gameLoop g

-- (Fix 12) Sieg statt "Game Over", wenn alle vier Schreine aktiviert sind
printVictory :: Game -> IO ()
printVictory game = do
    putStrLn ""
    putStrLn "The four shrines pulse in unison! A bright light breaks through the fog..."
    putStrLn "Slowly the fog recedes and the land can breathe freely again."
    case gamePrincess game of
        PrincessSaved -> putStrLn "And the princess you rescued will tell everyone of your heroism."
        PrincessDead  -> putStrLn "But the princess is lost forever - not every story ends well."
        PrincessAlive -> putStrLn "Somewhere by the river, a princess is still waiting to be saved..."
    putStrLn ""
    putStrLn "                    *** VICTORY ***"

game :: IO ()
game = do
    putStrLn ("Whats Your (Character) Name?")
    charName <- getLine
    gameLoop Game { gameLocation = gameMap !! 53, gameCharacter = giveName charName, gameInventory = [], gameObjects = objectList, gameStepCounter = 0, gameWolves = initialWolves, gameShrineFlags = initialShrineFlags, gamePrincess = initialPrincessStatus }
    return ()
