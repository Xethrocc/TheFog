import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants
import Control.Monad (when)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  game

drawBar :: Int -> Int -> String
drawBar current maxHP
    | maxHP <= 0 = "                    ]"
    | otherwise =
        let filled = min 10 (current * 10 `div` maxHP)
            empty = 10 - filled
        in (replicate filled '£') ++ (replicate empty ' ')

handleMovement :: Game -> String -> IO Game
handleMovement game direction = do
    let nextLocId = fromMaybe (locId (gameLocation game)) (locI (stringToDirection direction) (gameLocation game))
        newStep = gameStepCounter game + 1
        newWolves = moveWolves newStep (gameWolves game)
        wolvesWithNew = spawnPostFogWolves newWolves (gameShrineFlags game) newStep
    when (shouldWaitForEnter direction alldir) $ do
        putStrLn "Press 'Enter' to continue."
        _ <- getLine
        putStrLn ""
    return game { gameLocation = gameMap !! nextLocId, gameWolves = wolvesWithNew, gameStepCounter = newStep }

handlePrincessSave :: Game -> IO Game
handlePrincessSave game = do
    let stepMod = gameStepCounter game `mod` 3
        result = if stepMod == 0 
                    then (PrincessDead, "You tried to save the princess but she drowned!\n")
                    else (PrincessSaved, "You guided the princess to safety!\n")
    putStrLn (snd result)
    return game { gamePrincess = fst result }

handleActivate :: Game -> Object -> IO Game
handleActivate game obj = do
    let hasCrystal = isInInv obj (filter (\t -> objId t == 4) (gameInventory game))
    if hasCrystal then do
        let updatedObjList = activateObj obj (gameObjects game) (gameInventory game)
            updatedFlags = updateShrineFlags obj (gameShrineFlags game)
        putStrLn "You activated it!\n"
        when (shouldWaitForEnter "activate" alldir) $ do
            putStrLn "Press 'Enter' to continue."
            _ <- getLine
            putStrLn ""
        return game { gameObjects = updatedObjList, gameShrineFlags = updatedFlags }
    else do
        putStrLn "You need the Crystal to activate this shrine.\n"
        when (shouldWaitForEnter "activate" alldir) $ do
            putStrLn "Press 'Enter' to continue."
            _ <- getLine
            putStrLn ""
        return game

handleTake :: Game -> Object -> IO Game
handleTake game obj = do
    let (updatedObjList, updatedInv) = takeObj obj (gameObjects game) (gameInventory game)
    putStrLn "You picked it up, it's in your Inventory\n"
    when (shouldWaitForEnter "take" alldir) $ do
        putStrLn "Press 'Enter' to continue."
        _ <- getLine
        putStrLn ""
    return game { gameObjects = updatedObjList, gameInventory = updatedInv }

handleFight :: Game -> Object -> IO Game
handleFight game obj = do
    let fightGame = game { gameLocation = Location { locId = 54, locName = "Wolf-Fight", locExits = [] } }
        objWolfId = objId obj
        killWolf w = if wolfId w == objWolfId then w { wolfDead = True } else w
        updatedWolfGame = fightGame { gameWolves = map killWolf (gameWolves fightGame) }
    fightLoop updatedWolfGame obj 10

fightLoop :: Game -> Object -> Life -> IO Game
fightLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList} object enemyHP = do
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
    
    let randVal = 0
        newEnemyHP = enemyHP - (getAtk character + randVal)
    if (newEnemyHP <= 0) then do
        putStrLn ("You Won! Press 'Enter' to continue")
        _ <- getLine
        let updatedObjList = attackObj object objList
        return game { gameObjects = updatedObjList }
    else if input `elem` quitFight
        then return game
        else fightLoop game object newEnemyHP

gameLoop :: Game -> IO ()
gameLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList, gameWolves = wolves} = do
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
        then do
            putStrLn "The fog descends over the land..."
            putStrLn "Wolves grow stronger, guards fall, the world becomes hostile..."
            putStrLn "Game Over."
        else do
            if input `elem` quitCharacter
                then putStrLn "Bye!" >> gameLoop game
                else do
                    let splitInput = splitString input
                        actionStr = head splitInput
                        objectName = last splitInput
                        allObjects = objList
                        targetObject = getObject objectName allObjects wolves
                    
                    case targetObject of
                        Just obj -> do
                            let isActionPossible' = isActionPossible actionStr obj
                                isObjHere' = isObjHere obj location
                                actioBool = isActionPossible' && isObjHere'
                            case actionStr of
                                "read" | actioBool -> readObj obj
                                "examine" | actioBool -> examineObj obj
                                "save" | actioBool -> 
                                    case gamePrincess game of
                                        PrincessDead -> putStrLn "The princess is already dead. You failed to save her.\n"
                                        PrincessSaved -> putStrLn "The princess is already saved.\n"
                                        PrincessAlive -> void $ handlePrincessSave game
                                "take" | actioBool -> void $ handleTake game obj
                                "activate" | actioBool -> void $ handleActivate game obj
                                "attack" | actioBool -> void $ handleFight game obj
                                "fight" | actioBool -> void $ handleFight game obj
                                _ -> doNothinSimple obj
                            if input `elem` invActions
                                then showInventory inv
                                else return ()
                            if actionStr `elem` changinAction && actioBool && actionStr `elem` ["attack", "fight"]
                                then return ()
                                else handleMovement game input >>= gameLoop
                        Nothing -> handleMovement game input >>= gameLoop

void :: IO a -> IO ()
void x = x >> return ()

game :: IO ()
game = do
    putStrLn ("Whats Your (Character) Name?")
    charName <- getLine
    gameLoop Game { gameLocation = gameMap !! 53, gameCharacter = giveName charName, gameInventory = [], gameObjects = objectList, gameStepCounter = 0, gameWolves = initialWolves, gameShrineFlags = initialShrineFlags, gamePrincess = initialPrincessStatus }
    return ()

stringToDirection :: String -> Direction
stringToDirection s
    | s `elem` north = N
    | s `elem` east  = O
    | s `elem` south = S
    | s `elem` west  = W
    | s `elem` secret = SE
    | otherwise = error "Invalid direction string"
