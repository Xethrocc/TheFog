import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)

main :: IO ()
main = do
  game

gameLoop :: Game -> IO ()
gameLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList, gameStepCounter = step, gameWolves = wolves, gameShrineFlags = flags, gamePrincess = princess} = do
    setTitle ("THE FOG - " ++ (locS location))
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")

    ascii (locA location)
    putStr ("                    You are in " )
    bold (locS location)
    putStr ("\n                    You are " )
    bold (getString character)
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
    
    -- Check for wolf encounter
    if checkEncounter location wolves
        then do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "WARNING: A wolf is nearby!"
            setSGR [Reset]
            putStrLn ""
        else putStrLn ""
    putStrLn ("Usable Objects:\n")
    setSGR [SetColor Foreground Vivid Red]
    putStrLn (getObjHere objList location)
    setSGR [SetConsoleIntensity BoldIntensity,SetColor Foreground Vivid Green]
    putStrLn ("\nYour Attack:  " ++ show(getAtk character))
    putStrLn ("Your Defense: " ++ show(getDef character))
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("Your HP:      "  ++ show(getHP character))
    setSGR [Reset]
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    input <- getLine
    
    let newCharacterWithSteps = addSteps character
        defenseChar           = addDef inv newCharacterWithSteps
        attackChar            = addAng inv defenseChar
        splitInput            = splitString input
        actionStr             = head splitInput
        objectName            = last splitInput
    
    -- Check ending condition
    if checkEnding flags
        then do
            putStrLn "The fog descends over the land..."
            putStrLn "Wolves grow stronger, guards fall, the world becomes hostile..."
            putStrLn "Game Over."
        else
            case getObject objectName (objList ++ inv) of
                Just targetObject -> do
                    let isActionPossible' = isActionPossible actionStr targetObject
                        isObjHere'        = isObjHere targetObject location
                        actioBool         = isActionPossible' && isObjHere'

                    if input `elem` quitCharacter
                        then putStrLn "Bye!"
                        else do
                            case actionStr of
                                "read" | actioBool -> readObj targetObject
                                "examine" | actioBool -> examineObj targetObject
                                "save" | actioBool -> do
                                    case princess of
                                        PrincessDead -> putStrLn "The princess is already dead. You failed to save her.\n"
                                        PrincessSaved -> putStrLn "The princess is already saved.\n"
                                        PrincessAlive -> do
                                            let randStep = step `mod` 3
                                            if randStep == 0 
                                                then putStrLn "You tried to save the princess but she drowned!\n"
                                                else putStrLn "You guided the princess to safety!\n"
                                            let newPrincess = if randStep == 0 then PrincessDead else PrincessSaved
                                            gameLoop game { gamePrincess = newPrincess, gameStepCounter = step + 1 }
                                "take" | actioBool -> do
                                    putStrLn "You picked it up, it's in your Inventory\n"
                                    when (shouldWaitForEnter actionStr alldir) $ do
                                        putStrLn "Press 'Enter' to continue."
                                        _ <- getLine
                                        putStrLn ""
                                    let (updatedObjList, updatedInv) = takeObj targetObject objList inv
                                    gameLoop game { gameObjects = updatedObjList, gameInventory = updatedInv, gameStepCounter = step + 1 }
                                "activate" | actioBool -> do
                                    if isInInv targetObject (filter (\t -> objId t == 4) inv) then do
                                        putStrLn "You activated it!\\n"
                                        when (shouldWaitForEnter actionStr alldir) $ do
                                            putStrLn "Press 'Enter' to continue."
                                            _ <- getLine
                                            putStrLn ""
                                        let updatedObjList = activateObj targetObject objList inv
                                        let updatedFlags = updateShrineFlags targetObject flags
                                        gameLoop game { gameObjects = updatedObjList, gameShrineFlags = updatedFlags, gameStepCounter = step + 1 }
                                    else do
                                        putStrLn "You need the Crystal to activate this shrine.\\n"
                                        when (shouldWaitForEnter actionStr alldir) $ do
                                            putStrLn "Press 'Enter' to continue."
                                            _ <- getLine
                                            putStrLn ""
                                        gameLoop game { gameStepCounter = step + 1 }
                                _ -> doNothinSimple targetObject

                            if input `elem` invActions
                                then showInventory inv
                                else putStrLn ""

                            if actionStr `elem` changinAction && actioBool
                                then do
                                    let randVal = step `mod` 10
                                    randText randVal actionStr
                                    when (shouldWaitForEnter actionStr alldir) $ do
                                        putStrLn "Press 'Enter' to continue."
                                        _ <- getLine
                                        putStrLn ""
                                    updatedGame <- fightLoop game { gameLocation = Location { locId = 54, locName = "Wolf-Fight", locExits = [] }, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList, gameStepCounter = step, gameWolves = wolves, gameShrineFlags = flags, gamePrincess = princess } targetObject 10
                                    gameLoop updatedGame
                                else do
                                    let nextLocId = fromMaybe (locId location) (locI (stringToDirection input) location)
                                    when (shouldWaitForEnter actionStr alldir) $ do
                                        putStrLn "Press 'Enter' to continue."
                                        _ <- getLine
                                        putStrLn ""
                                    let newStep = step + 1
                                        newWolves = moveWolves newStep wolves
                                    gameLoop game { gameLocation = gameMap !! nextLocId, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList, gameStepCounter = newStep, gameWolves = newWolves }
                Nothing -> do
                    let nextLocId = fromMaybe (locId location) (locI (stringToDirection input) location)
                    when (shouldWaitForEnter actionStr alldir) $ do
                        putStrLn "Press 'Enter' to continue."
                        _ <- getLine
                        putStrLn ""
                    let newStep = step + 1
                        newWolves = moveWolves newStep wolves
                    gameLoop game { gameLocation = gameMap !! nextLocId, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList, gameStepCounter = newStep, gameWolves = newWolves }

fightLoop :: Game -> Object -> Life -> IO Game
fightLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList} object enemyHP = do
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")
    ascii (locA location)

    putStrLn ("                    You are fighting a " ++ (getObjStr object))
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
    putStrLn ("\nYour Atk: " ++ show(getAtk character))
    putStrLn ("Your Def: " ++ show(getDef character))
    putStrLn ("Your HP:  " ++ show(getHP character))
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("\nHP of the " ++ (getObjStr object) ++ ": " ++ show(enemyHP))
    putStrLn ("\n\nYou can 'attack' or try to 'flee'..\n What will u do?")
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    
    input <- getLine
    
    let randVal = 0  -- Use step counter instead
        newEnemyHP = enemyHP - (getAtk character + randVal)
    if (newEnemyHP <= 0) then do
        putStrLn ("You Won! Press 'Enter' to continue")
        _ <- getLine
        let updatedObjList = attackObj object objList
        return game { gameLocation = gameLocation game, gameCharacter = character, gameInventory = inv, gameObjects = updatedObjList }
    else if input `elem` quitFight
        then return game
        else fightLoop game object newEnemyHP

-- starts the game loop with the initial Game
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

checkEnding :: (Bool, Bool, Bool, Bool) -> Bool
checkEnding (e, w, f, a) = e && w && f && a

-- input that exits the game


-- ------------------- GAME ---------------------
