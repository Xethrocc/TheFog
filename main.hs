import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants
import System.Random (StdGen, newStdGen, randomR)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  game

gameLoop :: Game -> IO Game
gameLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList} = do
    gen <- newStdGen -- Get a new StdGen for each loop iteration
    setTitle ("THE FOG - " ++ (locS location))
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")

    ascii (locA location)
    putStr ("                    You are in " )
    bold (locS location)
    putStr ("\n                    You are " )
    bold (getString character)
    putStrLn ("\n                    " ++ locD location ++ "\n\n")
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
        
        (randVal, newGen)     = random gen -- Get a random value and new generator

    case getObject objectName (objList ++ inv) of
        Just targetObject -> do
            let isActionPossible' = isActionPossible actionStr targetObject
                isObjHere'        = isObjHere targetObject location
                actioBool         = isActionPossible' && isObjHere'

            if input `elem` quitCharacter
                then return game
                else do
                    case actionStr of
                        "read" | actioBool -> readObj targetObject
                        "examine" | actioBool -> examineObj targetObject
                        _ -> doNothinSimple targetObject

                    if input `elem` invActions
                        then showInventory inv
                        else putStrLn ""

                    if actionStr `elem` changinAction && actioBool
                        then do
                            randText randVal actionStr
                            when (shouldWaitForEnter actionStr alldir) $ do
                                putStrLn "Press 'Enter' to continue."
                                _ <- getLine
                                putStrLn ""
                            fightLoop game { gameLocation = Location { locId = 54, locName = "Wolf-Fight", locExits = [] }, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList } targetObject 10 newGen
                        else if actionStr == "activate" && actioBool
                            then do
                                putStrLn "You activated it\n"
                                when (shouldWaitForEnter actionStr alldir) $ do
                                    putStrLn "Press 'Enter' to continue."
                                    _ <- getLine
                                    putStrLn ""
                                let updatedObjList = activateObj targetObject objList inv
                                gameLoop game { gameLocation = location, gameCharacter = attackChar, gameInventory = inv, gameObjects = updatedObjList }
                        else if actionStr `elem` ["take","pick"] && actioBool
                            then do
                                putStrLn "You picked it up, it's in your Inventory\n"
                                when (shouldWaitForEnter actionStr alldir) $ do
                                    putStrLn "Press 'Enter' to continue."
                                    _ <- getLine
                                    putStrLn ""
                                let (updatedObjList, updatedInv) = takeObj targetObject objList inv
                                gameLoop game { gameLocation = location, gameCharacter = attackChar, gameInventory = updatedInv, gameObjects = updatedObjList }
                        else do
                            let nextLocId = fromMaybe (locId location) (locI (stringToDirection input) location)
                            when (shouldWaitForEnter actionStr alldir) $ do
                                putStrLn "Press 'Enter' to continue."
                                _ <- getLine
                                putStrLn ""
                            gameLoop game { gameLocation = gameMap !! nextLocId, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList }
        Nothing -> do
            let nextLocId = fromMaybe (locId location) (locI (stringToDirection input) location)
            when (shouldWaitForEnter actionStr alldir) $ do
                putStrLn "Press 'Enter' to continue."
                _ <- getLine
                putStrLn ""
            gameLoop game { gameLocation = gameMap !! nextLocId, gameCharacter = attackChar, gameInventory = inv, gameObjects = objList }


fightLoop :: Game -> Object -> Life -> StdGen -> IO Game
fightLoop game@Game{gameLocation = location, gameCharacter = character, gameInventory = inv, gameObjects = objList} object enemyHP gen = do
    let (randVal, newGen) = random gen

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
    
    let newEnemyHP = enemyHP - (getAtk character + randVal)
    if (newEnemyHP <= 0) then do
        putStrLn ("You Won! Press 'Enter' to continue")
        _ <- getLine
        let updatedObjList = attackObj object objList
        gameLoop game { gameLocation = gameLocation game, gameCharacter = character, gameInventory = inv, gameObjects = updatedObjList }
    else if input `elem` quitFight
        then gameLoop game { gameLocation = gameLocation game, gameCharacter = character, gameInventory = inv, gameObjects = objList }
        else fightLoop game object newEnemyHP newGen

-- starts the game loop with the initial Game
game :: IO ()
game = do
    putStrLn ("Whats Your (Character) Name?")
    charName <- getLine
    initialGen <- newStdGen
    gameLoop Game { gameLocation = gameMap !! 53, gameCharacter = giveName charName, gameInventory = [], gameObjects = objectList }
    return ()

stringToDirection :: String -> Direction
stringToDirection s
    | s `elem` north = N
    | s `elem` east  = O
    | s `elem` south = S
    | s `elem` west  = W
    | s `elem` secret = SE
    | otherwise = error "Invalid direction string"

-- input that exits the game




-- ------------------- GAME ---------------------




