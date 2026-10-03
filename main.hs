import Ascii
import System.Console.ANSI
import GameTypes
import GameData
import GameActions
import GameUtils
import GameConstants
import Data.Maybe (isJust)
import Data.Char (toLower)
import System.IO (isEOF)
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
        in (replicate filled '\9608') ++ (replicate empty ' ')

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
-- Wolf-Bewegung, kein Schritt).
--
-- (A4) Ein Zug = Bewegung -> `advanceTurn`: Schritt zaehlen, Wölfe bewegen,
-- im Nebel 1 LP abziehen. `spawnPostFogWolves` ist hier raus (A6) und wird
-- nur noch beim Nebel-Uebergang aufgerufen (B2).
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
                let moved = advanceTurn game { gameLocation = locById nextLocId }
                putStrLn ""
                -- (Fix 22) Woelfe sind gefaehrlich: Biss beim Betreten moeglich
                maybeBite moved

-- (Fix 22) Beim Betreten einer Location mit (lebendem) Wolf: 50% Chance
-- auf einen Biss (roh 1-3).
-- (A3) Der Biss laeuft jetzt durch `damage` und benutzt `getDef`. Vorher
-- wurden die Verteidigung des Helden komplett ignoriert.
maybeBite :: Game -> IO Game
maybeBite game
    | any (\w -> wolfLoc w == locId (gameLocation game) && not (wolfDead w)) (gameWolves game) = do
        chance <- roll 0 1
        if chance == (1 :: Int)
            then do
                raw <- roll 1 3
                let char = gameCharacter game
                    dmg = damage raw (getDef char)
                putStrLn ("A wolf lunges from the shadows and bites you for " ++ show dmg ++ "!\n")
                return game { gameCharacter = char { charLife = max 0 (getHP char - dmg) } }
            else return game
    | otherwise = return game

-- (AUTOR-DESIGN) Die Prinzessin überlebt die Rettung nur in 1/3 der Fälle
-- (2/3 ertrinken). Bei Erfolg verschwindet sie aus der Objektliste; nach
-- ihrem Tod bleibt sie liegen, weitere Rettungsversuche sind blockiert.
handlePrincessSave :: Game -> Object -> IO Game
handlePrincessSave game obj = do
    r <- roll 1 3
    if r == (1 :: Int)
        then do
            putStrLn "You dive into the river and guide the princess to safety!\n"
            return game { gamePrincess = PrincessSaved
                        , gameObjects = deleteByObjId (objId obj) (gameObjects game) }
        else do
            putStrLn "You tried to save the princess but she drowned!\n"
            return game { gamePrincess = PrincessDead }

-- (Fix 5) Crystal-Check: geprueft wird, ob der Kristall im Inventar liegt -
-- nicht, ob das Shrine-Objekt selbst dort ist (war immer False).
handleActivate :: Game -> Object -> IO Game
handleActivate game obj = do
    let hasCrystal = any ((== crystalId) . objId) (gameInventory game)
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

-- (Fix 9/20) Inventar-Boni werden auf den Charakter angewendet.
-- (A3) `addDef` liefert jetzt Basis + Bonus (1 + 4 = 5 mit Schild) statt
-- den Absolutwert 10 zu setzen. `addAng` bleibt wirkungslos - siehe
-- GameUtils.addAng.
handleTake :: Game -> Object -> IO Game
handleTake game obj = do
    let (updatedObjList, updatedInv) = takeObj obj (gameObjects game) (gameInventory game)
        updatedChar = addDef updatedInv (addAng updatedInv (gameCharacter game))
    putStrLn "You picked it up, it's in your Inventory\n"
    pause
    return game { gameObjects = updatedObjList
                , gameInventory = updatedInv
                , gameCharacter = updatedChar }

-- (Fix 10) Gegner-HP aus den echten Wolf-Daten, inkl. Max-HP fuer die
-- Balken. Vorher war `enemyHP` fest auf 12 und der Balken fest auf 30.
handleFight :: Game -> Object -> IO Game
handleFight game obj = do
    let homeLoc = gameLocation game
        (enemyHP, enemyMaxHP) = wolfFullStats (gameWolves game) (objId obj)
        fightGame = game { gameLocation = Location { locId = 54
                                                   , locName = "Wolf-Fight"
                                                   , locExits = [] } }
    result <- fightLoop fightGame obj enemyHP enemyMaxHP
    return result { gameLocation = homeLoc }

-- (Fix 9) Wuerfel-Helfer fuer variablen Schaden
roll :: Int -> Int -> IO Int
roll lo hi = do
    gen <- newStdGen
    return (fst (randomR (lo, hi) gen))

-- Kampfsystem.
--
-- (A3) Beide Seiten laufen durch dieselbe Schadensformel `damage`. Die
--      Verteidigung des Wolfs wird benutzt (vorher mit `_` verworfen), und
--      der Spieler verursacht mindestens 1 Schaden, damit ein hoher
--      Schutzwert keinen Endlos-Kampf erzeugt.
--
-- (A4) Jede Kampf-Runde und jedes `flee` kosten einen Zug (`advanceTurn`).
--      Bewusst KEIN Off-Screen-Biss: sonst waere der Guardian-Kampf bei
--      ~6 Runden nicht ueberlebbar, weil Wolf 42 ueber Loc 17 mitlaeuft.
--
-- (B5) Im Nebel zieht `advanceTurn` zusaetzlich 1 LP ab - auch mitten im
--      Kampf. Der Nebel-Todesmarsch laesst sich also nicht auskämpfen.
fightLoop :: Game -> Object -> Life -> Life -> IO Game
fightLoop game@Game{gameLocation = location, gameCharacter = character} object enemyHP enemyMaxHP = do
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________\n")
    ascii (locA location)

    putStrLn ("                    You are fighting a " ++ (getObjStr object))
    putStrLn ("\n                    " ++ locD (fogActive game) location ++ "\n\n")
    putStrLn ("\nYour Atk: " ++ show(getAtk character))
    putStrLn ("Your Def: " ++ show(getDef character))
    putStrLn ("\nYour HP:  " ++ show(getHP character) ++ " [" ++ (drawBar (getHP character) (getMaxHP character)) ++ "]")
    putStrLn ("Your Steps:   "  ++ show(getSteps character))
    putStrLn ("\nHP of the " ++ (getObjStr object) ++ ": " ++ show(enemyHP) ++ " [" ++ (drawBar enemyHP enemyMaxHP) ++ "]")
    putStrLn ("\n\nYou can 'attack' or try to 'flee'..\n What will u do?")
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")

    input <- getLine
    -- Der Kampf liest das Verb genauso wie die Hauptschleife. Vorher stand
    -- hier `input elem ["attack","hit"]` gegen die GANZE Zeile, d. h.
    -- "attack wolf" wurde im Kampf als Zögern gewertet, obwohl genau das
    -- der Befehl ist, den der Bildschirm nahelegt.
    let verb = map toLower (takeWhile (/= ' ') (dropWhile (== ' ') input))

    -- `quitFight` enthaelt bereits die kleingeschriebenen Formen; die
    -- `verb` ist kleingeschrieben, also direkt vergleichen.
    if verb `elem` quitFight
        then do
            putStrLn "You fled from the fight!\n"
            -- (A4) Flucht kostet einen Zug, ist aber nie blockiert -
            -- der Anti-Softlock gegen mehrere Wölfe im Raum.
            return (advanceTurn game)
        else if verb `elem` ["attack", "hit"]
            then do
                pBonus <- roll 0 2
                let (wAtk, wDef) = wolfCombatStats (gameWolves game) (objId object)
                    playerDmg = max 1 (getAtk character + pBonus - wDef)
                    newEnemyHP = enemyHP - playerDmg
                if newEnemyHP <= 0
                    then do
                        putStrLn "You Won! Press 'Enter' to continue"
                        _ <- getLine
                        let updatedObjList = attackObj object (gameObjects game)
                            killWolf w = if wolfId w == objId object then w { wolfDead = True } else w
                        return game { gameObjects = updatedObjList
                                    , gameWolves = map killWolf (gameWolves game) }
                    else do
                        wBonus <- roll 0 2
                        let wolfDmg = damage (wAtk + wBonus) (getDef character)
                            newHP = getHP character - wolfDmg
                            counter = if wolfDmg == 0
                                        then "The " ++ getObjStr object ++ " strikes, and the shield holds."
                                        else "The " ++ getObjStr object ++ " hits back for " ++ show wolfDmg ++ "!"
                        putStrLn ("You hit for " ++ show playerDmg ++ "! " ++ counter ++ "\n")
                        if newHP <= 0
                            then return game { gameCharacter = character { charLife = 0 } }
                            else do
                                let after = advanceTurn (game { gameCharacter = character { charLife = newHP } })
                                if getHP (gameCharacter after) <= 0
                                    then return after
                                    else fightLoop after object newEnemyHP enemyMaxHP
            else do
                putStrLn "You hesitate... (You can 'attack' or 'flee')\n"
                fightLoop game object enemyHP enemyMaxHP

-- (Fix 9/22) Tod-Check zentral im gameLoop: gilt fuer Kampf- und Biss-Tod.
-- (B6) Der Nebel-Tod hat einen eigenen Screen, weil inhaltlich etwas
-- anderes passiert. Der Basis-Tod bleibt unveraendert.
gameLoop :: Game -> IO ()
gameLoop game
    | getHP (gameCharacter game) <= 0 =
        if fogActive game
            then fogDeath (getSteps (gameCharacter game))
            else do
                putStrLn "Your vision fades... you collapse. You have died."
                putStrLn "Game Over."
    | otherwise = gameLoop' game

-- (B6) Keine Aufloesung, kein Rueckweg, kein "du haettest".
fogDeath :: Steps -> IO ()
fogDeath steps = do
    putStrLn "The fog closes over you. It does not lift."
    putStrLn "You cannot tell which way the shrines are any more, or whether you ever reached them."
    putStrLn ("You took " ++ show steps ++ " steps into the fog. Nobody else counted them.")
    putStrLn "Game Over."

-- (Fix 1/3/4) Das Spiel-Herzstueck: zeigt die Location an, liest die Eingabe
-- und delegiert an `processInput`.
-- (B8) Die Wolf-Warnung ist im Nebel deutlich haeufiger. Bewusst
-- unveraendert - sie macht den Todesmarsch lesbar.
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
    putStrLn ("\n                    " ++ locD (fogActive game) location ++ "\n\n")

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
    putStrLn ("Your HP:      "  ++ show(getHP character) ++ " / " ++ show(getMaxHP character))
    setSGR [Reset]
    putStrLn ("________________________________________________________________________________")
    putStrLn ("________________________________________________________________________________")
    input <- getLine

    -- Ohne diese Pruefung dreht sich das Spiel endlos: `getLine` liefert am
    -- EOF einen leeren String, und leere Eingabe bedeutet "nur neu zeichnen".
    eof <- isEOF
    if eof then putStrLn "\nBye!"
    -- (B2) Der `checkEnding`-Check vor `processInput` war redundant: die
    -- activate-Branch greift bereits. `fogEnding` haengt dort an.
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
                               else getObject objectName (gameLocation game) (gameObjects game ++ gameInventory game) (gameWolves game)
        in case targetObject of
            Nothing -> do
                putStrLn "You can't do that. (Type 'read Help' for a list of commands.)\n"
                gameLoop game
            Just obj -> performAction game actionStr obj

-- (Fix 1/3/4) Objekt-Aktionen: Ergebnisse der Handler werden verkettet
-- (>>= gameLoop), sodass Zustandserweiterungen erhalten bleiben.
--
-- (Befund 9) `help` war auf `saveAction` gemappt. Weil "help" in den
-- objActions der Prinzessin stand, loeste `help Princess` einen
-- Rettungswurf aus - die Prinzessin ertrank bei einem Hilferuf. `help`
-- gibt jetzt immer die Hilfe aus, unabhaengig vom Objekt.
performAction :: Game -> String -> Object -> IO ()
performAction game actionStr obj
    -- (A2) Essen laeuft weiterhin VOR dem isObjHere-Check, verlangt aber
    -- jetzt eine Inventar-Mitgliedschaft. Vorher heilte `eat Apple` in der
    -- Garden endlos, ohne dass der Apfel verbraucht wurde (er lag in
    -- gameObjects, wurde aber nur aus gameInventory geloescht).
    | actionStr `elem` ["consume", "eat"] = eatFood
    | actionStr == "help" = doHelp
    | not (isActionPossible actionStr obj) = cantDo
    -- Objekte im Inventar sind überall benutzbar, nicht nur im Raum, in dem
    -- sie aufgenommen wurden. Vorher war `read Help` ausserhalb des Start-
    -- menues unmoeglich, und der Schild/apfel waren nach dem Einsammeln
    -- nicht mehr benennbar.
    | not (isObjHere obj (gameLocation game))
    , not (any ((== objId obj) . objId) (gameInventory game)) = cantDo
    | otherwise = case actionStr of
        "read" -> readObj obj >> pause >> gameLoop game
        "examine" ->
            if objId obj == swordId
                then interactSword obj >> pause >> gameLoop game
                else examineObj obj >> pause >> gameLoop game
        "save" -> saveAction
        "take" -> handleTake game obj >>= gameLoop
        "pick" -> handleTake game obj >>= gameLoop   -- Alias, siehe objActions
        "activate" -> do
            g <- handleActivate game obj
            if checkEnding (gameShrineFlags g)
                then fogEnding g >>= gameLoop   -- (Original-Design) alle 4 Shrines = der Nebel faellt
                else gameLoop g
        "attack" -> handleFight game obj >>= gameLoop
        "fight" -> handleFight game obj >>= gameLoop
        "hit" -> handleFight game obj >>= gameLoop   -- Alias
        _ -> doNothinSimple obj >> gameLoop game
  where
    cantDo = do
        putStrLn "You can't do that here.\n"
        gameLoop game

    doHelp = case getObjectInList "help" (gameObjects game ++ gameInventory game) of
        Just h  -> readObj h >> pause >> gameLoop game
        Nothing -> putStrLn "There is no help here.\n" >> gameLoop game

    eatFood
        | isFood (objId obj)
        , any ((== objId obj) . objId) (gameInventory game)
        , Just heal <- foodValue (objId obj) = do
            let c = gameCharacter game
                newHP = min (getMaxHP c) (getHP c + heal)
                g2 = game { gameInventory = deleteByObjId (objId obj) (gameInventory game)
                          , gameCharacter = c { charLife = newHP } }
            putStrLn ("You eat the " ++ foodName (objId obj) ++ ". It restores "
                      ++ show (newHP - getHP c) ++ " of your health.\n")
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

-- ---------------------------------------------------------------------------
-- (B2) Der Nebel-Uebergang.
--
-- (Original-Design) Alle vier Shrines aktiv = der Nebel senkt sich ueber das
-- Land. Das bleibt das Ende. Neu ist, dass es einen spielbaren Epilog gibt
-- statt eines sofortigen "Game Over".
--
-- Der Uebergangstext benennt den Spieler als Ursache - das ist die Pointe
-- der Tragik-Komodie: er hat sich fuer den Helden der Prophezeiung gehalten
-- und am Ende den Nebel gerufen, vor dem die Prophezeiung warnte.
--
-- Wichtig: Der Text verspricht KEINE Aufloesung. Er richtet sich an dem
-- Wortlaut der Prophezeiung aus, der gerade NICHT behauptet, ein
-- Schwert-Held koenne den Nebel zurueckschlagen - er sagt nur, dass ein
-- Nebel kommen wird.
-- ---------------------------------------------------------------------------
fogEnding :: Game -> IO Game
fogEnding game = do
    putStrLn "The fourth shrine goes quiet under your hand."
    putStrLn ""
    putStrLn "Then the fog comes. It does not creep in. It is simply there, the way it was always going to be."
    putStrLn ""
    putStrLn "The prophecy never said a hero would turn it back. It only said that a fog would set over this land,"
    putStrLn "and it never said who would be standing here when it did."
    putStrLn ""
    putStrLn "You were never the one it was waiting for. You are the reason it is here."
    putStrLn ""
    pause
    putStrLn "The air goes grey. Out in it, something moves."
    putStrLn "Then something else. Then a lot of something."
    putStrLn ""
    putStrLn "From here on, time is not free. Every step you take and every round you fight, the fog takes a little more of you."
    pause
    return game { gameWolves = spawnPostFogWolves (gameWolves game) (gameShrineFlags game) }

game :: IO ()
game = do
    putStrLn ("Whats Your (Character) Name?")
    charName <- getLine
    gameLoop Game { gameLocation = locById 53
                  , gameCharacter = giveName charName
                  , gameInventory = initialInventory
                  , gameObjects = objectList
                  , gameWolves = initialWolves
                  , gameShrineFlags = initialShrineFlags
                  , gamePrincess = initialPrincessStatus }
    return ()
