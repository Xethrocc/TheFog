module GameUtils where

import Data.Array
import Data.Char (isSpace, toLower)
import Data.List
import Data.Maybe
import GameTypes
import GameData
import GameConstants

-- ---------------------------------------------------------------------------
-- Locations
-- ---------------------------------------------------------------------------

-- Lookup nach `locId`, NICHT positionell. `gameMap` ist eine Liste und wurde
-- mit `!!` indiziert - das Entfernen eines Eintrags (z. B. "Errorfield")
-- haette alle nachfolgenden Location-IDs um eins verschoben.
locById :: Int -> Location
locById i = case find ((== i) . locId) gameMap of
              Just l  -> l
              Nothing -> Location { locId = i, locName = "Nowhere", locExits = [] }

locS :: Location -> LocString
locS = locName

-- (B3) Phasenabhaengig: `True` = Nebel-Flag. In der Nebel-Phase liefern
-- die Sonderorte aus `fogDescr` ihren eigenen Text, alle anderen den
-- generierten Nebeltext aus `describeExits`.
locD :: Bool -> Location -> String
locD fog loc
    | fog       = case lookup (locId loc) fogDescr of
                      Just t  -> t
                      Nothing -> genericFogText loc
    | otherwise = if inRange (bounds descr) (locId loc) then descr ! locId loc else ""

genericFogText :: Location -> String
genericFogText loc
    | null (exitNames loc) = "Dense, oppressive fog. You can make out no way out at all."
    | otherwise            = "Dense, oppressive fog. You can make out " ++ describeExits loc ++ "."

locI :: Direction -> Location -> Maybe Int
locI targetDir loc = find (\(_, dir) -> dir == targetDir) (locExits loc) >>= Just . fst

locA :: Location -> Int
locA = locId

getObjStr :: Object -> ObjString
getObjStr = objName

isActionPossible :: String -> Object -> Bool
isActionPossible x obj = x `elem` objActions obj

isObjHere :: Object -> Location -> Bool
isObjHere obj loc = objPos obj == locId loc

-- Get objects at location (including wolves)
getObjHere :: ObjectList -> [Wolf] -> Location -> String
getObjHere objList wolves loc = objObjects ++ objWolves
  where
    objObjects = getObjHereObjects objList loc
    objWolves = getWolfHere wolves loc

getObjHereObjects :: ObjectList -> Location -> String
getObjHereObjects [] _ = ""
getObjHereObjects (x:xs) loc = obj ++ (getObjHereObjects xs loc)
                              where
                               obj = if isObjHere x loc then (objName x) ++ "\n" else ""

getWolfHere :: [Wolf] -> Location -> String
getWolfHere [] _ = ""
getWolfHere (w:ws) loc = obj ++ (getWolfHere ws loc)
                         where
                          obj = if (wolfLoc w == locId loc) && not (wolfDead w)
                                   then (wolfIdToName (wolfId w)) ++ "\n" else ""

-- ---------------------------------------------------------------------------
-- Woelfe benennen
-- ---------------------------------------------------------------------------

-- (B7) Nebelwoelfe stehen still und patrouillieren nicht. "Patrolling Wolf
-- #100" waere in beiden Phasen falsch; sie heissen "Fog Wolf 1".."Fog Wolf 8".
wolfIdToName :: Int -> String
wolfIdToName n
    | n == guardianWolfId = "Wolf (Guardian)"
    | n == tutorialWolfId = "Wolf"
    | n == castleGuardId  = "Castle Guard"
    | n == cryptWardenId  = "Crypt Warden"
    | n >= 100            = "Fog Wolf " ++ show (n - 99)
    | otherwise           = "Patrolling Wolf #" ++ show (n - 40)

-- Alle Namen, unter denen ein Wolf auffindbar ist. Der Anzeigename ist
-- immer enthalten; Aliase kommen dazu.
wolfAliases :: Int -> [String]
wolfAliases n
    | n == guardianWolfId = [wolfIdToName n, "Guardian"]
    | n == castleGuardId  = [wolfIdToName n, "Guard"]
    | n == cryptWardenId  = [wolfIdToName n, "Warden"]
    | otherwise           = [wolfIdToName n]

-- Generische Bezeichner: "wolf"/"wolves" duerfen NUR einen Wolf im
-- aktuellen Raum treffen. Sonst wuerde "attack wolf" in Loc 3 den Guardian
-- in Loc 28 finden (Befund 8).
genericWolfNames :: [String]
genericWolfNames = ["wolf", "wolves", "wolve"]

isGenericWolfName :: String -> Bool
isGenericWolfName s = any (`elem` nameVariants s) genericWolfNames

-- ---------------------------------------------------------------------------
-- (C8) Namensaufloesung
-- ---------------------------------------------------------------------------
-- Feste Reihenfolge: exakt, dann lowercase, dann "the " weg, dann Singular.
-- "Abschneiden" passiert erst danach (in main.hs, das den Rest des Satzes
-- als Objektnamen uebernimmt).
--
-- "Princess" endet auf "s": ein vorgezogenes Abschneiden wuerde sie zu
-- "Princes". Deshalb faellt der Singular-Stufe ein Name auf "-ss" durch,
-- und "-ies" wird korrekt zu "-y".
-- base 4.18 kennt `Data.List.stripSuffix` noch nicht.
stripSuffix' :: String -> String -> Maybe String
stripSuffix' suf s
    | suf `isSuffixOf` s = Just (take (length s - length suf) s)
    | otherwise          = Nothing

singular :: String -> String
singular s
    | length s > 1, last s == 's', not ("ss" `isSuffixOf` s) =
        case stripSuffix' "ies" s of
            Just r | not (null r) -> r ++ "y"
            _                     -> init s
    | otherwise = s

dropThe :: String -> String
dropThe s = case stripPrefix "the " (map toLower s) of
              Just rest -> rest
              Nothing    -> s

nameVariants :: String -> [String]
nameVariants s = nub [ s, map toLower s, dropThe s, singular s
                     , map toLower (dropThe s), map toLower (singular s) ]

-- ---------------------------------------------------------------------------
-- Objekte finden
-- ---------------------------------------------------------------------------

-- (A5b) Der Location-Parameter greift AUSSCHLIESSLICH im generischen
-- Wolf-Zweig. Explizite Namen ("Castle Guard", "Patrolling Wolf #1")
-- matchen weiterhin orts unabhaengig - nur so bleibt die Meldung
-- "You can't do that here." fuer "attack Guardian" aus Loc 2 erhalten.
getObject :: String -> Location -> ObjectList -> [Wolf] -> Maybe Object
getObject x loc objList wolves =
    case getObjectInList x objList of
        Just obj -> Just obj
        Nothing -> findWolfByName x loc wolves

getObjectInList :: String -> ObjectList -> Maybe Object
getObjectInList x objList = go 0
  where
    xs = nameVariants x
    go i | i >= length xs = Nothing
         | otherwise =
            case find (\o -> xs !! i `elem` nameVariants (objName o)) objList of
                Just o  -> Just o
                Nothing -> go (i + 1)

findWolfByName :: String -> Location -> [Wolf] -> Maybe Object
findWolfByName x loc wolves =
    case find inRoom wolves of
        Just w  -> Just (wolfToObj w)
        Nothing -> case find namedAnywhere wolves of
            Just w  -> Just (wolfToObj w)
            Nothing -> Nothing
  where
    inRoom w = isGenericWolfName x && wolfLoc w == locId loc && not (wolfDead w)
    namedAnywhere w = not (isGenericWolfName x)
                      && any (\a -> any (`elem` nameVariants x) (nameVariants a))
                             (wolfAliases (wolfId w))

-- Woelfe bekommen ihren Text aus `wolfDescription`, nicht aus `objectText`:
-- ihre IDs liegen ausserhalb des Objekt-Arrays.
wolfToObj :: Wolf -> Object
wolfToObj w = Object
    { objPos = wolfLoc w
    , objId = wolfId w
    , objName = wolfIdToName (wolfId w)
    , objDescription = wolfDescription w
    , objActions = ["examine", "attack", "fight"]
    }

wolfDescription :: Wolf -> String
wolfDescription w
    | wolfId w == guardianWolfId =
        "A huge Wolf, standing over the shrine. It has not moved, and it shows no sign of intending to."
    | wolfId w == castleGuardId =
        "A Castle Guard, still wearing the armour of a household that no longer answers to anyone."
    | wolfId w == cryptWardenId =
        "The Crypt Warden. It was put down here to keep something in, and it is still doing the job."
    | wolfId w == tutorialWolfId =
        "A dark Wolf with glowing eyes. It watches your every move. Maybe you can 'attack' him.."
    | wolfId w >= 100 =
        "A Wolf made of fog. It does not move. It does not need to."
    | otherwise =
        "A patrolling Wolf, padding its endless round through the trees."

-- ---------------------------------------------------------------------------
-- Character
-- ---------------------------------------------------------------------------

getString :: Character -> String
getString = charName

getSteps :: Character -> Steps
getSteps = charSteps

getAtk :: Character -> Ang
getAtk = charAttack

getDef :: Character -> Def
getDef = charDefense

getHP :: Character -> Life
getHP = charLife

getMaxHP :: Character -> Life
getMaxHP = charMaxLife

-- (A3) Eine einzige Schadensformel fuer jeden Pfad: Biss, Wolfsrueckschlag
-- und Nebel. Vorher griff der Biss gar nicht auf `getDef` zu und ein
-- `Def 10` gegen `Atk 8` drueckte den Schaden permanent auf 0.
damage :: Int -> Int -> Int
damage raw def = max 0 (raw - def)

giveName :: String -> Character
-- (Fix 24) Leere/Whitespace-Namen bekommen den Standardnamen "Hero"
giveName x = Character { charName = if all isSpace x then "Hero" else x
                       , charSteps = 0
                       , charAttack = baseAttack
                       , charDefense = baseDefense
                       , charLife = maxLife
                       , charMaxLife = maxLife
                       }

addSteps :: Character -> Character
addSteps char = char { charSteps = charSteps char + 1 }

addAng :: Inventory -> Character -> Character
-- DESIGN (Autor): Der Protagonist ist NICHT der Held der Prophezeiung.
-- Das Schwert leuchtet fuer ihn nur kurz auf und erlischt (interactSword) -
-- es verleiht bewusst KEINEN Angriffsbonus. addAng bleibt wirkungslos.
addAng _ char = char

-- (A3) Der Schild ist ein BONUS auf den Basiswert, kein Absolutwert.
-- Vorher: charDefense = 10, wodurch kein Wolf den Spieler treffen konnte.
addDef :: Inventory -> Character -> Character
addDef inv char
    | any ((== shieldId) . objId) inv = char { charDefense = baseDefense + shieldBonus }
    | otherwise                       = char { charDefense = baseDefense }

-- ---------------------------------------------------------------------------
-- Essen
-- ---------------------------------------------------------------------------

isFood :: ObjectID -> Bool
isFood i = isJust (foodValue i)

-- Heilung pro Nahrungsmittel. Aepfel sind auf +10 fixiert, der Rest ist
-- bewusst gestaffelt: im Nebel verlaengert jeder Apfel den Todesmarsch um
-- zehn Zuege.
foodValue :: ObjectID -> Maybe Life
foodValue i = lookup i foodIds

foodName :: ObjectID -> String
foodName i = case lookup i foodIds of
               Just _  -> objNameById i
               Nothing -> "food"

-- (Punkt 4) Die Beute der Woelfe steht nicht in `objectList` - sie entsteht
-- erst mit dem Tod des Wolfs. Ohne sie hier waere `foodName` fuer jedes
-- Beutestueck auf "food" zurueckgefallen und der Ess-Satz haette
-- "You eat the food." gesagt.
objNameById :: ObjectID -> String
objNameById i = case filter ((== i) . objId) (objectList ++ dropFoodTemplates) of
                  (o:_) -> objName o
                  []    -> "food"
