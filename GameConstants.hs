module GameConstants where

import GameTypes

quitCharacter :: [String]
quitCharacter = [":q", ":Q", ":e", ":E"]

quitFight :: [String]
quitFight     = ["flee","escape","run","Flee","Escape","Run"]

north :: [String]
north         = ["North","north","N"     ,"n"         ]

east :: [String]
east          = ["East" ,"east" ,"O"     ,"o" ,"E","e"]

south :: [String]
south         = ["South","south","S"     ,"s"         ]

west :: [String]
west          = ["West" ,"west" ,"W"     ,"w"         ]

secret :: [String]
secret        = ["game","dig","crawl","swim"          ]

invActions :: [String]
invActions    = ["show Inventory","show inventory","Inventory","inventory","show Items","show items","Items","items"]

-- ---------------------------------------------------------------------------
-- Benannte Objekt-IDs (A6: Magic Numbers aus dem Code gezogen).
-- Neue Content-Objekte nutzen ausschliesslich IDs >= 18, damit diese
-- Anker unangetastet bleiben.
-- ---------------------------------------------------------------------------
appleId   :: ObjectID
appleId   = 2

shieldId  :: ObjectID
shieldId  = 3

crystalId :: ObjectID
crystalId = 4

swordId   :: ObjectID
swordId   = 9

princessId :: ObjectID
princessId = 11

helpId    :: ObjectID
helpId    = 13

-- ---------------------------------------------------------------------------
-- Essen (A1/A2/C1): nur Objekte mit einer ID aus dieser Liste sind essbar.
-- Jedes Essen wird beim Verzehr aus dem Inventar entfernt - im Raum liegen
-- gelassene Aepfel heilen nicht (Befund 7 geschlossen).
-- ---------------------------------------------------------------------------
foodIds :: [(ObjectID, Life)]
foodIds =
  [ (2, 10)   -- Apple            (Garten, Startbestand)
  , (18, 6)   -- Berries          (Forest)
  , (19, 8)   -- Roots            (Forest)
  , (20, 10)  -- Mushroom         (Forest)
  , (21, 12)  -- Honeycomb        (Forest)
  , (28, 8)   -- Pale Fungus      (Catacombs)
  , (30, 14)  -- Sack of Grain    (Catacombs)
  ]

-- ---------------------------------------------------------------------------
-- Basiswerte (A7). Der Schild ist ein BONUS, kein Absolutwert - vorher
-- Def 10 gegen Atk 8, d. h. der Wolf konnte den Spieler gar nicht treffen.
-- ---------------------------------------------------------------------------
baseAttack  :: Ang
baseAttack  = 6

baseDefense :: Def
baseDefense = 1

shieldBonus :: Def
shieldBonus = 4

maxLife     :: Life
maxLife     = 30

-- Schaden pro Zug im Nebel (B5) - der Todesmarsch hat damit eine harte
-- Obergrenze von maxLife Zuegen, die nur gehortetes Essen verlaengert.
fogDamage   :: Life
fogDamage   = 1
