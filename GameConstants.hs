module GameConstants where

quitCharacter = [":q", ":Q", ":e", ":E"] 
quitFight     = ["flee","escape","run","Flee","Escape","Run"]
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