module Ascii where

import System.Console.ANSI

ascii :: Int -> IO ()
ascii 0 = putStrLn (("                     ▄▄▄█████▓ ██░ ██ ▓█████             
                    ▓  ██▒ ▓▒▓██░ ██▒▓█   ▀             
                    ▒ ▓██░ ▒░▒██▀▀██░▒███               
                    ░ ▓██▓ ░ ░▓█ ░██ ▒▓█  ▄             
                      ▒██▒ ░ ░▓█▒░██▓░▒████▒            
                      ▒ ░░    ▒ ░░▒░▒░░ ▒░ ░            
                        ░     ▒ ░▒░ ░ ░ ░  ░            
                      ░       ░  ░░ ░   ░                       
                              ░  ░  ░   ░  ░                     
                                                   
                                 █████▒▒█████    ▄████ 
                               ▓██   ▒▒██▒  ██▒ ██▒ ▀█▒
                               ▒████ ░▒██░  ██▒▒██░▄▄▄░
                               ░▓█▒  ░▒██   ██░░▓█  ██▓
                               ░▒█░   ░ ████▓▒░░▒▓███▀▒
                                ▒ ░   ░ ▒░▒░▒░  ░▒   ▒ 
                                ░       ░ ▒ ▒░   ░   ░ 
                                ░ ░   ░ ░ ░ ▒  ░ ░   ░ 
                                          ░ ░        ░ 
                                                   
"))
ascii 1 = putStrLn (("
 ____________________________________________________________________
|    The map shows you the most important places in the country.   |
|                                                                  |
| The forests in the north, formerly called the forest of Elwyn.   |
|      In the east, the Great Castle Minas..something.             |
|               A Grand Canyon in the south.                       |
|   In the west is a big mountain, it's called Mountain Doom.      |
|                                                                  |
'------------------------------------------------------------------'
                                `:++++++//+++o`                     
                             .++/. `  ``  .yy+y                     
                           `s/                :/s:                  
                          `h.     --FOREST--    :s:                   
                          s:                      -o+-                
                         -y                         :+++/:.          
                        -d______    :` `-`                .:/++++/`   
                      `ooy Grave|   o                            `+o` 
   ````./+++///////++os` | yard |`                                  `h`
    :++:`                |o`o-.s|    `              `      CASTLE   o:
  +o.   ____`           `|______|                                 .o+ 
 y:    /    \\`                   `                              :o/`  
`h    /      \\                               .~~~~~.       .:/++:     
 y`  /  MOUNTAIN          `   |-----|    `  - RIVER -  .+++:`        
 .s`/__________\\              |HOUSE|        '~~~~~' :++:             
  ++                          |_____|   ``        +o.               
   /o`                       `                   o+                   
    .o/                                   `    .s:                    
      -s-                            ```     .o+                     
        :o/                    `##########+o+`                     
          .+o.        #####--CANYON--#### /s`                       
             /+++###############:.       -s:                           
                ./s.```               `o+                            
                  `//+o-             :s.  `                           
                       /y`        `+o-                                
                        `/++:--//+/`                                  
                            `..`                                      "))
ascii 54 = putStrLn (("
     .---.
    |  _  |
    | (o o)
   .\\   /


    \\_/

 |   
   |____|

            .  .
         .  |  .
      .  |  |  .
     |   |  |   |
     |   |  |   |
      \\  |  /
        \\|/
         |

"))
ascii i = putStrLn ("")
