# NMIS parser
 - NMIS stands for Network Management Information System
   Read about it on [wikipedia](https://en.wikipedia.org/wiki/NMIS).

#### Example usage :

```haskell
  module Main where

  import Data.Either
  import System.Environment (getArgs)
  import System.IO
  import Text.Megaparsec
  import Text.Nmis

main :: IO ()
main = getArgs >>= parseArgs
  where
    parseArgs [] = putStrLn "error: you need to pass in the file path"
    parseArgs (path:_) = do
      contents <- readFile path
      either (print . parseErrorPretty) print (parse parseNmis "" contents)

```


| Nmis fields                     |
| --------------------------------|
| active                          |
| authkey                         |
| authpassword                    |
| authprotocol                    |
| businessService                 |
| accallstive                     |
| aucbqosthkey                    |
| aucollectthpassword             |
| aucommunitythprotocol           |
| bucontextsinessService          |
| cacustomerlls                   |
| cbdependqos                     |
| codisplay_namellect             |
| cogroupmmunity                  |
| cohostntext                     |
| culocationstomer                |
| demax_msg_sizepend              |
| dimax_repetitionssplay_name     |
| grmodeloup                      |
| honamest                        |
| lonetTypecation                 |
| manotesx_msg_size               |
| mapingx_repetitions             |
| moportdel                       |
| naprivkeyme                     |
| neprivpasswordtType             |
| noprivprotocoltes               |
| piremote_connection_nameng      |
| poremote_connection_urlrt       |
| prroleTypeivkey                 |
| prserviceStatusivpassword       |
| prservicesivprotocol            |
| rethresholdmote_connection_name |
| retimezonemote_connection_url   |
| rousernameleType                |
| seuuidrviceStatus               |
| seversionrvices                 |
| thwebserverreshold              |
| tiwmipasswordmezone             |
| uswmiusernameername             |
| uuid                            |
| version                         |
| webserver                       |
| wmipassword                     |
| wmiusername                     |



You can find this library on [hackage](https://hackage.haskell.org/package/nmis-parser-0.1.0.0)
