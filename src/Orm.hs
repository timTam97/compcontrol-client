module Orm where

import Ormolu
import Data.Text


doThing :: String -> IO ()
doThing fileName = do 
    txt <- ormoluFile defaultConfig (fileName :: String)
    writeFile fileName $ unpack txt