module Orm where

import Data.Text (unpack)
import Ormolu (defaultConfig, ormoluFile)

reformatFile :: String -> IO ()
reformatFile fileName = do
  txt <- ormoluFile defaultConfig (fileName :: String)
  writeFile fileName $ unpack txt
