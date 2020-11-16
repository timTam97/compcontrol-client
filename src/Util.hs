module Util where

import Data.Aeson.Types (FromJSON (parseJSON), Value, parseMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Sys as SYS
import System.Environment (getEnv)

currentUnixTime :: IO Integer
currentUnixTime = floor <$> getPOSIXTime

getToken :: IO String
getToken = getEnv "PB_TOKEN_SYS"

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

processCommand :: String -> IO ()
processCommand c =
  case c of
    "sleep" -> SYS.setSuspendState False
    "hibernate" -> SYS.setSuspendState True
    "shut down" -> SYS.shutdownSystem
    "lock" -> SYS.lockWorkStation
