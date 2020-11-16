module Util where

import Data.Aeson.Types (FromJSON (parseJSON), Value, parseMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Sys as SYS
import System.IO (IOMode (ReadMode), hGetLine, openFile)

currentUnixTime :: IO Integer
currentUnixTime = do floor <$> getPOSIXTime

getToken :: IO String
getToken = do
  handle <- openFile "..\\token.txt" ReadMode
  hGetLine handle

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

processCommand :: String -> IO ()
processCommand c =
  case c of
    "sleep" -> SYS.setSuspendState False
    "hibernate" -> SYS.setSuspendState True
    "shut down" -> undefined
    "lock" -> SYS.lockWorkStation
