module Util
  ( writeLog,
    currentUnixTime,
    getToken,
    fromJSONValue,
    processCommand,
  )
where

import Data.Aeson.Types (FromJSON (parseJSON), Value, parseMaybe)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Sys as SYS
import System.Environment (getEnv)
import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)

logTimeStr :: IO String
logTimeStr = do
  now <- getZonedTime
  let str = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  pure $ "[" <> str <> "] "

writeLog :: String -> IO ()
writeLog str = do
  file <- openFile ".\\run.log" AppendMode
  time <- logTimeStr
  hPutStrLn file $ time <> str
  hClose file

currentUnixTime :: IO Integer
currentUnixTime = floor <$> getPOSIXTime

getToken :: IO String
getToken = getEnv "PB_TOKEN_SYS"

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

processCommand :: String -> IO ()
processCommand c =
  writeLog ("Command: " <> c)
    >> case c of
      "sleep" -> SYS.setSuspendState False
      "hibernate" -> SYS.setSuspendState True
      "shut down" -> SYS.shutdownSystem
      "lock" -> SYS.lockWorkStation
