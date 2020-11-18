module Util
  ( writeLog,
    currentUnixTime,
    getToken,
    fromJSONValue,
    processCommand,
    reformatFile,
    getUserName
  )
where

import Data.Aeson.Types (FromJSON (parseJSON), Value, parseMaybe)
import Data.Text (unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ormolu (defaultConfig, ormoluFile)
import qualified Sys as SYS
import System.Environment (getEnv)
import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)

getUserName :: IO String
getUserName = SYS.getUserName

reformatFile :: String -> IO ()
reformatFile fileName = do
  txt <- ormoluFile defaultConfig (fileName :: String)
  writeFile fileName $ unpack txt

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
