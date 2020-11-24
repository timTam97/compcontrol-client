module Util
  ( writeLog,
    currentUnixTime,
    getToken,
    fromJSONValue,
    processCommand,
    reformatFile,
    getComputerName
  )
where

import Data.Aeson.Types (FromJSON (parseJSON), Value, parseMaybe)
import Data.Text (unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime, ZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ormolu (defaultConfig, ormoluFile)
import qualified Sys as SYS
import System.Environment (getEnv)
import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)

getComputerName :: IO String
getComputerName = SYS.getComputerName

reformatFile :: String -> IO ()
reformatFile fileName = do
  txt <- ormoluFile defaultConfig fileName
  writeFile fileName $ unpack txt

logTimeStr :: ZonedTime -> String
logTimeStr zt = "[" <> str <> "] "
  where str = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zt

writeLog :: String -> IO ()
writeLog str = do
  file <- openFile ".\\run.log" AppendMode
  now <- getZonedTime
  let time = logTimeStr now
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
