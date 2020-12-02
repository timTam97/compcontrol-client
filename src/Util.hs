module Util
  ( writeLog,
    getWebSocketURI,
    processCommand,
    reformatFile
  )
where

import Data.Text (unpack)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime, ZonedTime)
import Ormolu (defaultConfig, ormoluFile)
import qualified Sys as SYS
import System.Environment (getEnv)
import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)

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

getWebSocketURI :: IO String
getWebSocketURI = getEnv "AWS_WSS_URI"

processCommand :: String -> IO ()
processCommand c =
  writeLog ("Command: " <> c)
    >> case c of
      "sleep" -> SYS.setSuspendState False
      "hibernate" -> SYS.setSuspendState True
      "shutdown" -> SYS.shutdownSystem
      "lock" -> SYS.lockWorkStation
