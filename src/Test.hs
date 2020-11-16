{-# LANGUAGE OverloadedStrings #-}

module Test where
import Data.IORef
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans (liftIO)
import Data.Aeson
-- import Data.Aeson (decode, defaultOptions)
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Network.HTTP.Req
import qualified Network.WebSockets as WS
import System.IO (IOMode (ReadMode), hGetLine, openFile)
import qualified Wuss as WWS
import Data.Maybe


data Push = Push {pushes :: [SubPushes]} deriving (Show)

data SubPushes = SubPushes {
  title :: String,
  body :: String,
  modified :: Double,
  dismissed :: Bool,
  sender_name :: String
} deriving (Show)

instance FromJSON Push where
  parseJSON (Object v) = Push
    <$> v .: "pushes"
  parseJSON invalid =
      prependFailure "parsing Coord failed, "
          (typeMismatch "Object" invalid)

instance FromJSON SubPushes where
  parseJSON (Object v) = SubPushes
    <$> v .: "title"
    <*> v .: "body"
    <*> v .: "modified"
    <*> v .: "dismissed"
    <*> v .: "sender_name"

getFile :: IO String
getFile = readFile "src\\test.txt"

test :: IO ()
test = do
  str <- getFile
  let ok = decode $ BL.pack str :: Maybe Push
  case ok of
    Just a -> print a
    Nothing -> print "bruh"

test1 :: IO ()
test1 = do
  pot <- newIORef 2
  -- val <- pot
  hmm <- readIORef pot
  case hmm of
    2 -> modifyIORef pot (+1)
  val <- readIORef pot
  print val