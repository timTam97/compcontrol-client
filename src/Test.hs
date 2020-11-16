{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Monad.IO.Class ()
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
  )
import Data.Aeson.TH ()
import Data.Aeson.Types (prependFailure, typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe ()
import Network.HTTP.Req ()
import System.Environment

data Push = Push {pushes :: [SubPushes]} deriving (Show)

data SubPushes = SubPushes
  { title :: String,
    body :: String,
    modified :: Double,
    dismissed :: Bool,
    sender_name :: String
  }
  deriving (Show)

instance FromJSON Push where
  parseJSON (Object v) =
    Push
      <$> v .: "pushes"
  parseJSON invalid =
    prependFailure
      "parsing Coord failed, "
      (typeMismatch "Object" invalid)

instance FromJSON SubPushes where
  parseJSON (Object v) =
    SubPushes
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
    2 -> writeIORef pot 50
  val <- readIORef pot
  print val

test2 :: IO ()
test2 = do
  tok <- getEnv "PB_TOKEN_USR"
  print tok
