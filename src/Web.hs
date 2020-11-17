{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web (mainLoop) where

import Control.Concurrent.Timeout (timeout)
import Control.Exception (try)
import Control.Monad (forever, when)
import Control.Monad.Loops (whileJust_)
import Control.Monad.Trans (liftIO)
import Data.Aeson
  ( Options (fieldLabelModifier),
    decode,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    defaultHttpConfig,
    header,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
  )
import qualified Network.WebSockets as WS
import Util
  ( currentUnixTime,
    fromJSONValue,
    getToken,
    processCommand,
  )
import qualified Wuss as WWS

data RecData = RecData {mainType :: String, subtype :: Maybe String}
  deriving (Show)

newtype Push = Push {pushes :: [SubPushes]}
  deriving (Show)

data SubPushes = SubPushes
  { title :: String,
    body :: String,
    modified :: Double,
    dismissed :: Bool,
    sender_name :: String
  }
  deriving (Show)

-- https://stackoverflow.com/questions/48474587/how-to-deal-with-haskells-reserved-keywords-in-record-fields
$( deriveJSON
     defaultOptions
       { fieldLabelModifier =
           \x -> if x == "mainType" then "type" else x
       }
     ''RecData
 )
$(deriveJSON defaultOptions ''Push)
$(deriveJSON defaultOptions ''SubPushes)

interrogateResponse :: Push -> Integer -> IO Integer
interrogateResponse p n =
  if title subPush == "LENOVO-TIM"
    && body subPush /= ""
    && not (dismissed subPush)
    then do
      processCommand $ body subPush
      pure $ floor $ modified subPush
    else pure n
  where
    subPush = head $ pushes p

grabPush :: Integer -> IO (Maybe Push)
grabPush t = do
  token <- getToken
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "api.pushbullet.com" /: "v2" /: "pushes")
        NoReqBody
        jsonResponse
        ( header "Access-Token" (BS.pack token)
            <> ("modified_after" =: (show t :: String))
            <> ("active" =: ("true" :: String))
            <> ("limit" =: (1 :: Integer))
        )
    -- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
    let parsed = fromJSONValue (responseBody r) :: Maybe Push
    liftIO $ print parsed
    pure parsed

analyzePing :: IORef Integer -> Text -> IO ()
analyzePing tme msg = do
  let received = decode $ BL.pack $ unpack msg :: Maybe RecData
  case received of
    Just a ->
      when (mainType a == "tickle") $ do
        numTime <- readIORef tme
        push <- grabPush numTime
        when (isJust push) $ do
          newTime <- interrogateResponse (fromJust push) numTime
          writeIORef tme newTime
  print received
  liftIO $ T.putStrLn msg

app :: WS.ClientApp ()
app conn = do
  time <- currentUnixTime
  recentTime <- newIORef time
  putStrLn "Connected!"
  whileJust_ (timeout 35000000 $ do WS.receiveData conn :: IO Text) (analyzePing recentTime)
  print "exiting"
  WS.sendClose conn ("Bye!" :: Text)

mainLoop :: IO ()
mainLoop = do
  token <- getToken
  forever $ do
    exc <- try (WWS.runSecureClient "stream.pushbullet.com" 443 ("/websocket/" <> token) app) :: IO (Either WS.ConnectionException ())
    case exc of
      Left a -> print $ "Caught exception " ++ show a ++ ", continuing"
      Right _ -> pure ()
