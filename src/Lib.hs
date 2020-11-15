{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where
-- import Data.List
-- import qualified Network.WebSockets.Client as WS

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

import System.IO ( IOMode(ReadMode), hGetLine, openFile )
import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as BS
import           Data.Text           (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO        as T

import qualified Network.WebSockets  as WS
import qualified Wuss as WWS

import Control.Monad.IO.Class
import Data.Aeson
-- import Data.Aeson (decode, defaultOptions)
import Data.Aeson.TH (deriveJSON, Options(fieldLabelModifier))


import Data.Time.Clock.POSIX
import Network.HTTP.Req

data RecData = RecData { mainType :: String, subtype :: Maybe String }
    deriving (Show)

-- https://stackoverflow.com/questions/48474587/how-to-deal-with-haskells-reserved-keywords-in-record-fields
$( deriveJSON
     defaultOptions
       { fieldLabelModifier =
           \x -> if x == "mainType" then "type" else x
       }
     ''RecData
 )


currentUnixTime :: IO Integer
currentUnixTime = do floor <$> getPOSIXTime

getToken :: IO String
getToken = do
    handle <- openFile "src\\token.txt" ReadMode
    hGetLine handle
grabPush :: IO ()
grabPush = do
    currTime <- currentUnixTime
    token <- getToken
    runReq defaultHttpConfig $ do
        r <- req
            GET 
            (https "api.pushbullet.com" /: "v2" /: "pushes")
            NoReqBody
            jsonResponse
            (header "Access-Token" (BS.pack token) 
                <> ("modified_after" =: (show (currTime - 50000) :: String))
                <> ("active" =: ("true" :: String)))
        liftIO $ print (responseBody r :: Value)
    

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    forever $ do
        msg <- WS.receiveData conn
        let received = decode $ BL.pack $ unpack msg :: Maybe RecData
        case received of
            Just a -> if mainType a == "tickle"
                then grabPush 
                else undefined

        print received
        liftIO $ T.putStrLn msg

    WS.sendClose conn ("Bye!" :: Text)

mainLoop :: IO ()
mainLoop = do
    token <- getToken
    WWS.runSecureClient "stream.pushbullet.com" 443 ("/websocket/" <> token) app
