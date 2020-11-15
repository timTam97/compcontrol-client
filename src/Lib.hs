{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where
-- import Data.List
-- import qualified Network.WebSockets.Client as WS

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

import           Control.Concurrent  (forkIO)
import System.IO
import           Control.Monad       (forever, unless)
import           Control.Monad.Loops (whileM_)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text, unpack)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Wuss as WWS
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode, defaultOptions)
import Data.Aeson.TH (deriveJSON, Options(fieldLabelModifier))
import qualified Data.ByteString.Lazy.Char8 as BL
import Debug.Trace

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

getToken :: IO String
getToken = do
    handle <- openFile "src\\token.txt" ReadMode
    hGetLine handle

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    forever $ do
        msg <- WS.receiveData conn
        let received = decode $ BL.pack $ unpack msg :: Maybe RecData
        case received of
            Just a -> undefined
        print received
        liftIO $ T.putStrLn msg

    WS.sendClose conn ("Bye!" :: Text)

mainLoop :: IO ()
mainLoop = do
    token <- getToken
    WWS.runSecureClient "stream.pushbullet.com" 443 ("/websocket/" <> token) app
