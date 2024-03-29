{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web (mainLoop) where

import Control.Exception (try)
import Control.Monad (forever, when)
import Control.Monad.Loops (whileJust_)
import Data.Aeson
  ( Options (fieldLabelModifier),
    decode,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, unpack)
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (defaultConnectionOptions)
import System.Timeout (timeout)
import Util (getWebsocketToken, processCommand, writeLog)
import qualified Wuss as WWS

data RecData = RecData {mainType :: String, subtype :: Maybe String}
  deriving (Show)

-- https://stackoverflow.com/questions/48474587/how-to-deal-with-haskells-reserved-keywords-in-record-fields
$( deriveJSON
     defaultOptions
       { fieldLabelModifier =
           \x -> if x == "mainType" then "type" else x
       }
     ''RecData
 )

analyzePing :: Text -> IO ()
analyzePing msg =
  case received of
    Just a ->
      when
        ( mainType a == "command"
            && isJust (subtype a)
            && fromJust (subtype a) `elem` ["sleep", "hibernate", "lock", "shutdown"]
        )
        $ processCommand $ fromJust (subtype a)
  where
    received = decode $ BL.pack $ unpack msg :: Maybe RecData

app :: WS.ClientApp ()
app conn =
  writeLog "Connected"
    >> whileJust_
      (timeout 65000000 $ do WS.receiveData conn)
      analyzePing
    >> writeLog "Exiting"
    >> WS.sendClose conn ("Bye!" :: Text)

mainLoop :: IO ()
mainLoop = do
  forever $ do
    token <- getWebsocketToken
    exc <-
      try $
        WWS.runSecureClientWith
          "wss.timsam.au"
          443
          "/"
          defaultConnectionOptions
          [("auth", BS.pack token)]
          app ::
        IO (Either WS.ConnectionException ())
    case exc of
      Left a -> writeLog $ "Caught exception " ++ show a ++ ", restarting"
      Right _ -> writeLog "Restarting connection"
