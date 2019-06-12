{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BotBehavior where

import           Control.Concurrent
-- import           Control.Exception
import           Control.Monad              (when, (>=>))
import           Data.Aeson                 (FromJSON, Result (..), ToJSON,
                                             decode', encode, object,
                                             toEncoding, (.:), (.=))
import           Data.Aeson.Types           (pairs, parse)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.IORef
import           Data.Text                  (Text)
-- | TODO : Implement validations
-- import qualified Data.Validation            as Val
import qualified DiscordBindings            as DAPI (postMessage, getMessages)
import           GHC.Generics               (Generic)
import           Network.WebSockets         (Connection)
import           WebRequest                 (connectToGateway, wsClose,
                                             wsIncoming, wsSendTextMsg)

-- ^ NOTE : ASSUMING EVERYTHING IS SUCCESSFUL FOR NOW
runBot :: IO ()
runBot = connectToGateway bot


{- Bot -}
bot :: Connection -> IO ()
bot conn =  newIORef True   >>= initBot conn
         >> disconnect conn >>= print


closeInMinute :: Connection -> IORef Bool -> IO ()
closeInMinute conn isAlive
  =  threadDelay (5 * second) >> disconnect conn >> writeIORef isAlive False
  >> print ("Disconnecting" :: Text)


{- Response Parsers -}
getHeartbeat :: L8.ByteString -> Int
getHeartbeat h = maybe (-1) hasPulse (pulse <$> decode' h)
  where pulse  = parse ((.: "d") >=> (.: "heartbeat_interval"))
        hasPulse (Success n) =  n
        hasPulse _           = -1

{- Bot Functions -}
-- | Init bot connects, identifies with the server, and starts sending a
-- | heartbeat (converted from milliseconds to microseconds)
initBot :: Connection -> IORef Bool -> IO ()
initBot conn isAlive
  = wsIncoming conn >>= startHeart >> identify conn
  >> DAPI.getMessages "586730982476873760"
  >> closeInMinute conn isAlive
  where startHeart h =  forkIO (heartbeat conn (getHeartbeat h * 1000) isAlive)

-- | TODO : Convert to use direct encoding
identify :: Connection -> IO ()
identify conn = wsSendTextMsg conn (gatewayPayload Identify idProperties)
  where idProperties = object ["os"      .= ("Linux" :: Text),
                               "device"  .= ("HSBot" :: Text),
                               "browser" .= ("HSBot" :: Text) ]

heartbeat :: Connection -> Int -> IORef Bool -> IO ()
heartbeat conn interval isAlive
  =  threadDelay interval >> readIORef isAlive >>= stillAlive
  where stillAlive h =  when h (sendGateMsg conn Heartbeat ("" :: Text)
                                >> heartbeat conn interval isAlive    )

{- Time -}
second :: Int
second = 1000000

minute :: Int
minute =  60000000

sendGateMsg :: ToJSON d => Connection -> Opcode -> d -> IO ()
sendGateMsg gate opcode eventData =
  wsSendTextMsg gate (gatewayPayload opcode eventData)

disconnect :: Connection -> IO ()
disconnect conn = wsClose conn (gatewayPayload Heartbeat ("" :: Text))

{- Gateway Payload -}

data GatewayPayload d =
  GatewayPayload { -- | Opcode
                   op :: Int,
                   -- | Event data
                   d  :: d,
                   -- | Sequence number (Only present for Opcode 0)
                   s  :: Maybe Int,
                   -- | Event name (Only present for Opcode 0)
                   t  :: Maybe Text
  } deriving Generic

instance ToJSON d => ToJSON (GatewayPayload d) where
  toEncoding (GatewayPayload opcode eventData _ _) =
    pairs ("op" .= opcode <> "d" .= eventData)

instance FromJSON d => FromJSON (GatewayPayload d)

-- | gatewayPayload ceates and encodes gateway payloads
-- | Note: We are assuming that only valid opcodes are used
gatewayPayload :: ToJSON d => Opcode -> d -> L8.ByteString
gatewayPayload opcode eventData =
  encode GatewayPayload {op = opMap opcode, d = eventData,
                         s  = Nothing,      t = Nothing   }

{- Opcodes -}

opMap :: Opcode -> Int
opMap Dispatch       =  0
opMap Heartbeat      =  1
opMap Identify       =  2
opMap StatusUpdate   =  3
opMap Resume         =  6
opMap Reconnect      =  7
opMap InvalidSession =  9
opMap Hello          = 10
opMap HeartbeatACK   = 11

data Opcode = Dispatch       | Heartbeat | Identify
            | StatusUpdate   | Resume    | Reconnect
            | InvalidSession | Hello     | HeartbeatACK
