{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DiscordBindings where

import           Control.Monad       ((>=>))
import           Data.Aeson          (ToJSON, encode, toEncoding, (.=))
import           Data.Aeson.Types    (pairs)
import           GHC.Generics        (Generic)
import qualified HardCoded           as HC
import           Network.HTTP.Client (Request, RequestBody (RequestBodyLBS),
                                      method, parseRequest, requestBody,
                                      requestHeaders)
import           Network.HTTP.Types  (RequestHeaders)
import           WebRequest

{- API Calls -}

-- | Send a message over Discord
postMessage :: ToJSON j => j -> String -> IO ()
postMessage content targetID
  =   postMsgPayload content <$> parseRequest (msgEndpoint targetID)
  >>= discordRequest

-- | Get messages

{- Request Builders -}

-- | Send and print a request via the Discord API
discordRequest :: Request -> IO ()
discordRequest = secureRequest >=> printResponse id

data MessageRequest msg = GetMsg msg | PostMsg msg
  deriving Generic

instance ToJSON m => ToJSON (MessageRequest m) where
  toEncoding (PostMsg m) = pairs ("content" .= m <> "tts" .= False)
  toEncoding _           = undefined

{- Payloads -}

postMsgPayload :: ToJSON j => j -> Request -> Request
postMsgPayload content req = req {method         = "POST"                   ,
                                  requestBody    = reqBody (PostMsg content),
                                  requestHeaders = defHeader                 }

{- Encoders -}

reqBody :: ToJSON j => MessageRequest j -> RequestBody
reqBody =  RequestBodyLBS . encode

{- Misc -}
-- | Default header data
-- | Note: I doubt I'll ever change this, so I'm just gonna write this in everywhere!
defHeader :: RequestHeaders
defHeader = [("User-Agent"   , "DiscordBot (N/A, 0.1)"),
             ("Authorization", HC.botToken)            ,
             ("Content-Type" , "application/json")     ]


msgEndpoint :: String -> String
msgEndpoint chanID = HC.discordURL ++ "channels/" ++ chanID ++ "/messages/"
