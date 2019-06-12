{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DiscordBindings where

import           Control.Monad       ((>=>))
import           Data.Aeson          (ToJSON, encode, toEncoding, (.=))
import           Data.Aeson.Types    (pairs)
import           GHC.Generics        (Generic)
import qualified HardCoded           as HC
import           Network.HTTP.Client (Request, RequestBody (RequestBodyLBS),
                                      parseRequest, requestBody, requestHeaders,
                                      setQueryString)
import           Network.HTTP.Types  (RequestHeaders)
import           WebRequest          (printResponse, secureRequest)

{- API Calls -}

postMessage :: ToJSON j => j -> String -> IO ()
postMessage content = sendRequest (\p -> p {requestBody = postPayload})
  where postPayload = reqBody (PostMsg content)

getMessages :: String -> IO ()
getMessages = sendRequest (setQueryString [("around", Just "587935253830565898")])

sendRequest :: (Request -> Request) -> String -> IO ()
sendRequest payload targetID
  =   payload . (\x -> x {requestHeaders = defHeader})
  <$> parseRequest (msgEndpoint targetID)
  >>= discordRequest

testChanID :: String
testChanID = "586730982476873760"

{- Request Builders -}
-- | Send and print a request via the Discord API
discordRequest :: Request -> IO ()
discordRequest = secureRequest >=> printResponse id

data MessageRequest msg = PostMsg msg | GetMsg msg
  deriving Generic

instance ToJSON m => ToJSON (MessageRequest m) where
  toEncoding (PostMsg m) = pairs ("content" .= m <> "tts" .= False)
  toEncoding (GetMsg _)  = undefined

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
msgEndpoint chanID = HC.discordURL ++ "channels/" ++ chanID ++ "/messages"
