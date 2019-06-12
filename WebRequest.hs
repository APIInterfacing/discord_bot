--  Deal with any / all network request stuff here
module WebRequest where

import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, putStrLn)
import qualified HardCoded                  as HC -- ^ TODO : REMOVE
import qualified Network.HTTP.Client        as HTTP (Request, Response, httpLbs,
                                                     newManager, responseBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.WebSockets         (ClientApp, Connection, receiveData,
                                             sendClose, sendTextData)
import           Wuss                       (runSecureClient)


{- HTTPS Functions -}


-- | Send an image search request to IQDB
-- | Currently this is dumb (doesn't check if a result exists or not)
-- imageSearch imgURL = secureRequest searchRequest >>= printResponse iqdbParser
--   where searchRequest = HTTP.parseRequest (iqdbXML ++ imgURL)
--         iqdbParser    = extractSubPattern regURL . extractSubPattern regFileURL

{- Request Builders / Helpers -}

-- | Send a REST request via HTTPS
secureRequest :: HTTP.Request -> IO (HTTP.Response L8.ByteString)
secureRequest req     = HTTP.httpLbs req =<< secureManager
  where secureManager = HTTP.newManager tlsManagerSettings

-- | Print a REST response
printResponse :: (r -> L8.ByteString) -> HTTP.Response r -> IO ()
printResponse respParser = L8.putStrLn . respParser . HTTP.responseBody

{- WSS Functions -}

connectToGateway :: ClientApp bot -> IO bot
connectToGateway =  runSecureClient HC.discordGateway 443 HC.gatewayPath

wsIncoming :: Connection -> IO L8.ByteString
wsIncoming = receiveData

wsSendTextMsg :: Connection -> L8.ByteString -> IO ()
wsSendTextMsg = sendTextData

wsClose :: Connection -> L8.ByteString -> IO ()
wsClose = sendClose
