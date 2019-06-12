module Main where

import BotBehavior (runBot)

main :: IO ()
main = runBot

{- Load Data -}

-- autoData :: IO (Maybe IDFields)
-- autoData =  decodeFileStrict "auth_data.json"

-- loadURL :: IO (Maybe API)
-- loadURL =  decodeFileStrict "dapi.json"

