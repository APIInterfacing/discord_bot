{-# LANGUAGE OverloadedStrings #-}

module RegexParse where

import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Text.Regex.TDFA            ((=~))

{- Regex Pattern Extractor -}
extractSubPattern :: L8.ByteString -> L8.ByteString -> L8.ByteString
extractSubPattern ptrn str = str =~ ptrn

-- | URL found within file_url="..."
regURL :: L8.ByteString
regURL = "\"[^\"]*\""

-- | file_url="..."
regFileURL :: L8.ByteString
regFileURL = "file_url=\"[^\"]*\""

-- | Similarity number found within <match sim=...
regSimNum :: L8.ByteString
regSimNum = "[0-9]+.[0-9]+"

-- | <match sim=...
regMatchSim :: L8.ByteString
regMatchSim = "<match sim=\"[^\"]*\""

-- | IQDB XML URL
iqdbXML :: String
iqdbXML = "https://iqdb.org/index.xml?url="

-- | Example image URL
eImg :: String
eImg = "i.stack.imgur.com/1Kpbw.png"
