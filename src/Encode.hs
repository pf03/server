--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Encode where

--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Database.PostgreSQL.Simple.Time

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import Common


keyboard :: [String] -> LC.ByteString 
--keyboard :: [String] -> Value
keyboard strs =   encode $  object [ "keyboard" .=  Array ( fromList [Array $ fromList (arr strs)] ) ]
  where
    arr:: [String] -> [Value]
    arr = map (\str -> object["text" .= str])  

-- keyboardL :: [LC.ByteString ] -> LC.ByteString 
-- --keyboard :: [String] -> Value
-- keyboardL strs =   encode $  object [ "keyboard" .=  Array ( fromList [Array $ fromList (arrL strs)] ) ]
--   where
--     arrL:: [LC.ByteString] -> [Value]
--     arrL = map (\str -> object["text" .= str])  

-- instance ToJSON LC.ByteString where 
--     toJSON s = String s


-- users :: [(Int, String, String)] -> LC.ByteString 
-- users us = encode $  object [ "users" .=  Array ( fromList [Array $ fromList (arr us)] ) ]
--     where 
--         arr:: [(Int, String, String)] -> [Value]
--         arr = map (\u -> object["text" .= ("fgdfgd"::String)])  



