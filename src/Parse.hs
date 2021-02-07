--{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 50
module Parse where
    
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Text (Text, pack)


import Control.Monad.Except
import Control.Monad.Trans.Except

import Data.Aeson

--наши модули
import Error  
import Data.Aeson.Types
import Types  
import Data.Maybe
import Class
import Common

-----------------From JSON------------------------------

getObject :: LC.ByteString -> Except E Object
getObject = except . typeError ParseError . eitherDecode 

getValue :: LC.ByteString -> Except E Value 
getValue = except . typeError ParseError . eitherDecode 

eDecode :: FromJSON a => LC.ByteString -> Except E a
eDecode = except . typeError ParseError . eitherDecode




-- _parseJSONo :: FromJSON a => Object -> Parser a
-- _parseJSONo = parseJSON . Object

-- --выход из монады Parser
-- _parseE :: (Object -> Parser a) -> Object -> Except E a
-- _parseE f = except . typeError ParseError . parseEither f

-- --обертка для работы с опциональным полем 
-- _mwithItem :: Key -> (Object -> Parser  a) -> Object -> Parser (Maybe a)
-- _mwithItem k f o = do
--     mo <- o .:? pack k
--     case mo of
--         Nothing -> return Nothing
--         Just o1 -> Just <$> f o1  

-- --обертка для работы с внутренними массивами
-- _withArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser [a]
-- _withArrayItem k f o = do
--     arr <- o .: pack k
--     mapM f arr

-- --обертка для работы с внутренними опциональными массивами
-- _mwithArrayItem :: Key -> (Object -> Parser a) -> Object -> Parser (Maybe [a])
-- _mwithArrayItem k f o = do
--     marr <- o .:? pack k -- :: Parser (Maybe [Object])
--     case marr of
--          Nothing -> return Nothing
--          Just arr -> Just <$> mapM f arr

-- --обертка для работы с внутренними массивами с опциональными элементами
-- _withArraymItem :: Key -> (Object -> Parser (Maybe a)) -> Object -> Parser [a]
-- _withArraymItem k f o = do
--     ma <- _withArrayItem k f o
--     return $ fmap fromJust . filter isJust $ ma

-- ----------------------------------------To JSON-------------------------------------

-- (<:>) :: Convert a => String -> a -> Query
-- (<:>) key value = [(convert key, jc value)]
-- infixr 7 <:>

-- (<:?>) :: Convert a => String -> Maybe a -> Query
-- (<:?>) key mvalue = case mvalue of
--   Nothing -> []
--   Just value -> [(convert key, jc value)]
-- infixr 7 <:?>
