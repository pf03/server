{-# LANGUAGE DeriveGeneric #-}
module Auth where

import Database.PostgreSQL.Simple.FromRow --hiding (FromRow(..) ) 
import Database.PostgreSQL.Simple.Time
import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, Text(..))
import Types 
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common
import Query
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as B

import Control.Monad.Identity
import Select ( p, val )
import Data.Map as M ((!), fromList)
import qualified Data.Map as M (insert)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe
import API
import qualified State as S
import Data.Time
import Data.Time.Format.ISO8601
import Crypto.Hash.MD5 (hash)

import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Word
import Numeric
import Data.List.Split
import Text.Read

--простейший токен привязывается к пользователю, время жизни 1 сутки

--Data.Hashable
--можно использовать в хеше также пароль. Тогда в случае утечки секретного слова, все равно для генерации хеша нужен пароль.

--для авторизации логин и пароль нужно передавать в теле запроса!!!
--как и многие другие не-select запросы
--вобщем нужно реализовать оба варианта с возможностью простого переключения

newtype Token = Token {token :: String}  deriving (Show, Generic, Eq)
instance ToJSON Token

--получение токена
--во избежание коллизий нужно сделать ограничения на формат логина, пароля, email
login :: ParamsMap Param  -> T Token
login params  = do
    userIds <- query_ $ template [sql|SELECT id FROM users where login = 'pivan' and pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [p $ params ! "login", p $ params ! "pass"] :: T [Only Int]   
    --Log.debugT users
    case userIds of 
        [Only userId]  -> do 
            -- let curLogin = convert .  valStr . paramEq $ (params ! "login") 
            -- let curPass = convert .  valStr . paramEq $ (params ! "pass")
            
            -- if login == curLogin && pass == curPass then do
                let secret = "mySecretWord"
                date <- toT getCurrentTime 
                let day = iso8601Show . utctDay $ date
                let str = template "{0}_{1}_{2}" [secret, convert userId, convert day]
                return $ Token $ template "{0}_{1}_{2}" [show userId, day, toHex . hash $ str]
            -- else throwT $ AuthError "Неверный логин или пароль!"
        _ -> throwT $ AuthError "Неверный логин или пароль!"

--проверка токена, используется при каждом запросе
--здесь IO нужен только для даты  
auth :: Token -> T (Maybe Int)
auth (Token t)  = do
    let strs = splitOn "_" t
    case strs of
        (str: day : _) -> case readEither str of 
            Right userId -> do
                curDay <-  toT $ iso8601Show . utctDay <$> getCurrentTime
                if day == curDay then do
                    correctToken <- toT $ genToken userId
                    if correctToken == Token t 
                        then return (Just userId) 
                        else do
                            --return Nothing
                            throwT $ AuthError "Неверный токен!"
                else throwT $ AuthError "Неверная дата токена!"
            _ -> throwT $ AuthError "Неверный формат токена!"
        _ -> throwT $ AuthError "Неверный формат токена!"

genToken :: Int -> IO Token
genToken userId = do
    let secret = "mySecretWord"
    date <- getCurrentTime 
    let day = iso8601Show . utctDay $ date
    let str = template "{0}_{1}_{2}" [secret, convert userId, convert day]
    return $ Token $ template "{0}_{1}_{2}" [show userId, day, toHex . hash $ str]


h :: IO()
h = do
    let str = "hello"
    let ha = hash $ BC.pack $ str
    --putStrLn $ unpackString ha
    putStrLn $ show $ B.unpack ha
    putStrLn $ show $ BC.unpack ha
    print $ map B.w2c [1,2,3,4,5,6,7,8,9,10,11,12,13,255]
    let w8 = 23 :: Word8
    let ws8 = [0, 1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,255]
    putStrLn $ foldr helper "" ws8

    undefined 
    --showHex 
    where 

        helper :: Word8 -> String -> String
        helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc

toHex :: BC.ByteString -> String
toHex bs = foldr helper "" (B.unpack bs) where
        helper :: Word8 -> String -> String
        helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc

-- --это для корректной обработки кириллицы
-- unpackString :: BC.ByteString -> String
-- unpackString = T.unpack . T.decodeUtf8  