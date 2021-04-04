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
import qualified Network.Wai as Wai

--простейший токен привязывается к пользователю, время жизни 1 сутки

--Data.Hashable
--можно использовать в хеше также пароль. Тогда в случае утечки секретного слова, все равно для генерации хеша нужен пароль.

--для авторизации логин и пароль нужно передавать в теле запроса!!!
--как и многие другие не-select запросы
--вобщем нужно реализовать оба варианта с возможностью простого переключения

newtype Token = Token {token :: String}  deriving (Show, Generic, Eq)
instance ToJSON Token
instance FromJSON Token

--получение токена
--во избежание коллизий нужно сделать ограничения на формат логина, пароля, email
login :: T Token
login = do
    params <- S.getParams
    users <- query_ $ template [sql|SELECT id, is_admin FROM users where login = {0} and pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [p $ params ! "login", p $ params ! "pass"] :: T [(Int, Bool)]   
    --Log.debugT users
    case users of 
        [(userId, isAdmin)]  -> do
            Log.debugT users
            when (userId == 1) $ throwT $ AuthError "Невозможно авторизоваться удаленным пользователем" 
            let role = if isAdmin then "admin" else "user"
            toT $ genToken userId role 
            -- let curLogin = convert .  valStr . paramEq $ (params ! "login") 
            -- let curPass = convert .  valStr . paramEq $ (params ! "pass")
            
            -- if login == curLogin && pass == curPass then do
                -- let secret = "mySecretWord"
                -- date <- toT getCurrentTime 
                -- let day = iso8601Show . utctDay $ date
                -- let str = template "{0}_{1}_{2}" [secret, convert userId, convert day]
                -- return $ Token $ template "{0}_{1}_{2}" [show userId, day, toHex . hash $ str]
            -- else throwT $ AuthError "Неверный логин или пароль!"
        _ -> throwT $ AuthError "Неверный логин или пароль!"

--проверка токена происходит без базы данных
auth :: Wai.Request -> T ()
auth req  = do
    let h = Wai.requestHeaders req
    case lookup "Authorization" h of
        Nothing -> S.setAuth AuthNo --nothing только в случае отсутствия токена, в других случаях ошибка
        Just a -> auth_ (Token $ BC.unpack a)

--проверка токена, используется при каждом запросе
--здесь IO нужен только для даты  
--сделать рефакторинг
auth_ :: Token -> T ()
auth_ (Token t)  = do
    let strs = splitOn "_" t
    case strs of
        --во избежание коллизий нужно сделать ограничения на формат логина, пароля, email
        [userIdStr, role, day, hash] | role =="admin" || role == "user" || role == "author" -> case readEither userIdStr of 
            Right userId -> do
                curDay <-  toT $ iso8601Show . utctDay <$> getCurrentTime
                if day == curDay then do
                    correctToken <- toT $ genToken userId role
                    if correctToken == Token t && role == "user"
                        then S.setAuth $ AuthUser userId
                        else if correctToken == Token t && role == "admin"
                            then S.setAuth $ AuthAdmin userId
                            else do
                                --return Nothing
                                throwT $ AuthError "Неверный токен!"
                else throwT $ AuthError "Неверная дата токена!"
            _ -> throwT $ AuthError "Неверный токен!" --"Неверный формат токена!"
        _ -> throwT $ AuthError "Неверный токен!" --"Неверный формат токена!"

genToken :: Int -> String -> IO Token
genToken userId role = do
    let secret = "mySecretWord"
    date <- getCurrentTime 
    let day = iso8601Show . utctDay $ date
    let str = template "{0}_{1}_{2}_{3}" [convert userId, convert role, convert day, secret]
    return $ Token $ template "{0}_{1}_{2}_{3}" [show userId, role, day, toHex . hash $ str]

toHex :: BC.ByteString -> String
toHex bs = foldr helper "" (B.unpack bs) where
        helper :: Word8 -> String -> String
        helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc
