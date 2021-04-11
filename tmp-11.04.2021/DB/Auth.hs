{-# LANGUAGE DeriveGeneric #-}
module Auth where
import           Data.Aeson
import           GHC.Generics
import qualified DB.Types.Row as Row
--import           Types
import           Common.Common
import           DB.Class
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Internal         as B
import           Database.PostgreSQL.Simple.SqlQQ

import           Control.Monad.Identity
import           Data.Map                         as M (fromList, (!))
import qualified Log.Class as Log
import           Select                           (p, val)

import           Crypto.Hash.MD5                  (hash)
import           Data.Time
import           Data.Time.Format.ISO8601

import qualified Cache.Class as Cache
import           Data.List.Split
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Word
import qualified Error.Class as Error
import qualified Network.Wai                      as Wai
import           Numeric
import           Text.Read

newtype Token = Token {token :: String}  deriving (Show, Generic, Eq)
instance ToJSON Token
instance FromJSON Token

login :: MT m => m Token
login = do
    params <- Cache.getParams
    users <- DB.query_ $ template [sql|SELECT id, is_admin FROM users where login = {0} and pass = md5 (CONCAT_WS(' ', {0}, {1}))|]
        [p $ params ! "login", p $ params ! "pass"]
    case users  :: [(Int, Bool)] of
        [(userId, isAdmin)]   -> do
            Log.debugT users
            when (userId == 1) $ Error.throw $ AuthError "Невозможно авторизоваться удаленным пользователем"
            let role = if isAdmin then "admin" else "user"
            date <- liftEIO getCurrentTime
            genToken date userId role
        _ -> Error.throw $ AuthError "Неверный логин или пароль!"

-- | Проверка токена происходит без базы данных
-- AuthNo только в случае отсутствия токена, в других случаях ошибка
auth :: (MIOError m, MCache m) => Wai.Request -> m ()
auth req  = do
    let h = Wai.requestHeaders req
    case lookup "Authorization" h of
        Nothing -> Cache.setAuth AuthNo
        Just a -> do
            date <- liftEIO getCurrentTime
            checkAuth_ date (Token $ BC.unpack a)

checkAuth_ :: (MError m, MCache m) => UTCTime -> Token -> m ()
checkAuth_ date token  = do
    (userId, role, day, hash) <- parseToken_ token
    let curDay = iso8601Show . utctDay $ date
    if day == curDay then do
        correctToken <- genToken date userId role
        if correctToken == token && role == "user"
            then Cache.setAuth $ AuthUser userId
            else if correctToken == token && role == "admin"
                then Cache.setAuth $ AuthAdmin userId
                else Error.throw $ AuthError "Неверный токен!"
    else Error.throw $ AuthError "Неверная дата токена!"

parseToken_ :: MError m => Token -> m (Int, String, String, String)
parseToken_ (Token t)  = do
    let strs = splitOn "_" t
    case strs of
        [userIdStr, role, day, hash] | role =="admin" || role == "user" || role == "author" -> do
            case readEither userIdStr of
                Right userId -> return (userId, role, day, hash)
                _            -> Error.throw $ AuthError "Неверный токен!" --"Неверный формат токена!"
        _ -> Error.throw $ AuthError "Неверный токен!" --"Неверный формат токена!"

genToken :: Monad m => UTCTime -> Int -> String -> m Token
genToken  date userId role = do
    let secret = "mySecretWord"
    let day = iso8601Show . utctDay $ date
    let str = template "{0}_{1}_{2}_{3}" [convert userId, convert role, convert day, secret]
    return $ Token $ template "{0}_{1}_{2}_{3}" [show userId, role, day, toHex . hash $ str]

toHex :: BC.ByteString -> String
toHex bs = foldr helper "" (B.unpack bs) where
    helper :: Word8 -> String -> String
    helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc
