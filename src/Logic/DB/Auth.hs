{-# LANGUAGE DeriveGeneric #-}
module Logic.DB.Auth
    -- ( Token(..)
    -- , login
    -- , auth
    -- ) 
    where

-- Our Modules
import           Common.Misc
import           Interface.Cache                  as Cache hiding (auth)
import           Interface.DB                     as DB
import           Interface.Error                  as Error
import           Interface.Log                    as Log
import           Logic.DB.Select                  (p)

-- Other Modules
import           Control.Monad.Identity           (when)
import qualified Crypto.Hash.MD5 as MD5               (hash)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import           Data.List.Split                  (splitOn)
import           Data.Map                         as M ((!))
import           Data.Time                        
import           Data.Time.Format.ISO8601         (iso8601Show)
import           Data.Word                        (Word8)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           GHC.Generics                     (Generic)
import qualified Network.Wai                      as Wai
import           Numeric                          (showHex)
import           Text.Read                        (readEither)
import           System.Environment  (getArgs)
-- import Data.Time.Calendar

-----------------------------Types---------------------------------------------
newtype Token = Token {token :: String}  deriving (Show, Generic, Eq)
instance ToJSON Token
instance FromJSON Token

-----------------------------Public functions----------------------------------
login :: MT m => m Token
login = do
    pars <- Cache.getParams
    users <- DB.query <$> template [sql|
        SELECT id, is_admin FROM users
        WHERE login = {0} and pass = md5 (CONCAT_WS(' ', {0}, {1}))
    |] <$$> [p $ pars ! "login", p $ pars ! "pass"]
    case users  :: [(Int, Bool)] of
        [(userId, isAdmin)]   -> do
            Log.debugM users
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
            checkAuth date (Token $ BC.unpack a)

debugDate :: UTCTime
debugDate = UTCTime (fromGregorian 2021 04 24) (secondsToDiffTime 0)
-----------------------------Private functions---------------------------------
checkAuth :: (MError m, MCache m) => UTCTime -> Token -> m ()
checkAuth date tok  = do
    (userId, role, day, _) <- parseToken tok
    let curDay = iso8601Show . utctDay $ date
    if day == curDay then do
        correctToken <- genToken date userId role
        if correctToken == tok && role == "user"
            then Cache.setAuth $ AuthUser userId
            else if correctToken == tok && role == "admin"
                then Cache.setAuth $ AuthAdmin userId
                else Error.throw $ AuthError "Неверный токен!"
    else Error.throw $ AuthError "Неверная дата токена!"

parseToken :: MError m => Token -> m (Int, String, String, String)
parseToken (Token t)  = do
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
    return $ Token $ template "{0}_{1}_{2}_{3}" [show userId, role, day, toHex . MD5.hash $ str]

toHex :: BC.ByteString -> String
toHex bs = foldr helper "" (B.unpack bs) where
    helper :: Word8 -> String -> String
    helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc
