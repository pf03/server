{-# LANGUAGE DeriveGeneric #-}

module Logic.DB.Auth where

import Common.Convert (Convert (convert))
import Common.Functions (Template (template))
import Control.Monad.Identity (when)
import qualified Crypto.Hash.MD5 as MD5 (hash)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List.Split (splitOn)
import Data.Map as M ((!))
import Data.Time (UTCTime (utctDay), getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word (Word8)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Interface.Class (MCache, MError, MIOError, MTrans)
import Interface.MCache.Types ( Auth(AuthAdmin, AuthNo, AuthUser) )
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import Logic.DB.Select.Templates (paramToQuery)
import qualified Network.Wai as Wai
import Numeric (showHex)
import Text.Read (readEither)

-----------------------------Types---------------------------------------------
newtype Token = Token {token :: String} deriving (Show, Generic, Eq)

instance ToJSON Token

instance FromJSON Token

-----------------------------Public functions----------------------------------
login :: MTrans m => m Token
login = do
  pars <- Cache.getParams
  users <- DB.queryM
        [sql|
        SELECT id, is_admin FROM users
        WHERE login = {0} and pass = md5 (CONCAT_WS(' ', {0}, {1}))
    |] [paramToQuery $ pars ! "login", paramToQuery $ pars ! "pass"]
  case users :: [(Int, Bool)] of
    [(userId, isAdmin)] -> do
      Log.writeDebugM users
      when (userId == 1) $ Error.throwAuth "Unable to login as a deleted user" []
      let role = if isAdmin then "admin" else "user"
      date <- Error.liftEIO getCurrentTime
      genToken date userId role
    _ -> Error.throwAuth "Wrong login or password!" []

-- * Token verification take place without a database

-- AuthNo will be only in case of missing token, in other cases will be an error
auth :: (MIOError m, MCache m) => Wai.Request -> m ()
auth request = do
  let headers = Wai.requestHeaders request
  case lookup "Authorization" headers of
    Nothing -> Cache.setAuth AuthNo
    Just auth0 -> do
      date <- Error.liftEIO getCurrentTime
      checkAuth date (Token $ BC.unpack auth0)

-----------------------------Private functions---------------------------------
checkAuth :: (MError m, MCache m) => UTCTime -> Token -> m ()
checkAuth date token0 = do
  (userId, role, day, _) <- parseToken token0
  let curDay = iso8601Show . utctDay $ date
  if day == curDay
    then do
      correctToken <- genToken date userId role
      if correctToken == token0 && role == "user"
        then Cache.setAuth $ AuthUser userId
        else
          if correctToken == token0 && role == "admin"
            then Cache.setAuth $ AuthAdmin userId
            else Error.throwAuth "Wrong token!" []
    else Error.throwAuth "Wrong token date!" []

parseToken :: MError m => Token -> m (Int, String, String, String)
parseToken (Token token0) = do
  let strs = splitOn "_" token0
  case strs of
    [userIdStr, role, day, hash] | role == "admin" || role == "user" || role == "author" -> do
      case readEither userIdStr of
        Right userId -> return (userId, role, day, hash)
        _ -> Error.throwAuth "Wrong token!" [] --"Wrong token format!"
    _ -> Error.throwAuth "Wrong token!" [] --"Wrong token format!"

genToken :: Monad m => UTCTime -> Int -> String -> m Token
genToken date userId role = do
  let secret = "mySecretWord"
  let day = iso8601Show . utctDay $ date
  let str = template "{0}_{1}_{2}_{3}" [convert userId, convert role, convert day, secret]
  return $ Token $ template "{0}_{1}_{2}_{3}" [show userId, role, day, toHex . MD5.hash $ str]

toHex :: BC.ByteString -> String
toHex bs = foldr helper "" (B.unpack bs)
  where
    helper :: Word8 -> String -> String
    helper w8 acc = if w8 < 16 then "0" <> showHex w8 acc else showHex w8 acc
