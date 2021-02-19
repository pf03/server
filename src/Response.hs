--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Response where

import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified System.Console.ANSI as Color

import Data.Aeson

--import qualified Data.ByteString.Lazy as L

import Types
import Class
import qualified Log
import qualified DB 

--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Common
import qualified Encode
import Transformer
import Error 
import Data.Maybe
import Control.Monad.Except
import Data.List
import Control.Monad.Trans.Except
--import Database.PostgreSQL.Simple
import Network.HTTP.Types.URI

--all possible paths with handlers
-- getAPI :: ToJSON a => PathInfo -> API a
-- getAPI pathInfo = case pathInfo of 
--     ["users"] -> API "getUsers" pathInfo DB.getUsers
--     ["authors"] -> API "getAuthors" pathInfo DB.getAuthors
--     ["categories"] -> API "getCategories" pathInfo DB.getAuthors
--     ["posts"] -> API "getPosts" pathInfo DB.getPosts
--     ["tags"] -> API "getTags" pathInfo DB.getTags
--     _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]

-- apiList :: (ToJSON a, Show a) => [API a]
-- apiList =  [
--     API "getUsers" ["users"] DB.getUsers,
--     API "getAuthors" ["authors"] DB.getAuthors,
--     API "getCategories" ["categories"] DB.getCategories,
--     API "getPosts" ["posts"] DB.getPosts,
--     API "getTags" ["tags"] DB.getTags
--     ]

-- findApi :: PathInfo -> [API a] -> Maybe (API a)
-- findApi pathInfo =  find (\(API _ pi _) -> pi == pathInfo)

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    let pathInfo = Wai.pathInfo req
    let queryString = Wai.queryString req
    -- let mapi = Response.findApi pathInfo Response.apiList
    -- case mapi of
    --     Just api -> getData api
    --     Nothing -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]
    
    -- undefined 
    --let 
    --Response.getData $ Response.getAPI pathInfo
    -- let api = case pathInfo of 
    --     -- ["users"] -> API "getUsers" pathInfo DB.getUsers
    --     -- ["authors"] -> API "getAuthors" pathInfo DB.getAuthors
    --     -- ["categories"] -> API "getCategories" pathInfo DB.getAuthors
    --     -- ["posts"] -> API "getPosts" pathInfo DB.getPosts
    --     -- ["tags"] -> API "getTags" pathInfo DB.getTags
    --     _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]
    --let send = Response.sendData (show . head $ pathInfo) 
    case pathInfo of 
        ["users"] -> Response.sendData DB.getUsers pathInfo queryString 
        -- ["authors"] -> Response.sendData pathInfo queryString DB.getAuthors
        -- ["categories"] -> Response.sendData pathInfo queryString DB.getCategories
        ["posts"] -> Response.sendData DB.getPosts pathInfo queryString
        ["tags"] -> Response.sendData DB.getTags pathInfo queryString
        _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]
    --return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
    --Response.json edata
    --["users"] -> Response.sendData (show . head $ pathInfo) DB.getUsers


sendData :: ( ToJSON a, Show a) => (Query -> T a) -> PathInfo -> Query  -> T Response
sendData tgetData pathInfo queryString  = do
    Log.setSettings Color.Blue  True $ template "sendData: {0}" [show . head $ pathInfo] 
    _data <- tgetData queryString
    let edata = encode _data
    Log.dataT Log.Info _data
    Response.json edata

-- ff :: ToJSON a => T a -> T Response
-- ff = undefined

-- getUsers :: T Response
-- getUsers = do
--     Log.setSettings Color.Blue  True "Response.getUsers"
--     users <- DB.getUsers 
--     -- let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
--     let eusers = encode users
--     Log.dataT Log.Info users
--     return $ Wai.responseLBS status200 [(hContentType, "text/plain")] eusers

-- getAuthors :: T Response
-- getAuthors = do 
--     Log.setSettings Color.Blue  True "Response.getAuthors"
--     authors <- DB.getAuthors 
--     let eauthors = encode authors
--     Log.dataT Log.Info authors
--     Response.json eauthors

-- getCategories :: T Response 
-- getCategories = do
--     Log.setSettings Color.Blue  True "Response.getCategories"
--     categories <- DB.getCategories
--     -- categories <- toT $ evalCategories tuples 
--     let ecategories = encode categories
--     Log.dataT Log.Info categories
--     Response.json ecategories

-- getTags :: T Response
-- getTags = undefined

-- getPosts :: T Response
-- getPosts = do 
--     Log.setSettings Color.Blue  True "Response.getUsers"
--     categories <- DB.getCategories
--     -- categories <- toT $ evalCategories tuples 
--     posts <- DB.getPosts 
--     -- posts <- toT $ mapM (evalPost  categories) posts'
--     let eposts = encode posts
--     Log.dataT Log.Info posts
--     Response.json eposts

    --undefined
    -- let eposts = encode posts
    -- Log.dataT Log.Info posts
    -- Response.json eposts



json :: LC.ByteString -> T Response
json = return . Wai.responseLBS status200 [(hContentType, "text/plain")] 


-- getUsersBody :: T LC.ByteString
-- getUsersBody = do
--     users <- DB.getUsers 
--     let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
--     let eusers = Encode.users users
--     Log.dataT Log.Info users
--     return eusers

--это чистая безошибочная функция
errorHandler :: E -> Response
errorHandler e = do
    --let err = convertL ("Ошибочка вышла"::String)
    Wai.responseLBS status200 [(hContentType, "text/plain")] (convertL . show $ e)