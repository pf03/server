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

--all possible paths with handlers
list :: [(String, PathInfo, T Response)]
list = undefined

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    
    let pathInfo = Wai.pathInfo req
    case pathInfo of 
        ["users"] -> getUsers
        ["authors"] -> getAuthors
        ["categories"] -> getCategories
        ["posts"] -> getPosts
        _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]
    --return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"


--не все вписываются в этот шаблон
-- getData :: [String] -> T Response
-- getData pathInfo = do
--     Log.setSettings Color.Blue  True "Response.getData"
--     users <- DB.getUsers 
--     -- let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
--     let eusers = encode users
--     Log.dataT Log.Info users
--     Response.json eusers



getUsers :: T Response
getUsers = do
    Log.setSettings Color.Blue  True "Response.getUsers"
    users <- DB.getUsers 
    -- let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
    let eusers = encode users
    Log.dataT Log.Info users
    return $ Wai.responseLBS status200 [(hContentType, "text/plain")] eusers

getAuthors :: T Response
getAuthors = do 
    Log.setSettings Color.Blue  True "Response.getAuthors"
    authors <- DB.getAuthors 
    let eauthors = encode authors
    Log.dataT Log.Info authors
    Response.json eauthors

getCategories :: T Response 
getCategories = do
    Log.setSettings Color.Blue  True "Response.getCategories"
    tuples <- DB.getCategories
    categories <- toT $ evalCategories tuples 
    let ecategories = encode categories
    Log.dataT Log.Info categories
    Response.json ecategories


-- evalCategory :: (Int, Maybe Int, String) -> [(Int, Maybe Int, String)] -> Category
-- evalCategory (categoryId, mparentId, name) tuples = case mparentId of
--     Nothing -> Category categoryId Nothing name
--     Just parentId -> evalParentCategory parentId tuples

-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

--такой ответ кажется избыточный, но по условиям УЧЕБНОГО проекта требуются именно вложенные сущности. 
evalCategories :: [(Int, Maybe Int, String)] -> Except E [Category]
evalCategories tuples = mapM (evalCategory [] tuples . fstOfThree) tuples where 
    fstOfThree (a, _, _) = a
   
--учесть случай циклической зависимости, вывести ошибку в таком случае
--возможно tojson сам вызовет ошибку в таком случае -  
evalCategory :: [Int] -> [(Int, Maybe Int, String)] -> Int -> Except E Category
evalCategory  childs tuples categoryId = do
    if categoryId `elem` childs then do
        throwE . DBError $ template "Обнаружена циклическая категория {0}, которая является своим же родителем" [show categoryId]
    else do
        let mtuple = find (\(cid, _, _) -> cid == categoryId) tuples --двух категорий с одинаковым первичным ключом быть не может. Но может быть Nothing
        case mtuple of 
            Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (categoryId, mparentId, name) -> do
                case mparentId of
                    Nothing -> return $ Category categoryId Nothing name  
                    Just parentId -> do
                        parentCategory <- evalCategory (categoryId:childs) tuples parentId 
                        return $ Category categoryId (Just parentCategory) name 

getPosts :: T Response
getPosts = do 
    Log.setSettings Color.Blue  True "Response.getUsers"
    tuples <- DB.getCategories
    categories <- toT $ evalCategories tuples 
    posts' <- DB.getPosts 
    posts <- toT $ mapM (evalPost  categories) posts'
    let eposts = encode posts
    Log.dataT Log.Info posts
    Response.json eposts

    --undefined
    -- let eposts = encode posts
    -- Log.dataT Log.Info posts
    -- Response.json eposts

evalPost :: [Category] -> Post' -> Except E Post
evalPost categories (Post' postId (Content' a b c e categoryId f g))  = do
    let mcategory = find (\(Category cid _ _) -> cid == categoryId) categories
    case mcategory of
        Nothing -> throwE . DBError $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
        Just category -> return $ Post postId (Content a b c e category f g)

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