module DB where

import Database.PostgreSQL.Simple.SqlQQ
-- import  qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Database.PostgreSQL.Simple.Time
import Class
import Types
import qualified Query 
-- import Data.Text
import Control.Monad.Except

import Control.Monad.Trans.Except
import Common
import Data.List

-- class FromDB a where 
--     get :: T [a]

-- instance FromDB User where
--     --get :: T [User]
--     get = do
--         Query.query_ [sql|SELECT * FROM users|]


getUsers :: T [User]
getUsers = do
    Query.query_ [sql|SELECT * FROM users|]

getAuthors :: T [Author]
getAuthors = do
    Query.query_ [sql|SELECT * FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id|]

getCategories :: T [Category]
getCategories = do 
    categories' <- Query.query_ [sql|SELECT * FROM categories|]
    toT $ DB.evalCategories categories' 

getPosts :: T [Post]
getPosts = do
    categories <- DB.getCategories
    posts' <- Query.query_ [sql|SELECT * FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id|]
    toT $ mapM (DB.evalPost categories) posts'

getTags :: T [Tag]
getTags = undefined

--такой ответ кажется избыточный, но по условиям УЧЕБНОГО проекта требуются именно вложенные сущности. 
evalCategories :: [Category'] -> Except E [Category]
evalCategories categories' = mapM (DB.evalCategory [] categories' . categoryId') categories' 

evalCategory :: [Int] -> [Category'] -> Int -> Except E Category
evalCategory  childs categories' categoryId = do
    if categoryId `elem` childs then do
        throwE . DBError $ template "Обнаружена циклическая категория {0}, которая является своим же родителем" [show categoryId]
    else do
        let mcategory' = find (\(Category' cid _ _) -> cid == categoryId) categories' --двух категорий с одинаковым первичным ключом быть не может. Но может быть Nothing
        case mcategory' of 
            Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Category' categoryId mparentId name) -> do
                case mparentId of
                    Nothing -> return $ Category categoryId Nothing name  
                    Just parentId -> do
                        parentCategory <- DB.evalCategory (categoryId:childs) categories' parentId 
                        return $ Category categoryId (Just parentCategory) name 

evalPost :: [Category] -> Post' -> Except E Post
evalPost categories (Post' postId (Content' a b c e categoryId f g))  = do
    let mcategory = find (\(Category cid _ _) -> cid == categoryId) categories
    case mcategory of
        Nothing -> throwE . DBError $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
        Just category -> return $ Post postId (Content a b c e category f g)
    

