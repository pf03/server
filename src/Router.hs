module Router where

import Types
import Network.HTTP.Types
import qualified Data.Map as M
import Data.Text ( Text, unpack )
import Control.Monad.Except
import Text.Read 
import Control.Monad.Trans.Except
import API
import qualified Data.ByteString as B
import Common

--роутер проверяет только роли, а не соответстсвие id в токене и id в апи и params
router :: B.ByteString -> PathInfo -> Auth -> Except E API
---AUTH---
router _ ["login"] _ = return $ API Auth []
---UPLOAD---
router _ ["photos", "upload"] _ = return $ API Upload [Photo]
---INSERT---
router _ ["users", "create"] _ = return $ API Insert [User]
router _ ["authors", "create"] AuthAdmin = return $ API Insert [Author]
router _ ["categories", "create"] AuthAdmin = return $ API Insert [Category]
router _ ["tags", "create"] AuthAdmin = return $ API Insert [Tag]
router _ ["drafts", "create"] a  = withUser a $ API Insert [Draft]
router _ ["drafts", n, "publish"] AuthAdmin = withInt "post_id" n $ \pid -> API Insert [Draft, Id pid, Post] --метод drafts publish заменяет posts create и posts edit и подчеркивает, что новость нельзя опубликовать напрямую без черновика (премодерации)
router _ ["posts", n, "comments", "create"] a  = withUserE a $ withInt "post_id" n $ \pid -> API Insert [Post, Id pid, Comment]

---UPDATE---
router _ ["users", n, "edit"] a  = withUserE a $ withInt "user_id" n $ \pid -> API Update [User, Id pid]
router _ ["authors", n, "edit"] AuthAdmin = withInt "author_id" n $ \pid -> API Update [Author, Id pid]
router _ ["categories", n, "edit"] AuthAdmin = withInt "category_id" n $ \pid -> API Update [Category, Id pid]
router _ ["tags", n, "edit"] AuthAdmin = withInt "tag_id" n $ \pid -> API Update [Tag, Id pid]
router _ ["drafts", n, "edit"] a = withUserE a $ withInt "draft_id" n $ \pid -> API Update [Draft, Id pid]
router _ ["posts", n, "edit"] a = withUserE a $ withInt "post_id" n $ \pid -> API Update [Post, Id pid] 

---DELETE---
router _ ["users", n, "delete"] AuthAdmin = withInt "user_id" n $ \pid -> API Delete [User, Id pid]
router _ ["authors", n, "delete"] AuthAdmin = withInt "author_id" n $ \pid -> API Delete [Author, Id pid]
router _ ["categories", n, "delete"] AuthAdmin = withInt "category_id" n $ \pid -> API Delete [Category, Id pid]
router _ ["tags", n, "delete"] AuthAdmin = withInt "tag_id" n $ \pid -> API Delete [Tag, Id pid]
router _ ["drafts", n, "delete"] a  = withUserE a $ withInt "draft_id" n $ \pid -> API Delete [Draft, Id pid]
router _ ["posts", n, "delete"] AuthAdmin = withInt "post_id" n $ \pid -> API Delete [Post, Id pid] 
router _ ["comments", n, "delete"] a  = withUserE a $ withInt "comment_id" n $ \pid -> API Delete [Comment, Id pid] 

--у новости также есть еще фотографии
---SELECT MANY---
router _ ["users"] _ = return $ API Select [User]
router _ ["authors"] _ = return $ API Select [Author]
router _ ["categories"] _ = return $ API Select [Category]
router _ ["tags"] _ = return $ API Select [Tag]
router _ ["posts"] _ = return $ API Select [Post]
router _ ["drafts"] _ = return $ API Select [Draft]
router _ ["posts", n, "comments"] _ = withInt "post_id" n $ \pid -> API Select [Post, Id pid, Comment]

--SELECT BY ID---
router _ ["users", n ] _ = withInt "user_id" n $ \pid -> API SelectById [User, Id pid]
router _ ["authors", n] _ = withInt "author_id" n $ \pid -> API SelectById [Author, Id pid]
router _ ["categories", n] _ = withInt "category_id" n $ \pid -> API SelectById [Category, Id pid]
router _ ["tags", n] _ = withInt "tag_id" n $ \pid -> API SelectById [Tag, Id pid]
router _ ["posts", n] _ = withInt "post_id" n $ \pid -> API SelectById [Post, Id pid]
router _ ["drafts", n] _ = withInt "draft_id" n $ \pid -> API SelectById [Draft, Id pid]

--UNKNOWN---
router rawPathInfo _ _ = throwE . RequestError $ template "Неизвестный путь: {0}" [show rawPathInfo]

-- routerById name text apiType = withInt name text $ \pid -> API Select [User, Id pid]

-- helper ::  API -> BS -> Text -> Except E (API, ParamsMap Param)
-- helper api name text = do
--     int <- ereadInt (show name) text
--     return (API Update Tag, M.fromList [(name, ParamEq (Int int))])

ereadInt :: String -> Text -> Except E Int 
ereadInt name text = do
    let str = unpack text
    catchE (except . readEither $ str) $ \e -> do
        throwE . RequestError $ template "Значение {0} в роутере должно быть целым числом" [str] -- тут скорей нужно пропустить роутер дальше, типа функция неизвестна

withInt :: String -> Text -> (Int -> API) -> Except E API
withInt name text f = do
    pid <- ereadInt name text
    return $ f pid

--апи функция, которая требует авторизации
withUserE :: Auth -> Except E API -> Except E API
withUserE AuthNo _ = throwE . AuthError $ "Данная функция требует авторизации"
withUserE _ eapi = eapi

--то же с другой сигнатурой
withUser :: Auth -> API -> Except E API
withUser AuthNo _ = throwE . AuthError $ "Данная функция требует авторизации"
withUser _ api = return api

user :: Auth -> Bool 
user AuthNo = False 
user _ = True



