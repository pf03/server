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

router :: B.ByteString -> PathInfo -> Except E API

---INSERT---
router _ ["users", "create"] = return $ API Insert [User]
router _ ["authors", "create"] = return $ API Insert [Author]
router _ ["categories", "create"] = return $ API Insert [Category]
router _ ["tags", "create"] = return $ API Insert [Tag]
router _ ["drafts", "create"] = return $ API Insert [Draft]
router _ ["drafts", n, "publish"] = withInt "post_id" n $ \pid -> API Insert [Draft, Id pid, Post] --метод drafts publish заменяет posts create и posts edit и подчеркивает, что новость нельзя опубликовать напрямую без черновика (премодерации)
router _ ["posts", n, "comments", "create"] = withInt "post_id" n $ \pid -> API Insert [Post, Id pid, Comment]

---UPDATE---
router _ ["users", n, "edit"] = withInt "user_id" n $ \pid -> API Update [User, Id pid]
router _ ["authors", n, "edit"] = withInt "author_id" n $ \pid -> API Update [Author, Id pid]
router _ ["categories", n, "edit"] = withInt "category_id" n $ \pid -> API Update [Category, Id pid]
router _ ["tags", n, "edit"] = withInt "tag_id" n $ \pid -> API Update [Tag, Id pid]
router _ ["drafts", n, "edit"] = withInt "draft_id" n $ \pid -> API Update [Draft, Id pid]
router _ ["posts", n, "edit"] = withInt "post_id" n $ \pid -> API Update [Post, Id pid] 

---DELETE---
router _ ["users", n, "delete"] = withInt "user_id" n $ \pid -> API Delete [User, Id pid]
router _ ["authors", n, "delete"] = withInt "author_id" n $ \pid -> API Delete [Author, Id pid]
router _ ["categories", n, "delete"] = withInt "category_id" n $ \pid -> API Delete [Category, Id pid]
router _ ["tags", n, "delete"] = withInt "tag_id" n $ \pid -> API Delete [Tag, Id pid]
router _ ["drafts", n, "delete"] = withInt "draft_id" n $ \pid -> API Delete [Draft, Id pid]
router _ ["posts", n, "delete"] = withInt "post_id" n $ \pid -> API Delete [Post, Id pid] 
router _ ["comments", n, "delete"] = withInt "comment_id" n $ \pid -> API Delete [Comment, Id pid] 

--у новости также есть еще фотографии
---SELECT MANY---
router _ ["users"] = return $ API Select [User]
router _ ["authors"] = return $ API Select [Author]
router _ ["categories"] = return $ API Select [Category]
router _ ["tags"] = return $ API Select [Tag]
router _ ["posts"] = return $ API Select [Post]
router _ ["drafts"] = return $ API Select [Draft]
router _ ["posts", n, "comments"] = withInt "post_id" n $ \pid -> API Select [Post, Id pid, Comment]

--SELECT BY ID---
router _ ["users", n ] = withInt "user_id" n $ \pid -> API SelectById [User, Id pid]
router _ ["authors", n] = withInt "author_id" n $ \pid -> API SelectById [Author, Id pid]
router _ ["categories", n] = withInt "category_id" n $ \pid -> API SelectById [Category, Id pid]
router _ ["tags", n] = withInt "tag_id" n $ \pid -> API SelectById [Tag, Id pid]
router _ ["posts", n] = withInt "post_id" n $ \pid -> API SelectById [Post, Id pid]
router _ ["drafts", n] = withInt "draft_id" n $ \pid -> API SelectById [Draft, Id pid]

--UNKNOWN---
router rawPathInfo _ = throwE . RequestError $ template "Неизвестный путь: {0}" [show rawPathInfo]

-- routerById name text apiType = withInt name text $ \pid -> API Select [User, Id pid]

-- helper ::  API -> BS -> Text -> Except E (API, ParamsMap Param)
-- helper api name text = do
--     int <- ereadInt (show name) text
--     return (API Update Tag, M.fromList [(name, ParamEq (Int int))])

ereadInt :: String -> Text -> Except E Int 
ereadInt name text = do
    let str = unpack text
    catchE (except . readEither $ str) $ \e -> do
        throwE . RequestError $ "Значение {0} в роутере должно быть целым числом"

withInt :: String -> Text -> (Int -> API) -> Except E API
withInt name text f = do
    pid <- ereadInt name text
    return $ f pid


--типы ошибок:
--в запросах к бд должны быть ошибки бд

