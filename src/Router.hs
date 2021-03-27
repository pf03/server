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
import Error

--роутер проверяет только роли, а не соответстсвие id в токене и id в апи и params
router :: B.ByteString -> PathInfo -> Auth -> Except E API
---AUTH---
router _ ["login"] _ = return $ API Auth []
---UPLOAD---
router _ ["photos", "upload"] _ = return $ API Upload [Photo]
---INSERT---
router _ ["users", "create"] _ = return $ API Insert [User]
router _ ["authors", "create"] (AuthAdmin _) = return $ API Insert [Author]
router _ ["categories", "create"] (AuthAdmin _) = return $ API Insert [Category]
router _ ["tags", "create"] (AuthAdmin _) = return $ API Insert [Tag]
router _ ["drafts", "create"] a  = withUser a $ API Insert [Draft]
--во всех остальных апи на втором месте должно быть число. Если его нет - не выдаем существование этого апи
router p ["drafts", n, "publish"] (AuthAdmin _) = withInt p n $ \pid -> API Insert [Draft, Id pid, Post] --метод drafts publish заменяет posts create и posts edit и подчеркивает, что новость нельзя опубликовать напрямую без черновика (премодерации)
router p ["posts", n, "comments", "create"] a  = withUserE a $ withInt p n $ \pid -> API Insert [Post, Id pid, Comment]

---UPDATE---
router p ["users", n, "edit"] a  = withUserE a $ withInt p n $ \pid -> API Update [User, Id pid]
router p ["authors", n, "edit"] (AuthAdmin _) = withInt p n $ \pid -> API Update [Author, Id pid]
router p ["categories", n, "edit"] (AuthAdmin _) = withInt p n $ \pid -> API Update [Category, Id pid]
router p ["tags", n, "edit"] (AuthAdmin _) = withInt p n $ \pid -> API Update [Tag, Id pid]
router p ["drafts", n, "edit"] a = withUserE a $ withInt p n $ \pid -> API Update [Draft, Id pid]
router p ["posts", n, "edit"] a = withUserE a $ withInt p n $ \pid -> API Update [Post, Id pid] 

---DELETE---
router p ["users", n, "delete"] (AuthAdmin _) = withInt p n $ \pid -> API Delete [User, Id pid]
router p ["authors", n, "delete"] (AuthAdmin _) = withInt p n $ \pid -> API Delete [Author, Id pid]
router p ["categories", n, "delete"] (AuthAdmin _) = withInt p n $ \pid -> API Delete [Category, Id pid]
router p ["tags", n, "delete"] (AuthAdmin _) = withInt p n $ \pid -> API Delete [Tag, Id pid]
router p ["drafts", n, "delete"] a  = withUserE a $ withInt p n $ \pid -> API Delete [Draft, Id pid]
router p ["posts", n, "delete"] a = withUserE a $ withInt p n $ \pid -> API Delete [Post, Id pid] 
router p ["comments", n, "delete"] a  = withUserE a $ withInt p n $ \pid -> API Delete [Comment, Id pid] 

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
router p ["users", n ] _ = withInt p n $ \pid -> API SelectById [User, Id pid]
router p ["authors", n] _ = withInt p n $ \pid -> API SelectById [Author, Id pid]
router p ["categories", n] _ = withInt p n $ \pid -> API SelectById [Category, Id pid]
router p ["tags", n] _ = withInt p n $ \pid -> API SelectById [Tag, Id pid]
router p ["posts", n] _ = withInt p n $ \pid -> API SelectById [Post, Id pid]
router p ["drafts", n] _ = withInt p n $ \pid -> API SelectById [Draft, Id pid]

--UNKNOWN---
router p _ _ = unknownPath p

-- routerById name text apiType = withInt name text $ \pid -> API Select [User, Id pid]

-- helper ::  API -> BS -> Text -> Except E (API, ParamsMap Param)
-- helper api name text = do
--     int <- ereadInt (show name) text
--     return (API Update Tag, M.fromList [(name, ParamEq (Int int))])

ereadInt :: B.ByteString -> Text -> Except E Int 
ereadInt p text = do
    let str = unpack text
    catchE (except . readEither $ str) $ \e -> do
        unknownPath p
        --throwE . RequestError $ template "Значение {0} в роутере должно быть целым числом" [str] -- тут скорей нужно пропустить роутер дальше, типа функция неизвестна

withInt :: B.ByteString -> Text -> (Int -> API) -> Except E API
withInt p text f = do
    pid <- ereadInt p text 
    return $ f pid

--можно из типа E вычислить код ошибки
--ошибка 401
--апи функция, которая требует авторизации
withUserE :: Auth -> Except E API -> Except E API
withUserE AuthNo _ = throwE authErrorDefault
withUserE _ eapi = eapi

--то же с другой сигнатурой
withUser :: Auth -> API -> Except E API
withUser AuthNo _ = throwE authErrorDefault
withUser _ api = return api

user :: Auth -> Bool 
user AuthNo = False 
user _ = True

--ошибка 400
unknownPath :: B.ByteString -> Except E a
unknownPath rawPathInfo = throwE . RequestError $ template "Неизвестный путь: {0}" [show rawPathInfo]



