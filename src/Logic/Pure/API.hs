module Logic.Pure.API where


-- Our Modules
import           Common.Misc
import           Interface.Cache as Cache
import           Interface.Error as Error

-- Other Modules
import qualified Data.ByteString as B
import qualified Data.Map        as M
import           Data.Text       (Text, unpack)
import           Text.Read



-- * Роутер проверяет только роли, иногда id, НО роутер не использует БД (у него даже нет доступа к соединению)
-- Использование БД в компетенции API-функций.
router :: MError m => B.ByteString -> PathInfo -> Auth -> m API
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
-- * Во всех остальных апи на втором месте должно быть число. Если его нет - не выдаем существование этого апи
-- * Hовость нельзя опубликовать напрямую без черновика (премодерации)
router p ["drafts", n, "publish"] (AuthAdmin _) = withInt p n $ \pid -> API Insert [Draft, Id pid, Post]
router p ["posts", n, "comments", "create"] a  = withUserE a $ withInt p n $ \pid -> API Insert [Post, Id pid, Comment]

---UPDATE---
router p ["users", n, "edit"] (AuthAdmin _)  = withInt p n $ \pid -> API Update [User, Id pid]
router p ["user", "edit"] a  = withAuth a $ \pid -> API Update [User, Id pid]
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

---SELECT MANY---
router _ ["users"] (AuthAdmin _) = return $ API Select [User]
router _ ["authors"] (AuthAdmin _) = return $ API Select [Author]
router _ ["categories"] _ = return $ API Select [Category]
router _ ["tags"] _ = return $ API Select [Tag]
router _ ["posts"] _ = return $ API Select [Post]
router _ ["drafts"] _ = return $ API Select [Draft]
router _ ["posts", n, "comments"] _ = withInt "post_id" n $ \pid -> API Select [Post, Id pid, Comment]

--SELECT BY ID---
router p ["users", n] (AuthAdmin _) = withInt p n $ \pid -> API SelectById [User, Id pid]
router p ["user"] a = withAuth a $ \pid -> API SelectById [User, Id pid]
router p ["authors", n] (AuthAdmin _) = withInt p n $ \pid -> API SelectById [Author, Id pid]
router p ["categories", n] _ = withInt p n $ \pid -> API SelectById [Category, Id pid]
router p ["tags", n] _ = withInt p n $ \pid -> API SelectById [Tag, Id pid]
router p ["posts", n] _ = withInt p n $ \pid -> API SelectById [Post, Id pid]
router p ["drafts", n] _ = withInt p n $ \pid -> API SelectById [Draft, Id pid]

--UNKNOWN---
router p _ _ = Error.throw $ unknownPathError p

ereadInt :: MError m => B.ByteString -> Text -> m Int
ereadInt p text = do
    let str = unpack text
    Error.catchEither (readEither str) $ \e -> unknownPathError p

withInt :: MError m => B.ByteString -> Text -> (Int -> API) -> m API
withInt p text f = do
    pid <- ereadInt p text
    return $ f pid

-- * Ошибка 401 - API-функция, которая требует авторизации
withUserE :: MError m => Auth -> m API -> m API
--неверное решение, не учитывает вариант, когда m API уже включает ошибку
-- withUserE AuthNo _ = Error.throw Error.authErrorDefault
-- withUserE _ eapi   = eapi
withUserE auth mapi = do
    api <- mapi
    case auth of
        AuthNo -> Error.throw Error.authErrorDefault
        _      -> return api



withUser :: MError m => Auth -> API -> m API
withUser AuthNo _ = Error.throw Error.authErrorDefault
withUser _ api    = return api

withAuth :: MError m => Auth -> (Int -> API) -> m API
withAuth AuthNo f          = Error.throw Error.authErrorDefault
withAuth (AuthAdmin uid) f = return $ f uid
withAuth (AuthUser uid) f  = return $ f uid

-- * Ошибка 400
unknownPathError :: B.ByteString -> Error.E
unknownPathError rawPathInfo = Error.RequestError $ template "Неизвестный путь: {0}" [show rawPathInfo]




