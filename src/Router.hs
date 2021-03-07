module Router where

import Types
import Network.HTTP.Types
import qualified Data.Map as M
import Data.Text
import Control.Monad.Except
import Text.Read
import Control.Monad.Trans.Except
import API
import Params
import qualified Data.ByteString as B
import Common


routerWithParams :: B.ByteString -> PathInfo -> Query -> (API, ParamsMap Param)
routerWithParams rawPathInfo = undefined

-- router :: PathInfo -> (API, ParamsMap Param) 
-- router ["users"] = (API Select User, M.empty)

router :: B.ByteString -> PathInfo -> Except E (API, ParamsMap Param)
---SELECT MANY---
router _ ["users"] = e $ API Select User
router _ ["authors"] = e $ API Select Author
router _ ["categories"] = e $ API Select Category
router _ ["tags"] = e $ API Select Tag
router _ ["posts"] = e $ API Select Post

--SELECT BY ID---
router _ ["user", n ] = helper (API Select User) "user_id" n
router _ ["author", n] = helper (API Select User) "user_id" n
router _ ["category", n] = helper (API Select User) "user_id" n
router _ ["tag", n] = helper (API Select User) "user_id" n
router _ ["post", n] = helper (API Select User) "user_id" n

---INSERT---
router _ ["create", "author"] = e $ API Insert Author
router _ ["create", "category"] = e $ API Insert Category
router _ ["create", "Tag"] = e $ API Insert Tag
router _ ["create", "Draft"] = e $ API Insert Draft
router _ ["publish"] = e $ API Insert Publish

---UPDATE---
router _ ["edit", "tag", n] = helper (API Update Tag) "tag_id" n

---DELETE---
router _ ["delete", "tag", n] = helper (API Delete Tag) "tag_id" n

--UNKNOWN---
router rawPathInfo _ = throwE . RequestError $ template "Неизвестный путь: {0}" [show rawPathInfo]


--вспомогательные функции
e ::  API -> Except E (API, ParamsMap Param)
e api = return (api, M.empty)

helper ::  API -> BS -> Text -> Except E (API, ParamsMap Param)
helper api name text = do
    int <- ereadInt (show name) text
    return (API Update Tag, M.fromList [(name, ParamEq (Int int))])

ereadInt :: String -> Text -> Except E Int 
ereadInt name text = do
    let str = unpack text
    catchE (except . readEither $ str) $ \e -> do
        throwE . RequestError $ "Значение {0} в роутере должно быть целым числом"

--типы ошибок:
--в запросах к бд должны быть ошибки бд