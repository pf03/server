module Params where
import Control.Monad.Identity
import Network.HTTP.Types.URI
import Data.ByteString.Char8 as BC (ByteString, unpack)
import Control.Monad.Except
import Types
import Database.PostgreSQL.Simple.Time
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Control.Monad.Except
import Parse
import Common
import Control.Monad.Trans.Except
import Data.Maybe
import Text.Read
import Error
import Data.List

type BS = ByteString


jlookup :: Eq a => a -> [(a, b)] -> b
jlookup key list = fromJust $ lookup key list

for :: [a] -> (a -> b) -> [b]
for = flip map

--Any означает отсутствие параметра
--Eq отсутствие суффикса

-----------------------------GLOBAL CONSTANTS, used in other functions-----------------------------------------------------
templates :: [(Templ, ByteString)]
templates = [(Eq ,""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Like, "__like")]

possibleTemplsWithHandlers :: [(ByteString, [Templ], Maybe (Templ, BS, BS) -> Except E Param)]
possibleTemplsWithHandlers = [
    ("page", [Eq], page),
    ("tag", [Eq, In, All], tag),
    ("tag_id", [Eq, In, All], tagId),
    ("created_at", [Eq, Lt, Gt], createdAt),
    ("author_name", [Eq, Like], strParam),
    ("text", [Like], strParam),
    ("name", [Eq, Like], strParam)
    ];

possibleHandlers :: [Maybe (Templ, BS, BS) -> Except E Param]
possibleHandlers = map (\(a,b,c) -> c) possibleTemplsWithHandlers

possibleTempls :: [(ByteString, [Templ])]
possibleTempls = map (\(a,b,c) -> (a,b)) possibleTemplsWithHandlers

possibleNames :: [ByteString]
possibleNames = map (\(a,b,c) -> a) possibleTemplsWithHandlers

possibleParams :: [(ByteString, [ByteString])]
possibleParams = for possibleTempls $ \(name, tpls) ->
        (name, for tpls $ \tpl -> name <> jlookup tpl templates)

concatParams :: [ByteString]
concatParams = concatMap snd possibleParams 

--------------------------------------PARAMS HANDLERS--------------------------------------------------------------------

--конвертация из bs в типы haskell нужна хотя бы для проверки данных на корректность
--можно упростить и использовать eitherRead вместо eDecode!
page :: Maybe (Templ, BS, BS) -> Except E Param
page mtuple = do
    case mtuple of
        Nothing -> return . Param Eq $ Int 1
        Just (templ, param, bs)|templ==Eq -> do
            value <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show param]
            return . Param Eq $ Int value

tag :: Maybe (Templ, BS, BS) -> Except E Param
tag mtuple = do
    case mtuple of
        Nothing -> return ParamAny
        Just (templ, param, bs) -> case templ of 
            Eq -> do 
                value <- catchE (eread bs) $ \e -> do
                    throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show param]
                return $ Param templ $ Int value
            _ -> do 
                value <- catchE (eread bs) $ \e -> do
                    throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show param]
                return $ Param templ $ List value

createdAt :: Maybe (Templ, BS, BS) -> Except E Param
createdAt mtuple = do
    case mtuple of
        Nothing -> return ParamAny
        Just (templ, param, bs) -> do
            value <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен иметь формат YYYY-MM-DD" [show param]
            return $ Param templ $ Date value

strParam :: Maybe (Templ, BS, BS) -> Except E Param
strParam mtuple   = do
    case mtuple of
        Nothing -> return ParamAny
        Just (templ, param, bs) -> return $ Param templ $ Str $ unpackString bs

--в первом списке должно быть ровно одно значение из второго списка
lookupOne :: (Eq a, Show b, Show a) => [a] -> [(a, Maybe b)] -> Except E (Maybe (a, b))
lookupOne templates strs = do
    let filtered = filter (\(a, mb) -> a `elem` templates) strs
    case filtered of
        [] -> return Nothing
        [(a, Nothing)] -> throwE . RequestError $ template "Не указано значение параметра {0}" [show a]
        [(a, Just b)] -> return . Just $ (a, b)
        (r:rs) -> throwE . RequestError $ template "В списке параметров запроса должно быть не более одного значения из списка {0}, а их {1}: {2}"
            [show templates, show. length $ filtered, show filtered]

tagId = undefined


----------------------------------DECODING----------------------------------------------------------------------------------------------------
eread :: Read a => BC.ByteString -> Except String a
eread = except . readEither . BC.unpack

unpackString :: BC.ByteString -> String
unpackString = T.unpack . T.decodeUtf8

--------------------------------------------PARSE PARAMS WITH ERRORS HANDLING------------------------------------------------------------------


--просканировать все параметры на предмет лишних!!!
parseParams :: Query -> Except E [(ByteString, Param)]
parseParams qs = do
    checkParams (map fst qs)
    tuples <- groupParams qs
    params <- zipWithM ($) possibleHandlers tuples
    return $ zip possibleNames params

--проверка параметров на предмет лишних
checkParams :: [BS] -> Except E ()
checkParams params  = forM_ params $ \param -> do 
    if param `elem` concatParams then return () else
        throwE . RequestError $ template "Недопустимый параметр запроса: {0}" [show param]


--выбираем не более одного из возможных шаблонов для кажого возможного параметра
groupParams :: Query-> Except E [Maybe (Templ, BS, BS)]
groupParams qs = do
    let names = map fst possibleTempls
    forM names $ findTemplate qs

        --helper :: Query -> String ->  Except E (Maybe (ByteString,  Templ))
        --helper qs name = do
            -- params <- jlookup possibleParams name
            -- lookupOne params qs

--сложноватый код
--проверка всей строки запроса
findTemplate :: [(BS, Maybe BS)] -> BS -> Except E (Maybe (Templ, BS, BS))
findTemplate qs name = do
    let tmpls = jlookup name possibleTempls
    let pp = jlookup name possibleParams
    let filtered = forMaybe qs $ \q -> checkParam q name
    case filtered of
        [] -> return Nothing
        [(tmpl, param, Nothing)] -> throwE . RequestError $ template "Не указано значение параметра {0}" [show param]
        [(tmpl, param, Just value)] -> return . Just $ (tmpl, param, value)
        (r:rs) -> throwE . RequestError $ template "В списке параметров запроса должно быть не более одного значения из списка {0}, а их {1}: {2}"
            [show pp, show. length $ filtered, show filtered]

--проверка одного элемента строки запроса
checkParam :: (BS, Maybe BS) -> BS -> Maybe (Templ, BS, Maybe BS)
checkParam (param, mvalue) name = res where
    tmpls = jlookup name possibleTempls
    mtmpl = find (\tpl -> param == name <> jlookup tpl templates ) tmpls
    res = case mtmpl of 
        Nothing -> Nothing 
        Just tmpl -> Just (tmpl, param, mvalue)

--фильтруем элементы, для которых результат Nothing
forMaybe :: [a] -> (a -> Maybe b)  -> [b]
forMaybe = flip mapMaybe

--forFilter list f = fromJust . (filter isJust) . (map f) $ list

-- find :: (a -> Bool) -> [a] -> Maybe a






    -- let results = map (\(a,b) -> (a, fromJust b)). filter (isJust . snd) . map (\t -> (t, lookup t strs)) $  templates
    -- case results of
    --     [] -> return Nothing 
    --     [(a, Nothing)] -> throwE . RequestError $ template "Не указано значение параметра {0}" [show a]
    --     [(a, Just b)] -> return . Just $ (a, b)
    --     (r:rs) -> throwE . RequestError $ template "В списке параметров запроса {0} должно быть не более одного значения из списка {1}" [show strs, show templates]






-- page :: Query -> Except E Int
-- page qs = do
--     --mparamQuery <- lookupOne ["page"] qs
--     case mparamQuery of
--         Nothing -> return 1
--         Just ("page", bs) -> do
--             value <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "page"]
--             return value

    -- page <- eDecode . convertL . fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs
    -- return page

-- tag :: Query -> Except E Param
-- tag qs = do
--     --let params@[paramEq, paramIn, paramAll] = ["tag", "tags__in", "tags__all"] :: [BC.ByteString]
--     mparamQuery <- lookupOne ["tag", "tags__in", "tags__all"] qs
--     case mparamQuery of
--         Nothing -> return ParamsAny
--         Just ("tag", bs) -> do
--             paramInt <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "tag"]
--             return $ ParamsIn [paramInt]
--         Just ("tags__in", bs) -> do
--             paramList <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "tags__in"]
--             return $ ParamsIn paramList
--         Just ("tags__all", bs) -> do
--             paramList <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "tags__all"]
--             return $ Param All $ List paramList where



-- createdAt :: Query -> Except E Param
-- createdAt qs = do
--     mparamQuery <- lookupOne ["created_at", "created_at__lt", "created_at__gt"] qs
--     case mparamQuery of
--         Nothing -> return ParamAny
--         Just (field, bs) -> do
--             param <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен иметь формат YYYY-MM-DD" [show field]

--             case field of
--                 "created_at" -> return $ Param Eq $ Date param
--                 "created_at__lt" -> return $ Param Lt $ Date param
--                 "created_at__gt" -> return $ Param Gt $ Date param

-- --unpack нужен, чтобы потом сделать encodeUtf8 для кириллицы
-- --это выводит на консоль корректно, а запрос в бд некорректный
-- -- getTextParam :: HTTP.Query -> Maybe String
-- -- getTextParam =  fmap (BC.unpack) . fromMaybe Nothing . lookup "text"


-- strParam :: BC.ByteString -> Query -> Except E (Maybe String)
-- strParam paramName qs  = do
--     mparamQuery <- lookupOne [paramName] qs
--     case mparamQuery of
--         Nothing -> return Nothing
--         Just (p, bs) -> return . Just . unpackString $ bs

-- --вообще виснет при выводе на консоль, а запрос в бд корректный
-- --перекодировка в string нужна для корректной обработки кириллицы
-- -- text :: Query -> Except E (Maybe String)
-- -- --text = return . fmap ( T.unpack . T.decodeUtf8 ) . fromMaybe Nothing . lookup "text"
-- -- text qs = do
-- --     mparamQuery <- lookupOne ["text"] qs
-- --     case mparamQuery of 
-- --         Nothing -> return Nothing
-- --         Just ("text", bs) -> return . Just . unpackString $ bs

-- -- authorName :: Query -> Except E (Maybe String)
-- -- --text = return . fmap ( T.unpack . T.decodeUtf8 ) . fromMaybe Nothing . lookup "text"
-- -- authorName qs = do
-- --     mparamQuery <- lookupOne ["author_name"] qs
-- --     case mparamQuery of 
-- --         Nothing -> return Nothing
-- --         Just ("author_name", bs) -> return . Just . unpackString $ bs

-- authorName :: Query -> Except E (Maybe String)
-- authorName = strParam "author_name"
-- text :: Query -> Except E (Maybe String)
-- text = strParam "text"
-- name :: Query -> Except E (Maybe String)
-- name  = strParam "name"

-- --тут должен быть тип ошибки RequestError
-- --универсализировать, выкинуть ошибку при categories__all
-- category :: Query -> Except E Param
-- category qs = do
--     mparamQuery <- lookupOne ["category", "categories__in"] qs
--     case mparamQuery of
--         Nothing -> return ParamAny
--         Just ("category", bs) -> do
--             paramInt <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "category"]
--             return $ Param In List [paramInt]
--         Just ("categories__in", bs) -> do
--             paramList <- catchE (eread bs) $ \e -> do
--                 throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "categories__in"]
--             return $ ParamsIn paramList