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
import API
import qualified Data.Map as M

--Any означает отсутствие параметра
--Eq отсутствие суффикса

-----------------------------GLOBAL CONSTANTS, used in other functions-----------------------------------------------------
templates :: [(Templ, BSTempl)]
templates = [(Eq ,""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Bt, "__bt"), (Like, "__like")]

possibleParamDescs :: API.API -> ParamsMap ParamDesc
possibleParamDescs (API.API queryType apiType) = M.fromList list where
    param a b c d = (a, ParamDesc b c d)
    list = case queryType of
        API.Select -> case apiType of 
            API.Post -> map ($ False) [
                param "created_at" [Eq, Lt, Gt, Bt] ParamTypeDate,
                param "author_name" [Eq, Like] ParamTypeStr,
                param "category_id" [Eq, In] ParamTypeInt,
                param "tag_id" [Eq, In, All] ParamTypeInt,
                param "name" [Eq, Like] ParamTypeStr,
                param "text" [Like] ParamTypeStr,
                param "contains" [Like] ParamTypeStr,  --API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте, либо в имени автора, либо в названии категории/тега
                param "order_by" [Eq] $ ParamTypeSort ["created_at", "author_name", "category_id", "photos"], 
                param "page" [Eq] ParamTypePage
                ]
            _ -> map ($ False) [param "page" [Eq] ParamTypePage]
        API.Insert -> case apiType of 
            API.Tag -> map ($ True)[param "name" [Eq] ParamTypeStr]
            API.Author -> [
                param "user_id" [Eq] ParamTypeInt True,
                param "description" [Eq] ParamTypeStr True
                ]
            API.Category -> [
                param "parent_id" [Eq] ParamTypeInt False,
                param "category_name" [Eq] ParamTypeStr True
                ]

possibleParams :: BSName -> ParamDesc -> [BSKey]
possibleParams bsname (ParamDesc templs _ _) = for templs $ \templ -> bsname <> jlookup templ templates

concatParams :: ParamsMap ParamDesc -> [BSKey]
concatParams = concat . M.mapWithKey possibleParams

--------------------------------------------PARSE PARAMS WITH ERRORS HANDLING------------------------------------------------------------------

parseParams :: Query -> API.API -> Except E (ParamsMap Param)
parseParams qs api = do
    let paramDescs = possibleParamDescs api
    let names = M.keys paramDescs
    checkParams qs paramDescs
    forMapWithKeyM paramDescs $ parseParam qs

checkParams :: Query -> ParamsMap ParamDesc -> Except E ()
checkParams qs paramDesc  = do 
    --проверка на лишние параметры
    let params = map fst qs
    let cp = concatParams paramDesc
    forM_ params $ \param -> do 
    if param `elem` cp then return () else
        throwE . RequestError $ template "Недопустимый параметр запроса: {0}" [show param]
    
parseParam :: Query -> BSName -> ParamDesc -> Except E Param
parseParam qs bsname paramDesc@(ParamDesc _ paramType _)  = do
    mtuple <- findTemplate qs bsname paramDesc
    readParamAny paramType mtuple

--проверка всей строки запроса
--проверка на дублирующие, взаимоисключающие, обязательные параметры и параметры без значения (все проверки, кроме проверки на лишние параметры)
findTemplate :: [(BS, Maybe BS)] -> BSName -> ParamDesc -> Except E (Maybe (Templ, BSKey, BSValue))
findTemplate qs name paramDesc@(ParamDesc templs paramType must) = do
    let pp = possibleParams name paramDesc
    let filtered = forMaybe qs $ \q -> checkParam q name paramDesc
    case filtered of
        [] -> case  must of
            False ->  return Nothing
            True -> throwE . RequestError $ template "Не указан обязательный параметр {0}" [show name]
        [(tmpl, param, Nothing)] -> throwE . RequestError $ template "Не указано значение параметра {0}" [show param]
        [(tmpl, param, Just value)] -> return . Just $ (tmpl, param, value)
        (r:rs) -> throwE . RequestError $ template "В списке параметров запроса должно быть не более одного значения из списка {0}, а их {1}: {2}"
            [show pp, show. length $ filtered, show filtered]

--проверка одного элемента строки запроса
checkParam :: (BS, Maybe BS) -> BSName -> ParamDesc -> Maybe (Templ, BSKey, Maybe BSValue)
checkParam (param, mvalue) name (ParamDesc templs _ _) = res where
    mtmpl = find (\tpl -> param == name <> jlookup tpl templates ) templs
    res = case mtmpl of 
        Nothing -> Nothing 
        Just tmpl -> Just (tmpl, param, mvalue)

--------------------------------------PARAMS HANDLERS--------------------------------------------------------------------

readParamAny :: ParamType -> Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamAny paramType = case paramType of
    ParamTypePage -> readParamPage
    ParamTypeInt -> readParamInt
    ParamTypeStr -> readParamStr
    ParamTypeDate -> readParamDate
    ParamTypeSort list -> readParamSort list


--продумать, какие ограничения есть для каждой из трех функций
readParamPage :: Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamPage mtuple = case mtuple of
    Nothing -> return $ ParamEq (Int 1) 
    _ -> readParam Int "Int" mtuple

readParamInt :: Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamInt mtuple = case mtuple of
    Just (Like, param, bs) -> throwE . RequestError $ template "Шаблон param__like допустим только для строковых параметров: {0}" [show param] 
    _ -> readParam Int "Int" mtuple

readParamStr :: Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamStr = readParam Str "String"

readParamDate :: Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamDate mtuple = case mtuple of
    Just (Like, param, bs) -> throwE . RequestError $ template "Шаблон param__like допустим только для строковых параметров: {0}" [show param] 
    _ -> readParam Date "Date" mtuple

readParamSort :: [BSName] -> Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamSort list mtuple= do
    case mtuple of 
        Just (Like, param, bs)  -> if bs `elem` list 
            then readParam Str "String" mtuple
            else throwE . RequestError $ template "Параметр {0} должен быть элементом списка {1}" [show param, show list]
        _ -> readParam Str "String" mtuple



readParam :: Read a => (a -> Val) -> String -> Maybe (Templ, BSKey, BSValue)  -> Except E Param
readParam cons consStr mtuple = do
    let paramType = consStr
    let listType = template "[{0}]" [paramType]
    let tupleType = template "({0},{0})" [paramType]
    case mtuple of
        Nothing -> return ParamNo 
        Just (templ, param, bs) -> case templ of
            Eq -> ParamEq . cons <$> ereadMap paramType bs param
            In -> ParamIn <$> (cons <<$>> ereadMap listType bs param)
            All -> ParamAll <$> (cons <<$>> ereadMap listType bs param)
            Lt -> ParamLt . cons <$> ereadMap paramType bs param
            Gt -> ParamGt . cons <$> ereadMap paramType bs param
            Bt -> do
                (val1, val2) <- ereadMap tupleType bs param
                return $ ParamBt (cons val1, cons val2)
            Like -> ParamLike . cons <$> ereadMap paramType bs param

----------------------------------DECODING----------------------------------------------------------------------------------------------------

--по сути вся эта городуха нужна только для корректных сообщений об ошибках
ereadMap :: Read a => String -> BS -> BS -> Except E a
ereadMap t bs param = case t of
    "Int" -> eread bs $ template "{0}целым числом" [must]
    "[Int]" -> eread bs $ template "{0}массивом, состоящим из целых чисел в формате [x,y,z]" [must]
    "(Int,Int)" -> eread bs $ template "{0}парой целых чисел в формате (x,y)" [must]
    --вариант со строковым параметром без кавычек ?text__like=glasgow
    "String" -> eread ("\"" <> bs <> "\"") $ template "{0}строкой" [must]
    --вариант со строковым параметром в кавычках ?text__like="glasgow"
    --"String" -> eread bs $ template "{0}строкой" [must]
    --каждая строка внутри массива или кортежа должна быть в кавычках!!! ?tag__in=["python","haskell"]
    "[String]" -> eread bs $ template "{0}массивом, состоящим из строк в формате [x,y,z]" [must]
    "(String,String)" -> eread bs $ template "{0} парой строк в формате (x,y)" [must]
    "Date" -> eread bs $ template "{0}датой в формате YYYY-MM-DD" [must]
    "[Date]" -> eread bs $ template "{0}списком дат в формате [YYYY-MM-DD,YYYY-MM-DD,YYYY-MM-DD]" [must]
    "(Date,Date)" -> eread bs $ template "{0}списком дат в формате (YYYY-MM-DD,YYYY-MM-DD)" [must]
    _ -> throwE . RequestError $ template "Неизвестный тип параметра {1}: {0}" [t, show param]
    where must = template "Параметр запроса {0} должен быть " [show param]

eread :: Read a => BC.ByteString -> String -> Except E a
eread bs error = catchE (except . readEither . unpackString $ bs) $ \e -> do
    throwE . RequestError $ error

-- eread :: Read a => BC.ByteString -> Except String a
-- eread = except . readEither . BC.unpack

--это для корректной обработки кириллицы
unpackString :: BC.ByteString -> String
unpackString = T.unpack . T.decodeUtf8  










  
