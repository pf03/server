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

--Any означает отсутствие параметра
--Eq отсутствие суффикса

-----------------------------GLOBAL CONSTANTS, used in other functions-----------------------------------------------------
templates :: [(Templ, BSTempl)]
templates = [(Eq ,""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Bt, "__bt"), (Like, "__like")]

possibleParamDescs :: ParamDesc
possibleParamDescs = [
    ("page", [Eq], ParamTypePage), 
    ("tag", [Eq, In, All], ParamTypeStr),
    ("tag_id", [Eq, In, All], ParamTypeInt),
    ("category_id", [Eq, In], ParamTypeInt),
    ("created_at", [Eq, Lt, Gt, Bt], ParamTypeDate),
    ("author_name", [Eq, Like], ParamTypeStr),
    ("text", [Like], ParamTypeStr),
    ("name", [Eq, Like], ParamTypeStr)
    ];

possibleParamNames :: [BSName]
possibleParamNames = map _1of3 possibleParamDescs

possibleParamTempls :: [(BSName, [Templ])]
possibleParamTempls = map _12of3 possibleParamDescs

possibleParams :: [(BSName, [BSKey])]
possibleParams = for possibleParamDescs $ \(name, tpls, _) ->
        (name, for tpls $ \tpl -> name <> jlookup tpl templates)

concatParams :: [BSKey]
concatParams = concatMap snd possibleParams 

--------------------------------------------PARSE PARAMS WITH ERRORS HANDLING------------------------------------------------------------------

parseParams :: Query -> Except E [(BSName, Param)]
parseParams qs = do
    checkParams (map fst qs)
    tuples <- groupParams qs
    let paramTypes = map _3of3 possibleParamDescs
    params <- zipWithM readParamAny paramTypes tuples
    return $ zip possibleParamNames params

--проверка параметров на предмет лишних
checkParams :: [BS] -> Except E ()
checkParams params  = forM_ params $ \param -> do 
    if param `elem` concatParams then return () else
        throwE . RequestError $ template "Недопустимый параметр запроса: {0}" [show param]


--выбираем не более одного из возможных шаблонов для кажого возможного параметра
groupParams :: Query-> Except E [Maybe (Templ, BSKey, BSValue)]
groupParams qs = do
    let names = map _1of3 possibleParamDescs
    forM names $ findTemplate qs

--сложноватый код
--проверка всей строки запроса
findTemplate :: [(BS, Maybe BS)] -> BSName -> Except E (Maybe (Templ, BSKey, BSValue))
findTemplate qs name = do
    let tmpls = jlookup name possibleParamTempls
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
    tmpls = jlookup name possibleParamTempls
    mtmpl = find (\tpl -> param == name <> jlookup tpl templates ) tmpls
    res = case mtmpl of 
        Nothing -> Nothing 
        Just tmpl -> Just (tmpl, param, mvalue)

----------------------------------DECODING----------------------------------------------------------------------------------------------------

--по сути вся эта городуха нужна только для корректных сообщений об ошибках
ereadMap :: Read a => String -> BS -> BS -> Except E a
ereadMap t bs param = case t of
    "Int" -> eread bs $ template "{0}целым числом" [must]
    "[Int]" -> eread bs $ template "{0}массивом, состоящим из целых чисел в формате [x,y,z]" [must]
    "(Int,Int)" -> eread bs $ template "{0}парой целых чисел в формате (x,y)" [must]
    "String" -> eread bs $ template "{0}строкой" [must]
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


--------------------------------------PARAMS HANDLERS--------------------------------------------------------------------

readParamAny :: ParamType -> Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamAny paramType = case paramType of
    ParamTypePage -> readParamPage
    ParamTypeInt -> readParamInt
    ParamTypeStr -> readParamStr
    ParamTypeDate -> readParamDate


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
readParamStr = readParam Str "Str"

readParamDate :: Maybe (Templ, BSKey, BSValue) -> Except E Param
readParamDate mtuple = case mtuple of
    Just (Like, param, bs) -> throwE . RequestError $ template "Шаблон param__like допустим только для строковых параметров: {0}" [show param] 
    _ -> readParam Date "Date" mtuple


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









  
