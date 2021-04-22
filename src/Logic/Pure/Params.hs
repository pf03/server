module Logic.Pure.Params where

-- Our Modules
import           Common.Misc
import           Interface.Cache                 as Cache
import           Interface.Error                 as Error

-- Other Modules
import           Control.Monad.Except
import qualified Data.ByteString                 as B
import           Data.ByteString.Char8           as BC (ByteString)
import           Data.List
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Network.HTTP.Types.URI
import           Text.Read


-----------------------------Types---------------------------------------------
-- Eq - отсутствие суффикса
data Templ = Eq | In | All | Lt | Gt | Bt | Like  deriving (Show, Eq)

data ParamType = ParamTypePage
    | ParamTypeStr
    | ParamTypeInt
    | ParamTypeDate
    | ParamTypeSort  [BSName]
    | ParamTypeFileName [BSName] deriving Show

data ParamDesc = ParamDesc {
    templs    :: [Templ],
    paramType :: ParamType,
    must      :: Bool,
    nullable  :: Bool
}

--type BSName = BS    --created_at --Cache.hs
type BSKey = BS     --created_at__lt
type BSValue = BS   --"2021-01-01"
type BSTempl = BS   --"__lt"

-----------------------------GLOBAL CONSTANTS, used in other functions-----------------------------------------------------

templates :: [(Templ, BSTempl)]
templates = [(Eq ,""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Bt, "__bt"), (Like, "__like")]


--возможно перенести в роутер???
possibleParamDescs :: API -> M.Map BSName ParamDesc
possibleParamDescs (API queryType apiType) = M.fromList list where
    param a b c d = (a, ParamDesc b c d False)
    paramNull a b c d = (a, ParamDesc b c d True)
    list = case queryType of
        Auth -> [
                param "login" [Eq] ParamTypeStr True,
                param "pass" [Eq] ParamTypeStr True
                ]

        Upload -> case apiType of
            [Photo] -> [
                param "name" [Eq]  (ParamTypeFileName ["jpg", "png", "bmp"]) True
                ]

        Select -> case apiType of
            [Post] -> map ($ False) [
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
            _ -> map ($ False) [param "page" [Eq] ParamTypePage] --в тз про фильтры для других функций кроме posts ничего не сказано
        SelectById -> []
        Delete -> []

        Insert -> case apiType of
            [User] -> [
                --В каком формате нужно хранить и отдавать картинки (аватарки пользователей, фотографии к новостям)?
                --Просто URL до картинки. URL должен вести до твоего сервера, по запросу на этот URL сама картинка должна возвращаться
                param "last_name" [Eq] ParamTypeStr True,
                param "first_name" [Eq] ParamTypeStr True,
                param "avatar" [Eq] ParamTypeStr True,
                param "login" [Eq] ParamTypeStr True,
                param "pass" [Eq] ParamTypeStr True
                ]
            [Author] -> [
                param "user_id" [Eq] ParamTypeInt True,
                param "description" [Eq] ParamTypeStr True
                ]
            -- parent_id нельзя редактировать, чтобы не возникло циклических категорий. Можно только удалить категорию и создать заново
            --хотя удалить категорию тоже не всегда можно из-за связанных сущностей. Поэтому лучше сделать проверку на цикличность
            [Category] -> [
                param "parent_id" [Eq] ParamTypeInt False,
                param "category_name" [Eq] ParamTypeStr True
                ]
            [Tag] -> [param "name" [Eq] ParamTypeStr True]
            --тут еще добавить список тегов!!!
            [Draft] -> [
                param "name" [Eq] ParamTypeStr True,
                param "category_id" [Eq] ParamTypeInt True,
                param "text" [Eq] ParamTypeStr True,
                param "photo" [Eq] ParamTypeStr True,
                --возможно в данном случае более правильно использовать json, но в тз были именно такие конструкции
                --по крайней мере такие запросы единообразны с запросами select
                param "tag_id" [All] ParamTypeInt True,
                param "photos" [All] ParamTypeStr True --отличаем от главного фото
                ]
            [Draft, Id n, Post] -> [] --publish
            [Post] -> [] --[param "draft_id" [Eq] ParamTypeInt True] --draft_id уже в роутере
            [Post, Id n, Comment] -> [
                --param "user_id" [Eq] ParamTypeInt True,  --это из авторизации
                --param "creation_date" [Eq] ParamTypeDate True,   --дата берется на серваке
                param "text" [Eq] ParamTypeStr True
                ]
        --дополнительное требование - хотя бы один из параметров присутствует для update (checkParams)
        Update -> case apiType of
            User:xs -> [
                --В каком формате нужно хранить и отдавать картинки (аватарки пользователей, фотографии к новостям)?
                --Просто URL до картинки. URL должен вести до твоего сервера, по запросу на этот URL сама картинка должна возвращаться
                param "last_name" [Eq] ParamTypeStr False,
                param "first_name" [Eq] ParamTypeStr False,
                param "avatar" [Eq] ParamTypeStr False,  --потом подумать над загрузкой фото
                param "pass" [Eq] ParamTypeStr False
                ]
            Author:xs -> [
                param "user_id" [Eq] ParamTypeInt False,
                param "description" [Eq] ParamTypeStr False
                ]
            Category:xs -> [
                paramNull "parent_id" [Eq] ParamTypeInt False,
                param "category_name" [Eq] ParamTypeStr False
                ]
            Tag:xs -> [param "name" [Eq] ParamTypeStr False]
            --тут еще добавить список тегов!!!
            Draft:xs -> [
                --param "author_id" [Eq] ParamTypeInt False, --не редактируется
                param "name" [Eq] ParamTypeStr False,
                param "category_id" [Eq] ParamTypeInt False,
                param "text" [Eq] ParamTypeStr False,
                param "photo" [Eq] ParamTypeStr False,
                --param "news_id" [Eq] ParamTypeInt False --можно привязать черновик к другой новости?
                param "tag_id" [All] ParamTypeInt False,
                param "photos" [All] ParamTypeStr False --отличаем от главного фото
                ]
            [Post, Id _, Comment] -> [
                param "user_id" [Eq] ParamTypeInt False,
                param "text" [Eq] ParamTypeStr False
                ]
            [Post, Id _] -> [
                --param "author_id" [Eq] ParamTypeInt False,
                param "name" [Eq] ParamTypeStr True,  --для создания нового контента эти параметры обязательны. Фронтенд может взять их из оригинального поста
                param "category_id" [Eq] ParamTypeInt True,
                param "text" [Eq] ParamTypeStr True,
                param "photo" [Eq] ParamTypeStr True,
                param "tag_id" [All] ParamTypeInt True,
                param "photos" [All] ParamTypeStr True --отличаем от главного фото
                ]


possibleParams :: BSName -> ParamDesc -> [BSKey]
possibleParams bsname (ParamDesc templs _ _ _) = for templs $ \templ -> bsname <> jlookup templ templates

concatParams :: M.Map BSName ParamDesc -> [BSKey]
concatParams = concat . M.mapWithKey possibleParams

--------------------------------------------PARSE PARAMS WITH ERRORS HANDLING------------------------------------------------------------------

parseParams :: MError m => API -> Query -> m ParamsMap
parseParams  api qs = do
    let paramDescs = possibleParamDescs api
    let names = M.keys paramDescs
    checkParams api qs paramDescs
    forMapWithKeyM paramDescs $ parseParam qs

checkParams :: MError m => API -> Query -> M.Map BSName ParamDesc -> m ()
checkParams api qs paramDesc  = do
    --Update должен иметь хотя бы один параметр, иначе не имеет смысла
    case api of
        API Update xs -> do
            when (null qs) $ Error.throw . RequestError $ template "Необходимо указать хотя бы один параметр для редактирвания из следующего списка : {0}" [show $ M.keys paramDesc]
        _ -> return ()


    if M.null paramDesc && not (null qs) then do
        Error.throw . RequestError $ "Данная api-функция не имеет параметров"
        else do
            --проверка на лишние параметры
            let params = map fst qs
            let cp = concatParams paramDesc
            forM_ params $ \param -> do
                if param `elem` cp then return () else
                    Error.throw . RequestError $ template "Недопустимый параметр запроса: {0}" [show param]

parseParam :: MError m => Query -> BSName -> ParamDesc -> m Param
parseParam qs bsname paramDesc@(ParamDesc _ paramType _ nl)  = do
    mtuple <- findTemplate qs bsname paramDesc
    readParamAny paramType mtuple nl

--проверка всей строки запроса
--проверка на дублирующие, взаимоисключающие, обязательные параметры и параметры без значения (все проверки, кроме проверки на лишние параметры)
findTemplate :: MError m => [(BS, Maybe BS)] -> BSName -> ParamDesc -> m (Maybe (Templ, BSKey, BSValue))
findTemplate qs name paramDesc@(ParamDesc templs paramType must _) = do
    let pp = possibleParams name paramDesc
    let filtered = forMaybe qs $ \q -> checkParam q name paramDesc
    case filtered of
        [] -> if  must
            then Error.throw . RequestError $ template "Не указан обязательный параметр {0}" [show name]
            else return Nothing
        [(tmpl, param, Nothing)] -> Error.throw . RequestError $ template "Не указано значение параметра {0}" [show param]
        [(tmpl, param, Just value)] -> return . Just $ (tmpl, param, value)
        (r:rs) -> Error.throw . RequestError $ template "В списке параметров запроса должно быть не более одного значения из списка {0}, а их {1}: {2}"
            [show pp, show. length $ filtered, show filtered]

--проверка одного элемента строки запроса
checkParam :: (BS, Maybe BS) -> BSName -> ParamDesc -> Maybe (Templ, BSKey, Maybe BSValue)
checkParam (param, mvalue) name (ParamDesc templs _ _ _) = res where
    mtmpl = find (\tpl -> param == name <> jlookup tpl templates ) templs
    res = case mtmpl of
        Nothing   -> Nothing
        Just tmpl -> Just (tmpl, param, mvalue)

--------------------------------------PARAMS HANDLERS--------------------------------------------------------------------

readParamAny :: MError m => ParamType -> Maybe (Templ, BSKey, BSValue) -> Bool -> m Param
readParamAny paramType (Just (_, _, "null")) True = return ParamNull
readParamAny paramType (Just (_, key, "null")) False = Error.throw . RequestError $ template "Для параметра {0} не разрешено значение null" [show key]
readParamAny paramType mtuple _  = do
    case paramType of
        ParamTypePage          -> readParamPage mtuple
        ParamTypeInt           -> readParamInt mtuple
        ParamTypeStr           -> readParamStr mtuple
        ParamTypeDate          -> readParamDate mtuple
        ParamTypeSort list     -> readParamSort list mtuple
        ParamTypeFileName list -> readParamFileName list mtuple


--продумать, какие ограничения есть для каждой из трех функций
readParamPage :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamPage mtuple = case mtuple of
    Nothing -> return $ ParamEq (Int 1)
    _       -> readParam Int "Int" mtuple

readParamInt :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamInt mtuple = case mtuple of
    Just (Like, param, bs) -> Error.throw . RequestError $ template "Шаблон param__like допустим только для строковых параметров: {0}" [show param]
    _ -> readParam Int "Int" mtuple

readParamStr :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamStr = readParam Str "String"

readParamDate :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamDate mtuple = case mtuple of
    Just (Like, param, bs) -> Error.throw . RequestError $ template "Шаблон param__like допустим только для строковых параметров: {0}" [show param]
    _ -> readParam Date "Date" mtuple

readParamSort :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamSort list mtuple= do
    case mtuple of
        Just (Eq, param, bs)  -> if bs `elem` list
            then readParam Str "String" mtuple
            else Error.throw . RequestError $ template "Параметр {0} должен быть элементом списка {1}" [show param, show list]
        Just (_, param, bs) -> Error.throw . RequestError $ template "Для параметров сортировки допустим только шаблон eq: {0}" [show param]
        Nothing -> readParam Str "String" mtuple

readParamFileName :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamFileName list mtuple = case mtuple of
    Just (Eq, param, bs)  -> if any (helper bs) list
        then readParam Str "String" mtuple
        else Error.throw . RequestError $ template "Параметр {0} должен быть названием файла с одним из следующих разрешений: {1} в формате foo.png" [show param, show list]

    where
    --проверка на сответствие формату файла, например "foo.png"
    helper :: ByteString -> ByteString -> Bool
    helper bs format | B.length bs < 2 + B.length format = False
    helper bs format | takeEnd (1 + B.length format) bs  == "." <> format = True where takeEnd n xs = B.drop (B.length xs - n) xs
    helper bs format = False

readParam :: (MError m, Read a) => (a -> Val) -> String -> Maybe (Templ, BSKey, BSValue)  -> m Param
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

ereadMap :: (MError m, Read a) => String -> BS -> BS -> m a
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
    _ -> Error.throw . RequestError $ template "Неизвестный тип параметра {1}: {0}" [t, show param]
    where must = template "Параметр запроса {0} должен быть " [show param]

eread :: (MError m, Read a) => BC.ByteString -> String -> m a
eread bs error = Error.catchEither (readEither . unpackString $ bs) $ \e -> RequestError error

-- eread :: Read a => BC.ByteString -> Except String a
-- eread = except . readEither . BC.unpack

--это для корректной обработки кириллицы
unpackString :: BC.ByteString -> String
unpackString = T.unpack . T.decodeUtf8











