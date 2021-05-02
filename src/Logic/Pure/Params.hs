module Logic.Pure.Params where

-- Our modules
import           Common.Misc
import           Interface.Cache                 as Cache
import           Interface.Error                 as Error

-- Other modules
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
-- Eq - no suffix
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

-----------------------------Params map----------------------------------------

templates :: [(Templ, BSTempl)]
templates = [(Eq ,""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Bt, "__bt"), (Like, "__like")]

possibleParamDescs :: (MError m) => API -> m (M.Map BSName ParamDesc)
possibleParamDescs (API queryType apiType) = M.fromList <$> list where
    param a b c d = (a, ParamDesc b c d False)
    paramNull a b c d = (a, ParamDesc b c d True)
    list = case queryType of
        Auth -> return [
                param "login" [Eq] ParamTypeStr True,
                param "pass" [Eq] ParamTypeStr True
                ]

        Upload -> case apiType of
            [Photo] -> return [
                param "name" [Eq]  (ParamTypeFileName ["jpg", "png", "bmp"]) True
                ]
            _ -> Error.throw $ patError "Params.possibleParamDesc" apiType

        Select -> case apiType of
            [Post] -> return $ map ($ False) [
                param "created_at" [Eq, Lt, Gt, Bt] ParamTypeDate,
                param "author_name" [Eq, Like] ParamTypeStr,
                param "category_id" [Eq, In] ParamTypeInt,
                param "tag_id" [Eq, In, All] ParamTypeInt,
                param "name" [Eq, Like] ParamTypeStr,
                param "text" [Like] ParamTypeStr,
                param "contains" [Like] ParamTypeStr,  -- The news API should support search by string, which can be found in either text content, author name, or category / tag name
                param "order_by" [Eq] $ ParamTypeSort ["created_at", "author_name", "category_id", "photos"],
                param "page" [Eq] ParamTypePage
                ]
            _ -> return $ map ($ False) [param "page" [Eq] ParamTypePage]
        SelectById -> return []
        Delete -> return []

        Insert -> case apiType of
            [User] -> return [
                param "last_name" [Eq] ParamTypeStr True,
                param "first_name" [Eq] ParamTypeStr True,
                param "avatar" [Eq] ParamTypeStr True,
                param "login" [Eq] ParamTypeStr True,
                param "pass" [Eq] ParamTypeStr True
                ]
            [Author] -> return [
                param "user_id" [Eq] ParamTypeInt True,
                param "description" [Eq] ParamTypeStr True
                ]
            [Category] -> return [
                param "parent_id" [Eq] ParamTypeInt False,
                param "category_name" [Eq] ParamTypeStr True
                ]
            [Tag] -> return [param "name" [Eq] ParamTypeStr True]
            [Draft] -> return [
                param "name" [Eq] ParamTypeStr True,
                param "category_id" [Eq] ParamTypeInt True,
                param "text" [Eq] ParamTypeStr True,
                param "photo" [Eq] ParamTypeStr True,
                param "tag_id" [All] ParamTypeInt True,
                param "photos" [All] ParamTypeStr True
                ]
            [Draft, Id _, Post] -> return [] -- publish
            [Post] -> return [] 
            [Post, Id _, Comment] -> return [
                param "text" [Eq] ParamTypeStr True
                ]
            _ -> Error.throw $ patError "Params.possibleParamDesc" apiType
        -- An additional requirement - at least one of the parameters is present for Update (Params.checkParams)
        Update -> case apiType of
            User:_ -> return [
                param "last_name" [Eq] ParamTypeStr False,
                param "first_name" [Eq] ParamTypeStr False,
                param "avatar" [Eq] ParamTypeStr False,
                param "pass" [Eq] ParamTypeStr False
                ]
            Author:_ -> return [
                param "user_id" [Eq] ParamTypeInt False,
                param "description" [Eq] ParamTypeStr False
                ]
            Category:_ -> return [
                paramNull "parent_id" [Eq] ParamTypeInt False,
                param "category_name" [Eq] ParamTypeStr False
                ]
            Tag:_ -> return [param "name" [Eq] ParamTypeStr False]
            Draft:_ -> return [
                param "name" [Eq] ParamTypeStr False,
                param "category_id" [Eq] ParamTypeInt False,
                param "text" [Eq] ParamTypeStr False,
                param "photo" [Eq] ParamTypeStr False,
                param "tag_id" [All] ParamTypeInt False,
                param "photos" [All] ParamTypeStr False
                ]
            [Post, Id _, Comment] -> return [
                param "user_id" [Eq] ParamTypeInt False,
                param "text" [Eq] ParamTypeStr False
                ]
            [Post, Id _] -> return [
                param "name" [Eq] ParamTypeStr True,  -- These parameters are required to create new content. The frontend can take them from the original post
                param "category_id" [Eq] ParamTypeInt True,
                param "text" [Eq] ParamTypeStr True,
                param "photo" [Eq] ParamTypeStr True,
                param "tag_id" [All] ParamTypeInt True,
                param "photos" [All] ParamTypeStr True
                ]
            _ -> Error.throw $ patError "Params.possibleParamDesc" apiType


possibleParams :: BSName -> ParamDesc -> [BSKey]
possibleParams bsname (ParamDesc ts _ _ _) = for ts $ \templ -> bsname <> jlookup templ templates

concatParams :: M.Map BSName ParamDesc -> [BSKey]
concatParams = concat . M.mapWithKey possibleParams

-----------------------------Parse params--------------------------------------
parseParams :: MError m => API -> Query -> m ParamsMap
parseParams  api qs = do
    paramDescs <- possibleParamDescs api
    checkParams api qs paramDescs
    forMapWithKeyM paramDescs $ parseParam qs

checkParams :: MError m => API -> Query -> M.Map BSName ParamDesc -> m ()
checkParams api qs paramDesc  = do
    -- Update must have at least one parameter, otherwise it doesn't make sense
    case api of
        API Update _ -> do
            when (null qs) $ Error.throw . RequestError $ 
                template "You must specify at least one parameter for edit from the following list: {0}" [show $ M.keys paramDesc]
        _ -> return ()

    if M.null paramDesc && not (null qs) then do
        Error.throw . RequestError $ "This api function has no parameters"
        else do
            -- check for unsupported parameters
            let pars = map fst qs
            let cp = concatParams paramDesc
            forM_ pars $ \param -> do
                if param `elem` cp then return () else
                    Error.throw . RequestError $ template "Unsupported request parameter: {0}" [show param]

parseParam :: MError m => Query -> BSName -> ParamDesc -> m Param
parseParam qs bsname paramDesc@(ParamDesc _ pt _ nl)  = do
    mtuple <- findTemplate qs bsname paramDesc
    readParamAny pt mtuple nl

-- Checking the entire query string
-- Check for duplicate, mutually exclusive, required parameters and parameters without value 
-- (all checks, except for checking for unsupported parameters)
findTemplate :: MError m => [(BS, Maybe BS)] -> BSName -> ParamDesc -> m (Maybe (Templ, BSKey, BSValue))
findTemplate qs name paramDesc@(ParamDesc _ _ mst _) = do
    let pp = possibleParams name paramDesc
    let filtered = forMaybe qs $ \q -> checkParam q name paramDesc
    case filtered of
        [] -> if  mst
            then Error.throw . RequestError $ template "Required parameter {0} not specified " [show name]
            else return Nothing
        [(_, param, Nothing)]       -> Error.throw . RequestError $ template "Parameter value {0} not specified" [show param]
        [(tmpl, param, Just value)] -> return . Just $ (tmpl, param, value)
        (_:_) -> Error.throw . RequestError $ 
            template "The list of query parameters must contain no more than one value from the list {0}, but there are {1}: {2}"
            [show pp, show. length $ filtered, show filtered]

-- Checking one element of the query string
checkParam :: (BS, Maybe BS) -> BSName -> ParamDesc -> Maybe (Templ, BSKey, Maybe BSValue)
checkParam (param, mvalue) name (ParamDesc ts _ _ _) = res where
    mtmpl = find (\tpl -> param == name <> jlookup tpl templates ) ts
    res = case mtmpl of
        Nothing   -> Nothing
        Just tmpl -> Just (tmpl, param, mvalue)

-----------------------------Params handlers-----------------------------------
readParamAny :: MError m => ParamType -> Maybe (Templ, BSKey, BSValue) -> Bool -> m Param
readParamAny _ (Just (_, _, "null")) True = return ParamNull
readParamAny _ (Just (_, key, "null")) False = Error.throw . RequestError $ 
    template "Value \"null\" is not allowed for parameter {0}" [show key]
readParamAny pt mtuple _  = do
    case pt of
        ParamTypePage          -> readParamPage mtuple
        ParamTypeInt           -> readParamInt mtuple
        ParamTypeStr           -> readParamStr mtuple
        ParamTypeDate          -> readParamDate mtuple
        ParamTypeSort list     -> readParamSort list mtuple
        ParamTypeFileName list -> readParamFileName list mtuple



readParamPage :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamPage mtuple = case mtuple of
    Nothing -> return $ ParamEq (Int 1)
    _       -> readParam Int "Int" mtuple

readParamInt :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamInt mtuple = case mtuple of
    Just (Like, param, _) -> Error.throw . RequestError $ 
        template "The \"param__like\" template is only valid for string parameters: {0}" [show param]
    _ -> readParam Int "Int" mtuple

readParamStr :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamStr = readParam Str "String"

readParamDate :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamDate mtuple = case mtuple of
    Just (Like, param, _) -> Error.throw . RequestError $ 
        template "The \"param__like\" template is only valid for string parameters: {0}" [show param]
    _ -> readParam Date "Date" mtuple

readParamSort :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamSort list mtuple= do
    case mtuple of
        Just (Eq, param, bs)  -> if bs `elem` list
            then readParam Str "String" mtuple
            else Error.throw . RequestError $ 
                template "The parameter {0} must be an element of the list {1}" [show param, show list]
        Just (_, param, _) -> Error.throw . RequestError $ 
            template "Only template \"eq\" is valid for collation: {0}" [show param]
        Nothing -> readParam Str "String" mtuple

readParamFileName :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamFileName list mtuple = case mtuple of
    Just (Eq, param, bs) -> if any (helper bs) list
        then readParam Str "String" mtuple
        else Error.throw . RequestError $ 
            template "Parameter {0} must be a filename with one of the following extensions: {1}, in the format \"foo.png\"" [show param, show list]
    _ -> Error.throw $ patError "Params.readParamFileName" mtuple
    where
    --check the file format, for example "foo.png"
    helper :: ByteString -> ByteString -> Bool
    helper bs format | B.length bs < 2 + B.length format = False
    helper bs format | takeEnd (1 + B.length format) bs  == "." <> format = True where takeEnd n xs = B.drop (B.length xs - n) xs
    helper _ _ = False

readParam :: (MError m, Read a) => (a -> Val) -> String -> Maybe (Templ, BSKey, BSValue)  -> m Param
readParam cons consStr mtuple = do
    let pt = consStr
    let listType = template "[{0}]" [pt]
    let tupleType = template "({0},{0})" [pt]
    case mtuple of
        Nothing -> return ParamNo
        Just (templ, param, bs) -> case templ of
            Eq -> ParamEq . cons <$> ereadMap pt bs param
            In -> ParamIn <$> (cons <<$>> ereadMap listType bs param)
            All -> ParamAll <$> (cons <<$>> ereadMap listType bs param)
            Lt -> ParamLt . cons <$> ereadMap pt bs param
            Gt -> ParamGt . cons <$> ereadMap pt bs param
            Bt -> do
                (val1, val2) <- ereadMap tupleType bs param
                return $ ParamBt (cons val1, cons val2)
            Like -> ParamLike . cons <$> ereadMap pt bs param

-----------------------------Decoding------------------------------------------
ereadMap :: (MError m, Read a) => String -> BS -> BS -> m a
ereadMap t bs param = case t of
    "Int" -> eread bs $ template "{0}an integer" [mustBe]
    "[Int]" -> eread bs $ template "{0}a list of integers in the format [x,y,z]" [mustBe]
    "(Int,Int)" -> eread bs $ template "{0}a pair of integers in the format (x,y)" [mustBe]
    -- option with string parameter without quotes ?text__like=glasgow
    "String" -> eread ("\"" <> bs <> "\"") $ template "{0}a string" [mustBe]
    -- option with string parameter with quotes ?text__like="glasgow"
    --"String" -> eread bs $ template "{0}a string" [mustBe]
    -- every string inside a list or tuple must be quoted ?tag__in=["python","haskell"]
    "[String]" -> eread bs $ template "{0}a list of strings in the format [x,y,z]" [mustBe]
    "(String,String)" -> eread bs $ template "{0}a pair of strings in the format (x,y)" [mustBe]
    "Date" -> eread bs $ template "{0}a date in the format YYYY-MM-DD" [mustBe]
    "[Date]" -> eread bs $ template "{0}a list of dates in the format [YYYY-MM-DD,YYYY-MM-DD,YYYY-MM-DD]" [mustBe]
    "(Date,Date)" -> eread bs $ template "{0}a pair of dates in the format (YYYY-MM-DD,YYYY-MM-DD)" [mustBe]
    _ -> Error.throw . RequestError $ template "Unknown parameter type {1}: {0}" [t, show param]
    where mustBe = template "The query parameter {0} must be " [show param]

eread :: (MError m, Read a) => BC.ByteString -> String -> m a
eread bs err = Error.catchEither (readEither . unpackString $ bs) $ \_ -> RequestError err

-- For correct processing of the Cyrillic alphabet
unpackString :: BC.ByteString -> String
unpackString = T.unpack . T.decodeUtf8