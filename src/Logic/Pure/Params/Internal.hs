module Logic.Pure.Params.Internal where

import Common.Functions ((<<$>>))
import Common.Template (Template (template))
import Common.Types (BS, BSName)
import Control.Monad.Except (forM_, when)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Interface.Class (MError)
import Interface.MCache.Types
  ( API (..),
    APIType (Author, Category, Comment, Draft, Id, Image, Photo, Post, Tag, User),
    Param (ParamAll, ParamBt, ParamEq, ParamGt, ParamIn, ParamLike, ParamLt, ParamNo, ParamNull),
    ParamValue (..),
    QueryType (Auth, Delete, Insert, Load, Select, SelectById, Update, Upload),
  )
import qualified Interface.MError.Exports as Error
import Logic.Pure.Params.Types
  ( BSKey,
    BSTempl,
    BSValue,
    ParamDesc (ParamDesc),
    ParamType (..),
    Templ (..),
  )
import Network.HTTP.Types.URI (Query)
import Text.Read (readEither)

-----------------------------Params map----------------------------------------

templates :: [(Templ, BSTempl)]
templates = [(Eq, ""), (In, "__in"), (All, "__all"), (Lt, "__lt"), (Gt, "__gt"), (Bt, "__bt"), (Like, "__like")]

possibleParamDescs :: (MError m) => API -> m (M.Map BSName ParamDesc)
possibleParamDescs (API queryType apiType) = M.fromList <$> list
  where
    param a b c d = (a, ParamDesc b c d False)
    paramNull a b c d = (a, ParamDesc b c d True)
    list = case queryType of
      Auth ->
        return
          [ param "user_login" [Eq] ParamTypeStr True,
            param "pass" [Eq] ParamTypeStr True
          ]
      Upload -> case apiType of
        [Photo] ->
          return
            [ param "name" [Eq] (ParamTypeFileName ["jpg", "png", "bmp"]) True
            ]
        _ -> Error.throwServerError $ Error.patError "Params.possibleParamDesc" apiType
      Load -> case apiType of
        [Image _] -> return []
        _ -> Error.throwServerError $ Error.patError "Params.possibleParamDesc" apiType
      Select -> case apiType of
        [Post] ->
          return $
            map
              ($ False)
              [ param "created_at" [Eq, Lt, Gt, Bt] ParamTypeDate,
                param "author_name" [Eq, Like] ParamTypeStr,
                param "category_id" [Eq, In] ParamTypeInt,
                param "tag_id" [Eq, In, All] ParamTypeInt,
                param "content_name" [Eq, Like] ParamTypeStr,
                param "content_text" [Like] ParamTypeStr,
                param "contains" [Like] ParamTypeStr, -- The news API should support search by string, which can be found in either text content, author name, or category / tag name
                param "order_by" [Eq] $ ParamTypeSort ["created_at", "author_name", "category_id", "photos"],
                param "page" [Eq] ParamTypePage
              ]
        _ -> return $ map ($ False) [param "page" [Eq] ParamTypePage]
      SelectById -> return []
      Delete -> return []
      Insert -> case apiType of
        [User] ->
          return
            [ param "last_name" [Eq] ParamTypeStr True,
              param "first_name" [Eq] ParamTypeStr True,
              param "avatar" [Eq] ParamTypeStr True,
              param "user_login" [Eq] ParamTypeStr True,
              param "pass" [Eq] ParamTypeStr True
            ]
        [Author] ->
          return
            [ param "user_id" [Eq] ParamTypeInt True,
              param "description" [Eq] ParamTypeStr True
            ]
        [Category] ->
          return
            [ param "parent_id" [Eq] ParamTypeInt False,
              param "category_name" [Eq] ParamTypeStr True
            ]
        [Tag] -> return [param "tag_name" [Eq] ParamTypeStr True]
        [Draft] ->
          return
            [ param "content_name" [Eq] ParamTypeStr True,
              param "category_id" [Eq] ParamTypeInt True,
              param "content_text" [Eq] ParamTypeStr True,
              param "main_photo" [Eq] ParamTypeStr True,
              param "tag_id" [All] ParamTypeInt True,
              param "photos" [All] ParamTypeStr True
            ]
        [Draft, Id _, Post] -> return [] -- publish
        [Post] -> return []
        [Post, Id _, Comment] ->
          return
            [ param "comment_text" [Eq] ParamTypeStr True
            ]
        _ -> Error.throwServerError $ Error.patError "Params.possibleParamDesc" apiType
      -- An additional requirement - at least one of the parameters is present for Update (Params.checkParams)
      Update -> case apiType of
        User : _ ->
          return
            [ param "last_name" [Eq] ParamTypeStr False,
              param "first_name" [Eq] ParamTypeStr False,
              param "avatar" [Eq] ParamTypeStr False,
              param "pass" [Eq] ParamTypeStr False
            ]
        Author : _ ->
          return
            [ param "user_id" [Eq] ParamTypeInt False,
              param "description" [Eq] ParamTypeStr False
            ]
        Category : _ ->
          return
            [ paramNull "parent_id" [Eq] ParamTypeInt False,
              param "category_name" [Eq] ParamTypeStr False
            ]
        Tag : _ -> return [param "tag_name" [Eq] ParamTypeStr False]
        Draft : _ ->
          return
            [ param "content_name" [Eq] ParamTypeStr False,
              param "category_id" [Eq] ParamTypeInt False,
              param "content_text" [Eq] ParamTypeStr False,
              param "main_photo" [Eq] ParamTypeStr False,
              param "tag_id" [All] ParamTypeInt False,
              param "photos" [All] ParamTypeStr False
            ]
        [Post, Id _, Comment] ->
          return
            [ param "user_id" [Eq] ParamTypeInt False,
              param "comment_text" [Eq] ParamTypeStr False
            ]
        [Post, Id _] ->
          return
            [ param "content_name" [Eq] ParamTypeStr True, -- These parameters are required to create new content. The frontend can take them from the original post
              param "category_id" [Eq] ParamTypeInt True,
              param "content_text" [Eq] ParamTypeStr True,
              param "main_photo" [Eq] ParamTypeStr True,
              param "tag_id" [All] ParamTypeInt True,
              param "photos" [All] ParamTypeStr True
            ]
        _ -> Error.throwServerError $ Error.patError "Params.possibleParamDesc" apiType
      _ -> Error.throwServerError $ Error.patError "Params.possibleParamDesc" apiType

possibleParams :: BSName -> ParamDesc -> [BSKey]
possibleParams bsName (ParamDesc templs0 _ _ _) = catMaybes $ (flip fmap) templs0 $ \templ -> (Just bsName) <> lookup templ templates

concatParams :: M.Map BSName ParamDesc -> [BSKey]
concatParams = concat . M.mapWithKey possibleParams

-----------------------------Parse params--------------------------------------
checkParams :: MError m => API -> Query -> M.Map BSName ParamDesc -> m ()
checkParams api queries paramDesc = do
  -- Update must have at least one parameter, otherwise it doesn't make sense
  case api of
    API Update _ -> do
      when (null queries) $
        Error.throwRequest "You must specify at least one parameter for edit from the following list: {0}" [show $ M.keys paramDesc]
    _ -> return ()
  if M.null paramDesc && not (null queries)
    then Error.throwRequest "This api function has no parameters" []
    else do
      -- check for unsupported parameters
      let params = map fst queries
      let allParams = concatParams paramDesc
      forM_ params $ \param -> do
        if param `elem` allParams
          then return ()
          else Error.throwRequest "Unsupported request parameter: {0}" [show param]

-- For sql injection protection!
escapeQuotes :: Query -> Query
escapeQuotes = map (second (doubleQuotes <$>))
  where
    quote = c2w '\'' --39
    doubleQuotes :: BSValue -> BSValue
    doubleQuotes = B.concatMap (\word8 -> if word8 == quote then "''" else B.singleton word8)

parseParam :: MError m => Query -> BSName -> ParamDesc -> m Param
parseParam queries bsName paramDesc@(ParamDesc _ paramType0 _ nullable0) = do
  mTuple <- findTemplate queries bsName paramDesc
  readParamAny paramType0 mTuple nullable0

-- Checking the entire query string
-- Check for duplicate, mutually exclusive, required parameters and parameters without value
-- (all checks, except for checking for unsupported parameters)
findTemplate :: MError m => [(BS, Maybe BS)] -> BSName -> ParamDesc -> m (Maybe (Templ, BSKey, BSValue))
findTemplate queryString name paramDesc@(ParamDesc _ _ must0 _) = do
  let possibleParams0 = possibleParams name paramDesc
  let filtered = (flip mapMaybe) queryString $ \q -> checkParam q name paramDesc
  case filtered of
    [] ->
      if must0
        then Error.throwRequest "Required parameter {0} is not specified " [show name]
        else return Nothing
    [(_, param, Nothing)] -> Error.throwRequest "Parameter value {0} not specified" [show param]
    [(templ, param, Just value)] -> return . Just $ (templ, param, value)
    (_ : _) ->
      Error.throwRequest
        "The list of query parameters must contain no more than one value from the list {0}, but there are {1}: {2}"
        [show possibleParams0, show . length $ filtered, show filtered]

-- Checking one element of the query string
checkParam :: (BS, Maybe BS) -> BSName -> ParamDesc -> Maybe (Templ, BSKey, Maybe BSValue)
checkParam (param, mValue) name (ParamDesc ts _ _ _) = res
  where
    mTempl = find (\tpl -> Just param == Just name <> lookup tpl templates) ts
    res = case mTempl of
      Nothing -> Nothing
      Just templ -> Just (templ, param, mValue)

-----------------------------Params handlers-----------------------------------
readParamAny :: MError m => ParamType -> Maybe (Templ, BSKey, BSValue) -> Bool -> m Param
readParamAny _ (Just (_, _, "null")) True = return ParamNull
readParamAny _ (Just (_, key, "null")) False =
  Error.throwRequest "Value \"null\" is not allowed for parameter {0}" [show key]
readParamAny paramType mTuple _ = do
  case paramType of
    ParamTypePage -> readParamPage mTuple
    ParamTypeInt -> readParamInt mTuple
    ParamTypeStr -> readParamStr mTuple
    ParamTypeDate -> readParamDate mTuple
    ParamTypeSort list -> readParamSort list mTuple
    ParamTypeFileName list -> readParamFileName list mTuple

readParamPage :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamPage mTuple = case mTuple of
  Nothing -> return $ ParamEq (IntParam 1)
  _ -> readParam IntParam "Int" mTuple

readParamInt :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamInt mTuple = case mTuple of
  Just (Like, param, _) ->
    Error.throwRequest "The \"param__like\" template is only valid for string parameters: {0}" [show param]
  _ -> readParam IntParam "Int" mTuple

readParamStr :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamStr = readParam StringParam "String"

readParamDate :: MError m => Maybe (Templ, BSKey, BSValue) -> m Param
readParamDate mTuple = case mTuple of
  Just (Like, param, _) ->
    Error.throwRequest "The \"param__like\" template is only valid for string parameters: {0}" [show param]
  _ -> readParam DateParam "Date" mTuple

readParamSort :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamSort list mTuple = do
  case mTuple of
    Just (Eq, param, bs) ->
      if bs `elem` list
        then readParam StringParam "String" mTuple
        else Error.throwRequest "The parameter {0} must be an element of the list {1}" [show param, show list]
    Just (_, param, _) ->
      Error.throwRequest "Only template \"eq\" is valid for collation: {0}" [show param]
    Nothing -> readParam StringParam "String" mTuple

readParamFileName :: MError m => [BSName] -> Maybe (Templ, BSKey, BSValue) -> m Param
readParamFileName list mTuple = case mTuple of
  Just (Eq, param, bs) ->
    if any (checkFormat bs) list
      then readParam StringParam "String" mTuple
      else Error.throwRequest "Parameter {0} must be a filename with one of the following extensions: {1}, in the format \"foo.png\"" [show param, show list]
  _ -> Error.throwServerError $ Error.patError "Params.readParamFileName" mTuple
  where
    --check the file format, for example "foo.png"
    checkFormat :: BS -> BS -> Bool
    checkFormat bs format | B.length bs < 2 + B.length format = False
    checkFormat bs format | takeEnd (1 + B.length format) bs == "." <> format = True where takeEnd n xs = B.drop (B.length xs - n) xs
    checkFormat _ _ = False

readParam :: (MError m, Read a) => (a -> ParamValue) -> String -> Maybe (Templ, BSKey, BSValue) -> m Param
readParam constructor constructorStr mTuple = do
  let paramType = constructorStr
  let listType = template "[{0}]" [paramType]
  let tupleType = template "({0},{0})" [paramType]
  case mTuple of
    Nothing -> return ParamNo
    Just (templ, param, bs) -> case templ of
      Eq -> ParamEq . constructor <$> eReadMap paramType bs param
      In -> ParamIn <$> (constructor <<$>> eReadMap listType bs param)
      All -> ParamAll <$> (constructor <<$>> eReadMap listType bs param)
      Lt -> ParamLt . constructor <$> eReadMap paramType bs param
      Gt -> ParamGt . constructor <$> eReadMap paramType bs param
      Bt -> do
        (val1, val2) <- eReadMap tupleType bs param
        return $ ParamBt (constructor val1, constructor val2)
      Like -> ParamLike . constructor <$> eReadMap paramType bs param

-----------------------------Decoding------------------------------------------
eReadMap :: (MError m, Read a) => String -> BS -> BS -> m a
eReadMap paramType bs param = case paramType of
  "Int" -> eRead bs $ template "{0}an integer" [mustBe]
  "[Int]" -> eRead bs $ template "{0}a list of integers in the format [x,y,z]" [mustBe]
  "(Int,Int)" -> eRead bs $ template "{0}a pair of integers in the format (x,y)" [mustBe]
  -- option with string parameter without quotes ?text__like=glasgow
  "String" -> eRead ("\"" <> bs <> "\"") $ template "{0}a string" [mustBe]
  -- option with string parameter with quotes ?text__like="glasgow"
  --"String" -> eRead bs $ template "{0}a string" [mustBe]
  -- every string inside a list or tuple must be quoted ?tag__in=["python","haskell"]
  "[String]" -> eRead bs $ template "{0}a list of strings in the format [x,y,z]" [mustBe]
  "(String,String)" -> eRead bs $ template "{0}a pair of strings in the format (x,y)" [mustBe]
  "Date" -> eRead bs $ template "{0}a date in the format YYYY-MM-DD" [mustBe]
  "[Date]" -> eRead bs $ template "{0}a list of dates in the format [YYYY-MM-DD,YYYY-MM-DD,YYYY-MM-DD]" [mustBe]
  "(Date,Date)" -> eRead bs $ template "{0}a pair of dates in the format (YYYY-MM-DD,YYYY-MM-DD)" [mustBe]
  _ -> Error.throwRequest "Unknown parameter type {1}: {0}" [paramType, show param]
  where
    mustBe = template "The query parameter {0} must be " [show param]

eRead :: (MError m, Read a) => BS -> String -> m a
eRead bs err = Error.catchEither (readEither . unpackString $ bs) $ \_ -> Error.RequestError err

-- For correct processing of the Cyrillic alphabet
unpackString :: BS -> String
unpackString = T.unpack . T.decodeUtf8