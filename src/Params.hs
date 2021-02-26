module Params where
import Control.Monad.Identity
import Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as BC
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

-- templates :: BC.ByteString -> [BC.ByteString]
-- templates field = map (field <>) ["", "__in", "__all", "__lt", "__gt"]


--можно упростить и использовать eitherRead вместо eDecode!
page :: Query -> Except E Int
page qs = do
    mparamQuery <- lookupOne ["page"] qs
    case mparamQuery of
        Nothing -> return 1
        Just ("page", bs) -> do
            value <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "page"]
            return value

    -- page <- eDecode . convertL . fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs
    -- return page

tag :: Query -> Except E (Params Int)
tag qs = do
    --let params@[paramEq, paramIn, paramAll] = ["tag", "tags__in", "tags__all"] :: [BC.ByteString]
    mparamQuery <- lookupOne ["tag", "tags__in", "tags__all"] qs
    case mparamQuery of 
        Nothing -> return ParamsAny
        Just ("tag", bs) -> do
            paramInt <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "tag"]
            return $ ParamsIn [paramInt]
        Just ("tags__in", bs) -> do
            paramList <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "tags__in"]
            return $ ParamsIn paramList
        Just ("tags__all", bs) -> do
            paramList <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "tags__all"]
            return $ ParamsAll paramList where 

eread :: Read a => BC.ByteString -> Except String a 
eread bs = except . readEither . BC.unpack $ bs


createdAt :: Query -> Except E (Params Date)
createdAt qs = do
    mparamQuery <- lookupOne ["created_at", "created_at__lt", "created_at__gt"] qs
    case mparamQuery of 
        Nothing -> return ParamsAny
        Just (field, bs) -> do
            param <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен иметь формат YYYY-MM-DD" [show field]

            case field of 
                "created_at" -> return $ ParamsIn [param]
                "created_at__lt" -> return $ ParamsLT param
                "created_at__gt" -> return $ ParamsGT param

--unpack нужен, чтобы потом сделать encodeUtf8 для кириллицы
--это выводит на консоль корректно, а запрос в бд некорректный
-- getTextParam :: HTTP.Query -> Maybe String
-- getTextParam =  fmap (BC.unpack) . fromMaybe Nothing . lookup "text"


--вообще виснет при выводе на консоль, а запрос в бд корректный
text :: Query -> Except E (Maybe String)
--text = return . fmap ( T.unpack . T.decodeUtf8 ) . fromMaybe Nothing . lookup "text"
text qs = do
    mparamQuery <- lookupOne ["text"] qs
    case mparamQuery of 
        Nothing -> return Nothing
        Just ("text", bs) -> return . Just . T.unpack . T.decodeUtf8 $ bs


--тут должен быть тип ошибки RequestError
--универсализировать, выкинуть ошибку при categories__all
category :: Query -> Except E (Params Int)
category qs = do
    mparamQuery <- lookupOne ["category", "categories__in"] qs
    case mparamQuery of 
        Nothing -> return ParamsAny
        Just ("category", bs) -> do
            paramInt <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть целым числом" [show "category"]
            return $ ParamsIn [paramInt]
        Just ("categories__in", bs) -> do
            paramList <- catchE (eread bs) $ \e -> do
                throwE . RequestError $ template "Параметр запроса {0} должен быть массивом, состоящим из целых чисел" [show "categories__in"]
            return $ ParamsIn paramList

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




    -- let results = map (\(a,b) -> (a, fromJust b)). filter (isJust . snd) . map (\t -> (t, lookup t strs)) $  templates
    -- case results of
    --     [] -> return Nothing 
    --     [(a, Nothing)] -> throwE . RequestError $ template "Не указано значение параметра {0}" [show a]
    --     [(a, Just b)] -> return . Just $ (a, b)
    --     (r:rs) -> throwE . RequestError $ template "В списке параметров запроса {0} должно быть не более одного значения из списка {1}" [show strs, show templates]
