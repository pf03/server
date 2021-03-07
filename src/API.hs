module API where



--в будущем router объединить с api, и избежать циклических импортов

-- data API = API QueryType APIType
-- data QueryType = Select | Insert | Update | Delete deriving (Show, Read, Eq)  --это соответствует модулю
-- data APIType = Post | User | Author | Category | Tag | Draft | Publish deriving (Show, Read, Eq)  --это соответствует таблице в бд или id

data API = API QueryType [APIType]
data QueryType = Select | Insert | Update | Delete deriving (Show, Read, Eq)  --это соответствует модулю
data APIType = Post | User | Author | Category | Tag | Draft | Id Int deriving (Show, Read, Eq)  --это соответствует таблице в бд или id

-- publish это синоним для insertPost


--надо ли это?
-- instance Show API where
--     show (API queryType apiType)= res  where
--         res = case queryType of
--             Select -> plural $ show apiType
--             _ -> lower . show $ queryType <> "_" <> show apiType 
--         plural :: String -> String
--         plural apitype = case apitype of
--             "category" -> "categories"
--             _ -> apitype <> "s"
--         lower (x:xs) = toLower x <> xs

-- instance Read API where
--     readsPrec  = undefined --надо разобраться
