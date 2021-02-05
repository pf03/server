{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

main = do
    let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
    conn <- connect connectDBInfo
    -- let port = 80
    -- putStrLn $ "Listening on port " ++ show port
    -- run port app
    query_ conn [sql| SELECT id, first_name FROM employee |] :: IO [(Int, String)]

app :: Application
app req f =
    f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

-- main = do
--     let port = 8080
--     putStrLn $ "Listening on port " ++ show port
--     run port appmain