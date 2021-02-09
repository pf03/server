import Network.Wai (responseLBS, Application)
import Network.Wai.Internal ( Request, Response, ResponseReceived )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import qualified Migrations
import Types
import Transformer


-- main = runT app
    -- let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
    -- conn <- connect connectDBInfo
    -- -- let port = 80
    -- -- putStrLn $ "Listening on port " ++ show port
    -- -- run port app
    -- query_ conn [sql| SELECT id, first_name FROM employee |] :: IO [(Int, String)]
-- main = do
--     --let port = 8080
--     putStrLn $ "Listening on port " ++ show port
--     run port app

main :: IO()
main = runAppT app 

app :: ApplicationT
app req f = do
    --undefined
    print req
    received <- f $ response req
    
    return received

--clear function
response :: Request -> Response
response req = responseLBS status200 [(hContentType, "text/plain")] "Hello world!"



