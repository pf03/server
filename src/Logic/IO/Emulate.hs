module App.Emulate where

-- Our modules
import           Common.Misc
import           Interface.Cache            as Cache hiding (api, auth, params)
import           Interface.DB               as DB (MT)
import           Interface.Error            as Error
import           Interface.Log              as Log
import           Logic.DB.Auth              as Auth (Token (Token))
import qualified Logic.DB.Auth              as Auth

-- Other modules
import           Control.Monad
import qualified Data.ByteString.Char8      as BC

-- | Generate sh script with tokens.
writeTokens :: MT m => m ()
writeTokens = do
    tuples@((adminToken, _):_) <- forM users $ \(pname, login, pass) -> do
        token <- getToken login pass
        let t = templ pname token
        return (token, t)
    let ts = map snd tuples
    let ftoken = fakeToken adminToken
    let ft = templ "FAKEUSER" ftoken
    liftEIO $ BC.writeFile pathTokens $ convert $ concatMap (<>"\n") (ft:ts) where
        users = [
            ("ADMIN", "admin", "123456"),
            ("USER3", "pivan", "equalpass"),
            ("USER4", "ysergey", "equalpass"),
            ("USER5", "psergey", "psergeypass"),
            ("USER6", "vmayakovskiy", "vmayakovskiypass"),
            ("USER7", "dmoskvin", "dmoskvinpass")
            ]

        pathTokens :: FilePath
        pathTokens = "curl/tokens.sh"

        getToken :: MT m => String -> String -> m Token
        getToken login pass = do
            Cache.addStrParam_ "login" login
            Cache.addStrParam_ "pass" pass
            token <- Auth.login
            Log.debugM token
            return token

        templ :: String -> Token -> String
        templ pname (Token t) = template "{0}=\"Authorization: {1}\"" [pname, t]

        fakeToken :: Token -> Token
        fakeToken (Token t) = if last t == '0' then Token $ init t <> "1" else Token $ init t <> "0"