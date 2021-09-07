module Logic.IO.Emulate where

import Common.Convert (Convert (convert))
import Common.Template (Template (template))
import Control.Monad (forM)
import qualified Data.ByteString.Char8 as BC
import Interface.Class (MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.DB.Auth as Auth

-- | Generate sh script with tokens.
writeTokens :: MTrans m => m ()
writeTokens = do
  tuples@((adminToken, _) : _) <- forM users $ \(paramName, login, pass) -> do
    token <- getToken login pass
    let row = templ paramName token
    return (token, row)
  let rows = map snd tuples
  let fakeToken = calcFakeToken adminToken
  let fakeRow = templ "FAKEUSER" fakeToken
  Error.liftEIO $ BC.writeFile pathTokens $ convert $ concatMap (<> "\n") (fakeRow : rows)
  where
    users =
      [ ("ADMIN", "admin", "123456"),
        ("USER3", "pivan", "equalpass"),
        ("USER4", "ysergey", "equalpass"),
        ("USER5", "psergey", "psergeypass"),
        ("USER6", "vmayakovskiy", "vmayakovskiypass"),
        ("USER7", "dmoskvin", "dmoskvinpass")
      ]

    pathTokens :: FilePath
    pathTokens = "curl/tokens.sh"

    getToken :: MTrans m => String -> String -> m Auth.Token
    getToken login pass = do
      Cache.addStrParam_ "user_login" login
      Cache.addStrParam_ "pass" pass
      token <- Auth.login
      Log.writeDebugM token
      return token

    templ :: String -> Auth.Token -> String
    templ paramName (Auth.Token token) = template "{0}=\"Authorization: {1}\"" [paramName, token]

    calcFakeToken :: Auth.Token -> Auth.Token
    calcFakeToken (Auth.Token token) =
      if last token == '0'
        then Auth.Token $ init token <> "1"
        else Auth.Token $ init token <> "0"