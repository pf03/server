module Interface.MError.Functions where

import Common.Functions ( Template(template) )
import Common.Types ( LBS )
import qualified Control.Exception as E
import Control.Monad.Except ( MonadIO(liftIO) ) 
import Interface.MError.Class ( MError(..), MIOError ) 
import Interface.MError.Types ( Error(..) ) 
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import Network.HTTP.Types
    ( badRequest400, internalServerError500, unauthorized401, Status )
import Data.Aeson ( eitherDecode, FromJSON )
import Database.PostgreSQL.Simple ( SqlError(sqlErrorMsg) )

-----------------------------MError--------------------------------------------
liftE :: MError m => Either Error a -> m a
liftE ea = case ea of
  Left err -> throw err
  Right a -> return a

catchEither :: MError m => Either b a -> (b -> Error) -> m a
catchEither eba handler = case eba of
  Left b -> throw $ handler b
  Right a -> return a

toEither :: MError m => m a -> m (Either Error a)
toEither ma = do
  catch (Right <$> ma) $ \err -> return $ Left err

-----------------------------MIOError------------------------------------------
catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> Error) -> m a
catchEIO m h = do
  ea <- liftIO $ (Right <$> m) `E.catch` handler
  liftE ea
  where
    handler err = return . Left . h $ err

-- * The same as previous, but errors are handled automatically, without user handlers

liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
  ea <- liftIO $ (Right <$> m) `E.catch` iohandler `E.catch` sqlhandler `E.catch` otherhandler
  liftE ea
  where
    iohandler :: E.IOException -> IO (Either Error a)
    iohandler err = return . Left . IOError . show $ err
    sqlhandler :: SqlError -> IO (Either Error a)
    sqlhandler e = return . Left . DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e
    otherhandler :: E.SomeException -> IO (Either Error a)
    otherhandler err = return . Left . SomeError . show $ err

-----------------------------Functions-----------------------------------------
eDecode :: (MError m, FromJSON a) => LBS -> m a
eDecode bs = catchEither (eitherDecode bs) ParseError

getStatus :: Error -> Status
getStatus (ParseError _)   = internalServerError500
getStatus (RequestError _) = badRequest400
getStatus (ConfigError _)  = internalServerError500
getStatus (DBError _)      = badRequest400
getStatus (IOError _)      = internalServerError500
getStatus (AuthError _)    = unauthorized401
getStatus (DevError _)     = internalServerError500
getStatus (SomeError _)    = internalServerError500

-----------------------------Errors--------------------------------------------
errorDefault :: Error
errorDefault = SomeError "Some error occured"

authErrorDefault :: Error
authErrorDefault = AuthError "This function requires authorization"

dbErrorDefault :: Error
dbErrorDefault = DBError "A database error has occurred. Contact the administrator"

authErrorWrong :: Error
authErrorWrong = AuthError "Wrong authorization"

patError :: Show a => String -> a -> Error 
patError func pat = DevError $ template "Wrong pattern in function \"{0}\": {1}" [func, show pat]

throwDB :: (MError m) => String -> [String] -> m a
throwDB str args = throw $ DBError $ template str args

throwAuth :: (MError m) => String -> [String] -> m a
throwAuth str args = throw $ AuthError $ template str args

throwIO :: (MError m) => String -> [String] -> m a
throwIO str args = throw $ IOError $ template str args