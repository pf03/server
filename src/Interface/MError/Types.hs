{-# LANGUAGE FlexibleInstances #-}

module Interface.MError.Types where

import qualified Control.Exception as E

data Error = ParseError String
    | RequestError String
    | ConfigError String
    | DBError String
    | IOError String
    | AuthError String
    -- | An error that should never occur when the program is written correctly
    -- (for example, incorrect pattern matching)
    | DevError String 
    | SomeError String

instance Show Error where
    show (ParseError s)   = "Parse JSON error: "++s
    show (RequestError s) = "Request error: "++s
    show (ConfigError s)  = "Config error: "++s
    show (DBError s)      = "DB error: "++s
    show (IOError s)      = "IO error: "++s
    show (AuthError s)    = "Authorization error: "++s
    show (DevError s)     = "Developer error: "++s
    show (SomeError s)    = "Some error: "++s
instance E.Exception Error

instance MonadFail (Either Error) where
    fail s = Left $ DevError s