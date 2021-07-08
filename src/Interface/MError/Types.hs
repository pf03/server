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
    show (ParseError str)   = "Parse JSON error: "++str
    show (RequestError str) = "Request error: "++str
    show (ConfigError str)  = "Config error: "++str
    show (DBError str)      = "DB error: "++str
    show (IOError str)      = "IO error: "++str
    show (AuthError str)    = "Authorization error: "++str
    show (DevError str)     = "Developer error: "++str
    show (SomeError str)    = "Some error: "++str
instance E.Exception Error

instance MonadFail (Either Error) where
    fail str = Left $ DevError str