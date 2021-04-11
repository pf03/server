module Cache.Types.API where

data API = API QueryType [APIType] deriving (Show)
data QueryType = Select | SelectById | Insert | Update | Delete | Upload | Auth deriving (Show, Read, Eq)
data APIType = Post | User | Author | Category | Tag | Draft | Comment | Photo | Content | Id Int deriving (Show, Read, Eq)