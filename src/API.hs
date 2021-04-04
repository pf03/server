module API where



--в будущем router объединить с api, и избежать циклических импортов
import Data.Char
data API = API QueryType [APIType] deriving (Show)
data QueryType = Select | SelectById | Insert | Update | Delete | Upload | Auth deriving (Show, Read, Eq)  --это соответствует модулю
data APIType = Post | User | Author | Category | Tag | Draft | Comment | Photo | Content | Id Int deriving (Show, Read, Eq)  --это соответствует таблице в бд или id

