module Cache.Types.Params where

type ParamsMap = M.Map BSName

data Param = ParamEq {paramEq :: Val} 
    | ParamIn [Val] 
    | ParamAll [Val] 
    | ParamLt Val 
    | ParamGt Val 
    | ParamBt (Val, Val) 
    | ParamLike Val 
    | ParamNull 
    | ParamNo   deriving (Show, Eq)

data Val = Str { valStr :: String} | Int { valInt :: Int} | Date { valDate :: Date} deriving (Show, Eq)

type BSName = BS    --created_at
type BS = ByteString

data Templ = Eq | In | All | Lt | Gt | Bt | Like  deriving (Show, Eq)

data ParamType = ParamTypePage 
    | ParamTypeStr 
    | ParamTypeInt 
    | ParamTypeDate 
    | ParamTypeSort  [BSName] 
    | ParamTypeFileName [BSName] deriving Show

data ParamDesc = ParamDesc {
    templs :: [Templ],
    paramType :: ParamType,
    must :: Bool,
    nullable :: Bool
}

--type BSName = BS    --created_at --Cache.hs
type BSKey = BS     --created_at__lt
type BSValue = BS   --"2021-01-01"
type BSTempl = BS   --"__lt"
