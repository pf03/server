module Logic.Pure.Params.Types where

import Common.Types (BS, BSName)

-- Eq - no suffix
data Templ = Eq | In | All | Lt | Gt | Bt | Like deriving (Show, Eq)

data ParamType
  = ParamTypePage
  | ParamTypeStr
  | ParamTypeInt
  | ParamTypeDate
  | ParamTypeSort [BSName]
  | ParamTypeFileName [BSName]
  deriving (Show)

data ParamDesc = ParamDesc
  { templs :: [Templ],
    paramType :: ParamType,
    must :: Bool,
    nullable :: Bool
  }

--type BSName = BS    --created_at --Cache.hs
type BSKey = BS --created_at__lt

type BSValue = BS --"2021-01-01"

type BSTempl = BS --"__lt"