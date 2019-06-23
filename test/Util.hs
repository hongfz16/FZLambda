module Util where

-- import your AST
import           AST

data Example = Example { name :: String
                       , program :: Program
                       , expectedType :: Maybe Type
                       , expectedResult :: Result
                       }
                       deriving (Show)
