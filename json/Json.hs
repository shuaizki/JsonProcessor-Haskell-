module Json
    (
      Object(..),
      Value(..)
    ) where

import Data.List (intercalate, insert)
import Data.Char (toLower)

data Value = StringValue String | NumberValue Double | BoolValue Bool | NullValue | ArrayValue [Value] | ObjectValue Object  deriving (Show, Read, Eq)

data Object = EmptyObject | Objects [(String, Value)] deriving (Show, Read, Eq)
