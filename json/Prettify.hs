import Json
import Data.List (intercalate, insert)
import Data.Char (toLower)

data Doc = StringDoc String |
           CharDoc Char |
           EmptyDoc |
           TextDoc String |
           Line |
           Concat Doc Doc |
           Union Doc Doc deriving (Show, Read, Eq)

escape_chars = [('\n', "\\n"), ('\b', "\\b"), ('\f', "\\f"), ('\r', "\\r"), ('\t', "\\t"), ('\\',"\\\\")]

escape :: String -> Doc
escape [] = ""
escape (x:xs) = StringDoc ((processChar x (lookup x escape_chars)) ++ escape xs)
        where processChar a Nothing = a:[]
              processChar a (Just c) = c

strToLower :: String -> String
strToLower a = map toLower a

renderValue :: Value -> Doc
renderValue (NumberValue d) = StringDoc . show d
renderValue (BoolValue b) = StringDoc . strToLower . show b
renderValue (NullValue) = StringDoc "null"
renderValue (StringValue a) = escape a

(<>) :: Doc -> Doc -> Doc
EmptyDoc <> x = x
x <> EmptyDoc = x
x <> y = Concat x y

--enclose with left and right
enclose :: String->String->[Doc]->Doc
enclose left right docs = foldr
