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

escape :: String -> String
escape [] = ""
escape (x:xs) = (processChar x (lookup x escape_chars)) ++ escape xs
        where processChar a Nothing = a:[]
              processChar a (Just c) = c

strToLower :: String -> String
strToLower a = map toLower a

renderValue :: Value -> Doc
renderValue (NumberValue d) = StringDoc $ show d
renderValue (BoolValue b) = StringDoc $ strToLower $ show b
renderValue (NullValue) = StringDoc "null"
renderValue (StringValue a) = StringDoc $ escape a
renderValue (ArrayValue arr) = enclose '[' ']' renderValue arr
renderValue (ObjectValue EmptyObject) = StringDoc "{}"
renderValue (ObjectValue (Objects objs)) = enclose '{' '}' processKV objs
        where processKV (key, value) = StringDoc key <> CharDoc ':' <> renderValue value

enclose :: Char->Char->(a->Doc)->[a]->Doc
enclose left right f value = CharDoc left <> mid <> CharDoc right
                          where mid = foldr (<>) EmptyDoc . dconcat (CharDoc ',') . map f $ value

(<>) :: Doc -> Doc -> Doc
EmptyDoc <> x = x
x <> EmptyDoc = x
x <> y = Concat x y

dconcat :: Doc->[Doc]->[Doc]
dconcat _ [] = []
dconcat d [x] = [x]
dconcat d (x:xs) = (d <> x) : dconcat d xs

--now pretty
