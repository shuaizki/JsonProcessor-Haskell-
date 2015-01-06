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
        where processKV (key, value) = StringDoc "\"" <> StringDoc key <> StringDoc "\"" <> CharDoc ':' <> renderValue value

enclose :: Char->Char->(a->Doc)->[a]->Doc
enclose left right f value = foldr (<>) EmptyDoc (CharDoc left : (dconcat (CharDoc ',') . map f $ value) ++ [CharDoc right])

(<>) :: Doc -> Doc -> Doc
EmptyDoc <> x = x
x <> EmptyDoc = x
x <> y = Concat x y

dconcat :: Doc->[Doc]->[Doc]
dconcat _ [] = []
dconcat d [x] = [x]
dconcat d (x:xs) = x : d : dconcat d xs

prettify :: [Doc] -> Int -> Int -> String -> String
prettify [] count c ret = ret
prettify (d:ds) count c ret = case d of 
                               (Concat x y) -> prettify (x:y:ds) count c ret
                               other -> if (c + len other < count || c == 0) then prettify ds count (c + len other) (ret ++ flatten other) else prettify (d:ds) count 0 (ret ++ ['\n'])

len :: Doc -> Int
len (CharDoc c) = 1
len (StringDoc s) = length s

flatten :: Doc -> String
flatten (CharDoc c) = [c]
flatten (StringDoc s) = s

pretty :: Doc->String
pretty d = prettify [d] 20 0 ""

num = NumberValue 1.2
str = StringValue "this is a\nstring"
bool = BoolValue True
nul = NullValue
arr = ArrayValue [num, num, num, num]
str_arr = ArrayValue [str, str]
arr_arr = ArrayValue [str_arr, str_arr]
obj = Objects [("num", num), ("str", str), ("arr", ArrayValue [num, num])]
obj_arr = ArrayValue [ObjectValue obj]

main = do --putStrLn (renderValue num)
          putStrLn (pretty . renderValue $ num )
          putStrLn (pretty . renderValue $ str )
          putStrLn (pretty . renderValue $ bool )
          putStrLn (pretty . renderValue $ nul )
          --putStrLn (renderValue arr )
          putStrLn (pretty . renderValue $ arr )
          --putStrLn (renderValue  arr_arr )
          putStrLn (pretty . renderValue $ arr_arr )
          putStrLn (pretty . renderValue $ obj_arr )
