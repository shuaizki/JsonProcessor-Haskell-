import Json
import Data.List (intercalate, insert)
import Data.Char (toLower)

data Doc = StringDoc String |
           CharDoc Char |
           EmptyDoc |
           Concat Doc Doc deriving (Show, Read, Eq)

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
renderValue (StringValue a) = StringDoc ('\"' : escape a ++ "\"")
renderValue (ArrayValue arr) = enclose '[' ']' renderValue arr
renderValue (ObjectValue EmptyObject) = StringDoc "{}"
renderValue (ObjectValue (Objects objs)) = enclose '{' '}' processKV objs
        where processKV (key, value) = StringDoc ('\"':key ++ "\"") <> CharDoc ':' <> renderValue value

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

--try to add nesting support for exercise


prettify :: Doc -> Int -> String
prettify doc count = helper [doc] 0
                  where helper [] _ = ""
                        helper (d:ds) c = case d of 
                                           (Concat x y) -> helper (x:y:ds) c
                                           other -> if (c + len other < count || c == 0) 
                                                      then flatten other ++ (helper ds (c + len other)) 
                                                      else "\n" ++ (helper (d:ds) 0) 

nestedPretty :: Doc -> String
nestedPretty d = nestedPrettify d 66

nestedPrettify :: Doc -> Int -> String
nestedPrettify doc count = helper [doc] 0 []
                                where helper [] _ _ = "" 
                                      helper (d:ds) c base = case d of 
                                                               (Concat x y) -> helper (x:y:ds) c base
                                                               other  -> if (fits c other base)
                                                                         then (flatten other) ++ (helper ds (c + len other) (baseNext c other base))
                                                                         else "\n" ++ (implement base) ++ (helper (d:ds) (currentBase base) base)
                                      isLeft d = case d of
                                                   CharDoc '{' -> True
                                                   CharDoc '[' -> True
                                                   _ -> False
                                      isRight d = case d of  
                                                  CharDoc '}' -> True
                                                  CharDoc ']' -> True
                                                  _ -> False
                                      currentBase base = case base of 
                                                          [] -> 0
                                                          (b:bs) -> b
                                      fits c other base = c + (len other) < count  || c == (currentBase base)
                                      baseNext c d base = if (isLeft d)
                                                          then c:base
                                                          else if (isRight d)
                                                            then (drop 1 base)
                                                            else base
                                      implement [] = []
                                      implement (n:ns) = replicate n ' '
len :: Doc -> Int
len (CharDoc c) = 1
len (StringDoc s) = length s

flatten :: Doc -> String
flatten (CharDoc c) = [c]
flatten (StringDoc s) = s


pretty :: Doc->String
pretty d = prettify d 66

num = NumberValue 1.2
str = StringValue "this is a\nstring"
bool = BoolValue True
nul = NullValue
arr = ArrayValue [num, num, num, num]
str_arr = ArrayValue [str, str]
arr_arr = ArrayValue [str_arr, str_arr]
arr_arr2 = ArrayValue [arr, arr, arr, arr]
obj = Objects [("num", num), ("str", str), ("arr", ArrayValue [num, num]), ("nested_array", arr_arr), ("nested_array", arr_arr2)]
obj_arr = Objects [("obj", ObjectValue obj), ("str", str)]

main = do --putStrLn (renderValue num)
          putStrLn "renderNum"
          putStrLn (pretty . renderValue $ num )
          putStrLn "renderStr"
          putStrLn (pretty . renderValue $ str )
          putStrLn "renderBool"
          putStrLn (pretty . renderValue $ bool )
          putStrLn "renderNull"
          putStrLn (pretty . renderValue $ nul )
          --putStrLn (renderValue arr )
          putStrLn "renderArray"
          putStrLn (pretty . renderValue $ arr )
          putStrLn (pretty . renderValue $ str_arr )
          putStrLn "renderNestedArray"
          putStrLn (pretty . renderValue $  arr_arr )
          putStrLn (pretty . renderValue $ arr_arr2 )
          putStrLn "renderObj"
          putStrLn (pretty . renderValue $ ( ObjectValue obj_arr) )
          putStrLn "test nestedPretty"
          putStrLn (nestedPretty . renderValue $ arr_arr)
          putStrLn (nestedPretty . renderValue $ ( ObjectValue obj_arr) )
