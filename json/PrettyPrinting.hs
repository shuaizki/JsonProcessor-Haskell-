import Json
import Data.List (intercalate, insert)
import Data.Char (toLower)

renderObject :: Object -> String
renderObject EmptyObject = "{}"
renderObject (Objects a) = "{" ++ intercalate "," (renderKV a)  ++ "}"

renderKV :: [(String, Value)] -> [String]
renderKV xs = ["\"" ++ key ++ "\":" ++ (renderValue value) | (key, value) <- xs ]

strToLower :: String -> String
strToLower a = map toLower a

renderValue :: Value -> String
renderValue (NumberValue d) = show d
renderValue (BoolValue b) = strToLower (show b)
renderValue NullValue = "null"
renderValue (ArrayValue arr) = "[" ++ intercalate ", " (map renderValue arr) ++ "]"
renderValue (StringValue a) = escape a


--escape special charactors
escape_chars = [('\n', "\\n"), ('\b', "\\b"), ('\f', "\\f"), ('\r', "\\r"), ('\t', "\\t"), ('\\',"\\\\")]
escape :: String -> String
escape [] = ""
escape (x:xs) = (processChar x (lookup x escape_chars)) ++ escape xs
        where processChar a Nothing = a:[]
              processChar a (Just c) = c
--no need to escape unicode now

--then parsing
fromString :: String -> Object
fromString a = undefined

num = NumberValue 1.2
str = StringValue "this is a\nstring"
bool = BoolValue True
nul = NullValue
arr = ArrayValue [num, num]
str_arr = ArrayValue [str, str]
arr_arr = ArrayValue [num, num]
obj = Objects [("num", num), ("str", str), ("arr", ArrayValue [num, num])]
chinese_str = "我爱我家"

main = do putStrLn (renderValue num)
          putStrLn (renderValue str)
          putStrLn (renderValue bool)
          putStrLn (renderValue nul)
          putStrLn (renderValue arr)
          putStrLn (renderValue str_arr)
          putStrLn (renderValue arr_arr)
          putStrLn (renderObject obj)
          putStrLn (escape "\n\t\f我爱我家")
          putStrLn chinese_str
