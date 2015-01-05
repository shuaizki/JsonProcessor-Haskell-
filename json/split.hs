--try to split 
--split into key_value String

import Data.List

stripPre :: String -> String -> String
stripPre s x = case stripPrefix s x 
                of Nothing -> x
                   Just a  -> a

stripSuf :: String -> String -> String
stripSuf spliter = reverse . (stripPre spliter) . reverse 
                where stripPre s x = case stripPrefix s x
                                        of Nothing -> x 
                                           Just a -> a

--split Object
split :: String -> [String]
split s = undefined 


a = "{\"num\":1.2,\"str\":this is a\\nstring,\"arr\":[1.2, 1.2]}"
b = "\"num\t\n\" :1.2"

splitKeyValue :: String -> (String, String)
splitKeyValue s = splitKV s ("", "") False

splitTest :: String -> [Bool]
splitTest xs = scanl (\a b-> b == '\"') True xs

splitKV :: String -> (String, String) -> Bool -> (String, String)
splitKV (x:xs) (k, v) flag
        | (not flag) && x == '\"' = splitKV xs (k, v) True
        | (not flag) && x == ' ' = splitKV xs (k, v) flag
        | (not flag) && x == ':' = (k, xs ++ v) 
        | flag && x == '\\' = splitKV xs1 (x:x1:k, v) flag
        | flag && x == '\"' = splitKV xs (k, v) False
        | flag = splitKV xs (k ++ [x], v) True
                where (x1:xs1) = xs

--not so successful seems need to carry a status when parsing
