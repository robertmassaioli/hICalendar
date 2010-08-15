module Folding where

import Data.List
import Data.Char

unfold :: String -> String
unfold [] = []
unfold ('\r':'\n':c:cs) = if isSpace c then unfold . dropWhile isSpace $ cs else '\r':'\n':unfold (c:cs)
unfold ('\n':c:cs) = if isSpace c then unfold . dropWhile isSpace $ cs else '\n':unfold (c:cs)
unfold (a:as) = a : unfold as

fold :: String -> String
fold s = (myjoin "\r\n" $ map (myjoin "\r\n ") $ map foldLine $ mylines s) ++ "\r\n"
 where
   foldLine :: String -> [String]
   foldLine s
     | length s > 75 = start : foldLine end
     | otherwise = [s]
     where
       (start, end) = splitAt 75 s

mylines    :: String -> [String]
mylines "" =  []
mylines s  =  let (l, s') = break (\c -> (c == '\n' || c == '\r')) s
              in  l : case s' of
                        []      -> []
                        ('\r':'\n':s'') -> mylines s''
                        ('\n':s'') -> mylines s''

myjoin :: String -> [String] -> String
myjoin a = concat . intersperse a
