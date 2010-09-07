ICalendar Folding
=================

There is a process called "Folding" in the ICalendar specification that states that no ICalendar should
have a line that is longer than 75 characters wide. To maintain this constant maximum width the process
of folding is used to wrap a line into multiple smaller lines and unfolding is the opposite process (but
not nessecarily the inverse function; an inverse would have been useful for quickcheck testing).

> module Text.ICalendar.Folding (fold, unfold) where
> 
> import Data.List
> import Data.Char
> 
> -- for some reason the constant unfold cs lines seem like they could be optimised.
> unfold :: String -> String
> unfold [] = []
> unfold ('\r':'\n':c:cs) = if isSpace c then unfold cs else '\r':'\n':unfold (c:cs)
> unfold ('\n':c:cs) = if isSpace c then unfold cs else '\n':unfold (c:cs)
> unfold (a:as) = a : unfold as
> 
> fold :: String -> String
> fold s = (intercalate "\r\n" . map (intercalate "\r\n ") . map foldLine . mylines $ s) ++ "\r\n"
>  where
>    foldLine :: String -> [String]
>    foldLine s
>      | length s > 75 = start : foldLine end
>      | otherwise = [s]
>      where
>        (start, end) = splitAt 75 s
> 
> mylines    :: String -> [String]
> mylines "" =  []
> mylines s  =  let (l, s') = break (\c -> (c == '\n' || c == '\r')) s
>               in  l : case s' of
>                         []      -> []
>                         ('\r':'\n':s'') -> mylines s''
>                         ('\n':s'') -> mylines s''
