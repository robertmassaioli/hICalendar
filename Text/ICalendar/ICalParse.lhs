ICalendar Parser
================

This file is here for the purpose of parsing an ICalendar into a bunch of content lines. It uses
Parsec to do most of the heavy lifting work.

> module Text.ICalendar.ICalParse ( ContentLine(..), Attribute(..), icalendarFile ) where
> 
> import Text.ParserCombinators.Parsec

Define the data, a content line is defined as follows:

contentline = name *(";" param ) ":" value CRLF

TODO: Unfold the strings that get passed in. 

> data ContentLine = ContentLine 
>                     { contentName :: String
>                     , contentParameters :: [Attribute]
>                     , contentValue :: String }
>                  deriving(Show)
> 
> data Attribute = Attribute 
>                     { attributeName :: String 
>                     , attributeValue :: String }
>                deriving(Show) 

This is just the parser code, it grabs the lines and expects the right things at the right times.
Look at how beautifully small it all is:

> icalendarFile :: GenParser Char st [ContentLine]
> icalendarFile = do
>     result <- many line
>     eof
>     return result
> 
> line :: GenParser Char st ContentLine
> line = do
>     name <- lineName
>     params <- lineParams
>     char ':'
>     value <- lineValue
>     eol
>     return $ ContentLine { contentName = name, contentParameters = params, contentValue = value }
> 
> lineName :: GenParser Char st String
> lineName = do
>     many (noneOf ":;")
> 
> lineParams :: GenParser Char st [Attribute]
> lineParams = many lineAttribute
> 
> lineAttribute :: GenParser Char st Attribute
> lineAttribute = do
>     char ';'
>     name <- many (noneOf "=:;")
>     char '='
>     value <- many (noneOf ":;")
>     return $ Attribute { attributeName = name, attributeValue = value }
> 
> lineValue :: GenParser Char st String
> lineValue = many (noneOf "\n\r")
> 
> eol =   try (string "\r\n")
>     <|> try (string "\n\r")
>     <|> string "\n"
>     <|> string "\r"
>     <?> "end of line"
