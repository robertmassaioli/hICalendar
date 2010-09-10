{- 
 - ICalendar Parser
 - This file is here for the purpose of parsing an ICalendar into a bunch of content lines. It uses
 - Parsec to do most of the heavy lifting work.
 -
 - But maybe it would be better to come up with a list of tokens and then go from there?
 - TODO decide how you wish to approach the problem of parsing the grammer in the way
 -      that makes the most sense.
-}

{-
 - The Approach
 -
 -    I have decided that a good first attempt to try would be to sit there and implement every property
 -    first and then write up every different type of parameter, the different value types and then make
 -    the higher level components use them. That way it mirrors the rfc as best as we can. It may turn out
 -    that it generates a large amount of code but it will be crystal clear exactly how everything works
 -    and direct comparison to the spec should be rather easy.
 -
 -    Other options include using Alex to generate a token stream and parse them or even use parsec twice to
 -    first create a list of content lines and then parse them. We could also not use parsec at all but I see
 -    that as a bit of a mistake.
 -}

module Text.ICalendar.ICalParse ( ContentLine(..), Attribute(..), icalendarFile ) where

import Text.ParserCombinators.Parsec

{-
Define the data, a content line is defined as follows:

contentline = name *(";" param ) ":" value CRLF

TODO: Unfold the strings that get passed in. 
-}

data ContentLine = ContentLine 
                    { contentName :: String
                    , contentParameters :: [Attribute]
                    , contentValue :: String }
                 deriving(Show)

data Attribute = Attribute 
                    { attributeName :: String 
                    , attributeValue :: String }
               deriving(Show) 

-- This is just the parser code, it grabs the lines and expects the right things at the right times.
-- Look at how beautifully small it all is:

icalendarFile :: CharParser st [ContentLine]
icalendarFile = do
    result <- many line
    eof
    return result

line :: CharParser st ContentLine
line = do
    name <- lineName
    params <- lineParams
    char ':'
    value <- lineValue
    eol
    return $ ContentLine { contentName = name, contentParameters = params, contentValue = value }

lineName :: CharParser st String
lineName = do
    many (noneOf ":;")

lineParams :: CharParser st [Attribute]
lineParams = many lineAttribute

lineAttribute :: CharParser st Attribute
lineAttribute = do
    char ';'
    name <- many (noneOf "=:;")
    char '='
    value <- many (noneOf ":;")
    return $ Attribute { attributeName = name, attributeValue = value }

lineValue :: CharParser st String
lineValue = many (noneOf "\n\r")

eol =   try (string "\r\n")
    <|> try (string "\n\r")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
