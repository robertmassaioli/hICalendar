module Properties where

import Text.ParserCombinators.Parsec

-- I think that they take in a component and spit a component back out and change whatever they found in the middle


parseICalendar :: GenParser ContentLine st ICalendar
parseICalendar = do
  icalBegin
  many (calProperties <|> components)
  icalEnd
  where
    calProperties :: ICalendar -> GenParser ContentLine st ICalendar

components :: GenParser ContentLine st Component

lineWithName :: String -> GenParser ContentLine st ICalendar


calscale :: ICalendar -> GenParser ContentLine st ICalendar
calscale ical = do
                  name -- try CALSCALE and then

                where
                  name = string "CALSCALE"

line :: a -> CharParser st a
line = do
    lineName itsName
    params <- lineParams allowedParams
    char ':'
    value <- lineValue
    eol
    return $ ContentLine { contentName = name, contentParameters = params, contentValue = value }

lineParams :: CharParser st [Attribute]
lineParams = many lineAttribute

lineAttribute :: CharParser st Attribute
lineAttribute = do
    char ';'
    name <- many (noneOf "=:;")
    char '='
    value <- many (noneOf ":;")
    return $ Attribute { attributeName = name, attributeValue = value }

method :: Component -> GenParser Char st Component
productID :: Component -> GenParser Char st Component
version
attachment
categories
classification
comment
description
geolocation
location
percentComplete
priority
resources
status
summary
completed
dtend
due
duration
freebusy
transp
timezoneID
timezoneName
timezoneOffsetFrom
timezoneOffsetTo
timezoneUrl
attendee
contact
organiser
recurrenceID
relatedTo
url
uid
exdate
-- exrule deprecated
rdate
rrule
action
repeat
trigger
created
dtstamp
lastModified
sequence
requestStatus
