ICalendar Library
=================

> {-
> importICalendar :: String -> IO ICalendar
> importICalendar filename = undefined
> 
> exportICalendar :: String -> ICalendar -> IO ()
> exportICalendar filename calendar = undefined
> 
> readICalendar :: String -> ICalendar
> readICalendar inputString = undefined
> 
> showICalendar :: ICalendar -> String
> showICalendar calendar = undefined
> -}

> -- showICalendar . readICalendar should be the identity, ignoring whitespace 
> -- There are many different structures that we can make for the ICalendar object

This library attemts to be an excellent implementation of the rfc5545
__Internet Calendaring and Scheduling Core Object Specification__
and will have documentation that is so good that it will be obvious if a mistake
was made or not. You should be able to read the source code and understand what the spec
said at the same time.

Naming
------

For the sake of this document we need naming conventions and this is how we will refer to things:

 - 'Calendar Components' are all of the VEvents, VTodos, VJournals, lAlarms and VFreeBusies all at once
   so that we can refer to them in a group manner

> module Main where
>
> import Network.URI
> import BaseTypes
> import VEvent
> import VTodo
> import VJournal
> import VFreeBusy
 
The iCalendar must have atleast one component which is not made obvious by this structure

> data ICalendar = ICalendar 
>                     -- Calendar Metadata
>                     { productID     :: String
>                     , version       :: Float
>                     , calscale      :: Maybe CalendarScale
>                     , method        :: Maybe Method
>                     , calendarName  :: String
>                     -- Calendar Components
>                     , events        :: [VEvent]
>                     , todos         :: [VTodo]
>                     , journals      :: [VJournal]
>                     , freebusies    :: [VFreeBusy]
>                     , timezones     :: [VTimezone]
>                     -- Random Data for the generation of UUID's
>                     -- , lowRandom :: Word8
>                     -- , highRandom :: Word8
>                     }

Purpose: This property defines the calendar scale used for the
calendar information specified in the iCalendar object.

Currently there is only one type of calendar scale but who knows, there
may be more in the future so this will be used in its place. 

Conformance: This property can be specified once in an iCalendar
object. The default value is "GREGORIAN".

> data CalendarScale = Gregorian

> data Method = Publish
>             | Request
 

In section __3.8.1.11__ of the Spec it gives one status type but I think that this is wrong I would sleep more 
soundly knowing that you can only put the correct types the right items.

> data ParticipationRoles = Chair
>                         | RequiredParticipant
>                         | OptionalParticipant
>                         | NonParticipant
 
> data VTimezone = VTimezone
>                 { timezoneID                :: Integer
>                 , timezoneLastModified      :: Maybe ICalDateTime
>                 , timezoneUrl               :: Maybe String
>                 , timezoneStandardCs        :: [TimeZoneProperty]
>                 , timezoneDaylightSavings   :: [TimeZoneProperty]
>                 }
> 
> data TimeZoneProperty = TZP 
>                 { tzpStart :: ICalDateTime
>                 , tzpOffsetTo :: UTCOffset
>                 , tzpOffsetFrom :: UTCOffset 
>                 , tzpRRule :: Maybe RRule
>                 , tzpComment :: Maybe String
>                 , tzpRDate :: Maybe RDate
>                 , tzName :: Maybe String
>                 }
