module Main where

-- Some template functions that as a first step we wish to implement.

importICalendar :: String -> IO ICalendar
importICalendar filename = undefined

exportICalendar :: String -> ICalendar -> IO ()
exportICalendar filename calendar = undefined

readICalendar :: String -> ICalendar
readICalendar inputString = undefined

showICalendar :: ICalendar -> String
showICalendar calendar = undefined

-- showICalendar . readICalendar should be the identity, ignoring whitespace

-- There are many different structures that we can make for the ICalendar object

data ICalendar = ICalendar 
                    -- Calendar Metadata
                    { productID     :: String
                    , version       :: Float
                    , calscale      :: CalendarScale
                    , method        :: Method
                    , calendarName  :: String
                    -- Calendar Components
                    , events        :: [VEvent]
                    , todos         :: [VTodo]
                    , journals      :: [VJournal]
                    , freebusies    :: [VFreeBusy]
                    , timezones     :: [VTimezone]
                    -- Random Data for the generation of UUID's
                    -- , lowRandom :: Word8
                    -- , highRandom :: Word8
                    }

data CalendarScale = Gregorian    -- there are no others currently
data Method = Publish
            | Request

data VEvent = VEvent 
                { stamp         :: LocalTime
                , created       :: LocalTime
                , lastModified  :: LocalTime
                , eventStart    :: LocalTime
                , eventEnd      :: LocalTime
                , sequence      :: Int
                , description   :: String
                , location      :: String
                , status        :: Status
                , summary       :: String
                , transparency  :: Transparency
                , repeat        :: RepeatRule
                }

data Transparency = Transparent 
                  | Opaque

data VTodo
data VJournal
data VFreeBusy = VFreeBusy 
                { stamp :: LocalTime  -- Required
                , freebusyStart :: Maybe LocalTime -- Optional
                , freebusyEnd :: Maybe LocalTime -- Optional
                , contact :: Maybe ? -- Optional
                , organizer :: Maybe ? -- Optional
                , url :: Maybe ? -- Optional
                , attendees :: Maybe [?] -- Optional
                , comments :: Maybe [?] -- Optional
                , requestStatus :: Maybe RequestStatus -- Optional
data VTimezone
