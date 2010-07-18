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
                { eventStamp         :: LocalTime
                , eventCreated       :: LocalTime
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
                , eventAlarms   :: [VAlarm]
                }

data Transparency = Transparent 
                  | Opaque

data VTodo = VTodo 
                { todoStamp :: LocalTime -- Required
                , todoCreated :: Maybe LocalTime -- Optional
                , todoLastModified :: Maybe LocalTime -- Optional
                , todoLocation :: Maybe String -- Optional
                , todoStart :: Maybe LocalTime -- Optional
                , todoDueOrDuration :: ? -- LocalTime or some other form of time Optional
                , todoClass -- Optional
                , todoGeo :: ? -- Optional - This is a geographical location I have no idea how this should look.
                , todoSummary :: Maybe String
                , todoCompleted :: Maybe LocalTime
                , todoDescription :: Maybe String
                , todoAlarms :: [VAlarm]
                , todoRRule :: Maybe RRule
                -- These seem to be common to many different places and should be refactored (they are optional)
                , todoResources :: ? -- What are the resources
                { todoAttachments :: [Attachment] 
                , todoAttendees :: [Attendee] 
                , todoCategories :: [Category] 
                , todoComments :: [String] 
                , todoContact :: [Contact]
                , todoExDate :: ? -- I don't even know what this property is
                , todoRequestStatus :: Maybe RequestStatus
                , todoRelated :: ? -- I think that this says that it is related to other issues, this suggests
                                    -- some sort of indexing system
                , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
                }

data VJournal = VJournal 
                { journalStamp :: LocalTime -- Required
                , journalUID :: ? -- Required but what is it?
                , journalClass :: Maybe ? 
                , journalCreated :: LocalTime
                , journalStart :: Maybe ?
                , journalLastModified :: Maybe LocalTime
                , journalOrganizer :: Maybe String
                , journalRecurID :: Maybe ?
                , journalSeq :: Maybe ?
                , journalStatus :: Maybe ?
                , journalSummary :: Maybe ?
                , journalUrl :: Maybe ? 
                , journalRRule :: Maybe RRule
                , journalAttachments :: [Attachment]
                , journalAttendees :: [Attendee]
                , journalCategories :: [Category]
                , journalComments :: [Comment]
                , journalContact :: [Contact]
                , journalDescription :: Maybe String
                , journalExDate :: Maybe LocalTime -- expiration date?
                , journalRelated :: Maybe ?
                , journalRDate :: Maybe ?
                , journalRequestStatus :: Maybe ?
                }

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
                }

data VTimezone = VTimezone
                { timezoneID :: Integer
                , timezoneLastModified :: Maybe LocalTime
                , timezoneUrl :: Maybe ? 
                , timezoneStandardCs :: [TimeZoneProperty]
                , timezoneDaylightSavings :: [TimeZoneProperty]
                }

data TimeZoneProperty = TZP 
                { tzpStart :: LocalTime
                , tzpOffsetTo :: ?
                , tzpOffsetFrom :: ?
                , tzpRRule :: Maybe RRule
                , tzpComment :: Maybe String
                , tzpRDate :: Maybe ?
                , tzName :: Maybe String
                }
