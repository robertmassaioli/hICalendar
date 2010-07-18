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

{-
 - Use: 
 -  LocalTime for Date-Time's
 -  Day for Date's
 -  TimeOfDay for Time's
 -}

data Times = DateTime LocalTime
           | Date Day
           | Time TimeOfDay

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

-- Each of the constructors is a different action property from 3.8.6.1
data VAlarm = AudioProperties
                { alarmTrigger      :: Trigger
                , alarmDuration     :: Maybe Double -- in hours
                , alarmRepeat       :: Maybe Integer -- number of times to repeat alarm after trigger
                , alarmAttach       :: Maybe Attachment
                }
            | DisplayProperties
                { alarmDescription  :: String
                , alarmTrigger      :: Trigger
                , alarmDuration     :: Maybe Double -- in hours
                , alarmRepeat       :: Maybe Integer -- number of times to repeat alarm after trigger
                }
            | EmailProperties
                { alarmDescription        :: String
                , alarmTrigger            :: Trigger
                , alarmSummary            :: String
                , alarmAttendees          :: [Attendee] -- This list must have atleast one element.
                , alarmDurationAndRepeat  :: Maybe ?
                , alarmAttachments        :: [Attachment]
                }

data Attachment = Attachment MimeType URI

data URI -- this is defined by RFC 3986

-- They are all avaliable online here: http://www.iana.org/assignments/media-types/
data MimeType -- this is defined by RFC4288

data Trigger = DurationTrigger Duration
             | DateTimeTrigger LocalTime

data TimeOrDuration = DurationOption Duration
                    | TimeOption DateType

data DateType = DateTime LocalTime
              | Date Day


data VTodo = VTodo 
                { todoStamp           :: LocalTime -- Required
                , todoCreated         :: Maybe LocalTime -- Optional
                , todoLastModified    :: Maybe LocalTime -- Optional
                , todoLocation        :: Maybe String -- Optional
                , todoStart           :: Maybe LocalTime -- Optional
                , todoDue             :: TimeOrDuration -- LocalTime or some other form of time Optional
                , todoClass           :: Maybe Classification -- Optional
                , todoGeo             :: Maybe (Double, Double) 
                , todoSummary         :: Maybe String
                , todoCompleted       :: Maybe LocalTime
                , todoDescription     :: Maybe String
                , todoAlarms          :: [VAlarm]
                , todoRRule           :: Maybe RRule
                -- These seem to be common to many different places and should be refactored (they are optional)
                , todoResources       :: ? -- What are the resources
                { todoAttachments     :: [Attachment] 
                , todoAttendees       :: [Attendee] 
                , todoCategories      :: [Category] 
                , todoComments        :: [String] 
                , todoContact         :: [Contact]
                , todoExcludingDates  :: [Times] -- Excluded dates from repeated events.
                , todoRequestStatus   :: Maybe RequestStatus
                , todoRelated         :: Maybe Relationship -- I think that this says that it is related to other issues, this suggests
                                    -- some sort of indexing system
                , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
                }

data Relationship = Child
                  | Sibling
                  | Parent

data ParticipantStatus = NeedsAction
                       | Accepted
                       | Declined
                       | Tentative
                       | Delegated
                       | Completed
                       | InProcess

data FreeBusyTypes = Free
                   | Busy
                   | BusyUnavaliable
                   | BusyTentative

data CalendarUserType = Individual
                      | Group
                      | Resource
                      | Room
                      | UserUnknown

data ParticipationRoles = Chair
                        | RequiredParticipant
                        | OptionalParticipant
                        | NonParticipant

data Classification = Public
                    | Private
                    | Confidential

data StatusResult = PreliminarySuccess Integer
                  | Successful         Integer
                  | ClientError        Integer
                  | SchedulingError    Integer

data Status = Status StatusResult String (Maybe String)

data VJournal = VJournal 
                { journalStamp          :: LocalTime -- Required
                , journalUID            :: String -- The unique identifier
                , journalClass          :: Maybe Classification 
                , journalCreated        :: LocalTime
                , journalStart          :: Maybe DateType
                , journalLastModified   :: Maybe LocalTime
                , journalOrganizer      :: Maybe String
                , journalRecurrenceID   :: Maybe DateType
                , journalSequence       :: Maybe Integer
                , journalStatus         :: Maybe Status
                , journalSummary        :: Maybe String
                , journalUrl            :: Maybe String 
                , journalRRule          :: Maybe RRule
                , journalAttachments    :: [Attachment]
                , journalAttendees      :: [Attendee]
                , journalCategories     :: [Category]
                , journalComments       :: [Comment]
                , journalContact        :: [Contact]
                , journalDescription    :: Maybe String
                , journalExcludedDates  :: [DateType]
                , journalRelated        :: Maybe String -- The UID of the object that it is related to
                , journalRDate          :: Maybe ?
                , journalRequestStatus  :: Maybe Status
                }

data Recurrence = DateRecurrence RDate
                | RuleRecurrence RRule

data RDate = RDateTime  [LocalTime]
           | RDate      [Day]

data VFreeBusy = VFreeBusy 
                { stamp         :: LocalTime  -- Required
                , freebusyStart :: Maybe LocalTime -- Optional
                , freebusyEnd   :: Maybe LocalTime -- Optional
                , contact       :: Maybe String -- Optional
                , organizer     :: Maybe String -- Optional this is a uri
                , url           :: Maybe String -- Optional
                , attendees     :: Maybe [Attendee] -- Optional
                , comments      :: Maybe [Comment] -- Optional
                , requestStatus :: Maybe RequestStatus -- Optional
                }

data VTimezone = VTimezone
                { timezoneID                :: Integer
                , timezoneLastModified      :: Maybe LocalTime
                , timezoneUrl               :: Maybe String
                , timezoneStandardCs        :: [TimeZoneProperty]
                , timezoneDaylightSavings   :: [TimeZoneProperty]
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
