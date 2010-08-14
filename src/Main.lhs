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

> module Main where

Property Vaule Data Types
-------------------------

The iCalendar is made up of a bunch of content lines each that have Property, Attribute
Value trios. The values that these properties can hold are.

 - Binary
 - Boolean (Natural: _Bool_)
 - Calendar User Address
 - Date
 - Date-Time
 - Duration
 - Float (Natural: _Rational_)
 - Integer (Natural: _Integer_)
 - Period of Time
 - Recurrence Rule
 - Text (Natural: _String_)
 - Time
 - URI (Uniform Resource Identifier)
 - UTC (Coordinated Universal Time) Offset

The Data that has been marked as natural are those that have natural representations in Haskell
and should just be used directly rather than through another definition.

> type Binary               = [Bool]
> type CalendarUserAddress  = URI
> type icalDate             = Day
> type icalDateTime         = LocalTime
> type Duration             = TimeDiff  -- may need a sign too for direction
> data Period = PeriodExplicit 
>               { periodStart :: icalDateTime 
>               , periodEnd   :: icalDateTime }
>             | PeriodStart icalDateTime Duration

The size and shape of the Recur structure is currently a mess. It suggests that there
should be a more elegant structure that will encompass the same functionlity.

> data Recur = Recur 
>     { recurFrequency :: Frequency
>     , recurUntilOrCount :: Maybe (Either (Either Date DateTime) Integer)  -- this could be cleaned up
>     , recurInterval :: Maybe Integer
>     , recurBySecond :: Maybe SecList
>     , recurByMinute :: Maybe MinList
>     , recurByHour :: Maybe HourList
>     , recurByDay :: Maybe WeekDayList
>     , recurByMonthDay :: Maybe ModayList
>     , recurByYearDay :: Maybe YearDayList
>     , recurByWeekNumber :: Maybe WeekNoList
>     , recurByMonth :: Maybe MonthList
>     , recurBySetPos :: Maybe SetPositionList
>     , recurWkSt :: Maybe System.Time.Day
>     }
> type SecList = [Integer]
> type MinList = [Integer]
> type HourList = [Integer]
> type WeekDayList = [WeekDayNum]
> type ModayList = [Integer]
> type YearDayList = [Integer]
> type WeekNoList = [Integer]
> type MonthList = [System.Time.Month]
> type SetPositionList = [Integer]
> -- WeekDayNum Sign OrdWeek DayOfWeek
> data WeekDayNum = WeekDayNum Sign Integer System.Time.Day
> type icalTime             = TimeOfDay
> -- for URI use import Network.URI
> data UTCOffset = UTCOffset Sign Integer Integer Integer
> data Sign = Positive | Negative

There are some interesting things to note about this:

 - The names Binary and Boolean do not seem so different but Binary could mean that an entire file could be
   attached in Binary to the property. Boolean means a single bit True or False as you would expect.
 - The types Boolean, Integer, Float and Text are so natural that there is no point just not using them straight up.


Use: 
 - LocalTime for Date-Time's
 - Day for Date's
 - TimeOfDay for Time's
 
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
 
> data VEvent = VEvent 
>                 { eventStamp    :: LocalTime
>                 , eventUID      :: UID
>                 , eventCreated  :: LocalTime
>                 , lastModified  :: LocalTime
>                 , eventStart    :: LocalTime
>                 , eventEnd      :: LocalTime
>                 , sequence      :: Int
>                 , description   :: String
>                 , location      :: String
>                 , status        :: Status
>                 , summary       :: String
>                 , transparency  :: Transparency
>                 , repeat        :: RepeatRule
>                 , eventAlarms   :: [VAlarm]
>                 }
> 
> data Transparency = Transparent 
>                   | Opaque
> 
> -- Each of the constructors is a different action property from 3.8.6.1
> data VAlarm = AudioProperties
>                 { alarmTrigger      :: Trigger
>                 , alarmDuration     :: Maybe Double -- in hours
>                 , alarmRepeat       :: Maybe Integer -- number of times to repeat alarm after trigger
>                 , alarmAttach       :: Maybe Attachment
>                 }
>             | DisplayProperties
>                 { alarmDescription  :: String
>                 , alarmTrigger      :: Trigger
>                 , alarmDuration     :: Maybe Double -- in hours
>                 , alarmRepeat       :: Maybe Integer -- number of times to repeat alarm after trigger
>                 }
>             | EmailProperties
>                 { alarmDescription        :: String
>                 , alarmTrigger            :: Trigger
>                 , alarmSummary            :: String
>                 , alarmAttendees          :: [Attendee] -- This list must have atleast one element.
>                 , alarmDurationAndRepeat  :: Maybe (Duration, Repeat)
>                 , alarmAttachments        :: [Attachment]
>                 }
> 
> 
> data Attachment = Attachment MimeType URI
> 
> -- They are all avaliable online here: http://www.iana.org/assignments/media-types/
> data MimeType -- this is defined by RFC4288
> 
> data Trigger = DurationTrigger Duration
>              | DateTimeTrigger LocalTime
> 
> data TimeOrDuration = DurationOption Duration
>                     | TimeOption DateType
> 
> data DateType = DateTime LocalTime
>               | Date Day
> 
> 
> data VTodo = VTodo 
>                 { todoStamp           :: LocalTime -- Required
>                 , todoCreated         :: Maybe LocalTime -- Optional
>                 , todoLastModified    :: Maybe LocalTime -- Optional
>                 , todoLocation        :: Maybe String -- Optional
>                 , todoStart           :: Maybe LocalTime -- Optional
>                 , todoDue             :: TimeOrDuration -- LocalTime or some other form of time Optional
>                 , todoClass           :: Maybe Classification -- Optional
>                 , todoGeo             :: Maybe (Double, Double) 
>                 , todoSummary         :: Maybe String
>                 , todoCompleted       :: Maybe LocalTime
>                 , todoDescription     :: Maybe String
>                 , todoAlarms          :: [VAlarm]
>                 , todoRRule           :: Maybe RRule
>                 -- These seem to be common to many different places and should be refactored (they are optional)
>                 , todoResources       :: Resources -- What are the resources? They seem to be a comma separated 
>                                                   -- list of resources that you will need for the event
>                 { todoAttachments     :: [Attachment] 
>                 , todoAttendees       :: [Attendee] 
>                 , todoCategories      :: [Category] 
>                 , todoComments        :: [String] 
>                 , todoContact         :: [Contact]
>                 , todoExcludingDates  :: [Times] -- Excluded dates from repeated events.
>                 , todoRequestStatus   :: Maybe RequestStatus
>                 , todoRelated         :: Maybe Relationship -- I think that this says that it is related to other issues, this suggests
>                                     -- some sort of indexing system
>                 , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
>                 }

Purpose: This property defines the equipment or resources anticipated for an activity specified by a 
calendar component.

> type Resource = String

> data Relationship = Child
>                   | Sibling
>                   | Parent
 
> data ParticipantStatus = NeedsAction
>                        | Accepted
>                        | Declined
>                        | Tentative
>                        | Delegated
>                        | Completed
>                        | InProcess
 
> data FreeBusyTypes = Free
>                    | Busy
>                    | BusyUnavaliable
>                    | BusyTentative
 
> data CalendarUserType = Individual
>                       | Group
>                       | Resource
>                       | Room
>                       | UserUnknown
 
> data ParticipationRoles = Chair
>                         | RequiredParticipant
>                         | OptionalParticipant
>                         | NonParticipant
 
> data Classification = Public
>                     | Private
>                     | Confidential
 
> data StatusResult = PreliminarySuccess Integer
>                   | Successful         Integer
>                   | ClientError        Integer
>                   | SchedulingError    Integer
 
> data Status = Status StatusResult String (Maybe String)
 
> data VJournal = VJournal 
>                 { journalStamp          :: LocalTime -- Required
>                 , journalUID            :: String -- The unique identifier
>                 , journalClass          :: Maybe Classification 
>                 , journalCreated        :: LocalTime
>                 , journalStart          :: Maybe DateType
>                 , journalLastModified   :: Maybe LocalTime
>                 , journalOrganizer      :: Maybe String
>                 , journalRecurrenceID   :: Maybe DateType
>                 , journalSequence       :: Maybe Integer
>                 , journalStatus         :: Maybe Status
>                 , journalSummary        :: Maybe String
>                 , journalUrl            :: Maybe String 
>                 , journalRRule          :: Maybe RRule
>                 , journalAttachments    :: [Attachment]
>                 , journalAttendees      :: [Attendee]
>                 , journalCategories     :: [Category]
>                 , journalComments       :: [Comment]
>                 , journalContact        :: [Contact]
>                 , journalDescription    :: Maybe String
>                 , journalExcludedDates  :: [DateType]
>                 , journalRelated        :: Maybe String -- The UID of the object that it is related to
>                 , journalRDate          :: Maybe ?
>                 , journalRequestStatus  :: Maybe Status
>                 }
> 
> data Recurrence = DateRecurrence RDate
>                 | RuleRecurrence RRule
> 
> data RDate = RDateTime  [LocalTime]
>            | RDate      [Day]
> 
> data VFreeBusy = VFreeBusy 
>                 { stamp         :: LocalTime  -- Required
>                 , freebusyStart :: Maybe LocalTime -- Optional
>                 , freebusyEnd   :: Maybe LocalTime -- Optional
>                 , contact       :: Maybe String -- Optional
>                 , organizer     :: Maybe String -- Optional this is a uri
>                 , url           :: Maybe String -- Optional
>                 , attendees     :: Maybe [Attendee] -- Optional
>                 , comments      :: Maybe [Comment] -- Optional
>                 , requestStatus :: Maybe RequestStatus -- Optional
>                 }
> 
> data VTimezone = VTimezone
>                 { timezoneID                :: Integer
>                 , timezoneLastModified      :: Maybe LocalTime
>                 , timezoneUrl               :: Maybe String
>                 , timezoneStandardCs        :: [TimeZoneProperty]
>                 , timezoneDaylightSavings   :: [TimeZoneProperty]
>                 }
> 
> data TimeZoneProperty = TZP 
>                 { tzpStart :: LocalTime
>                 , tzpOffsetTo :: UtcOffset
>                 , tzpOffsetFrom :: UtcOffset
>                 , tzpRRule :: Maybe RRule
>                 , tzpComment :: Maybe String
>                 , tzpRDate :: Maybe ?
>                 , tzName :: Maybe String
>                 }
> 