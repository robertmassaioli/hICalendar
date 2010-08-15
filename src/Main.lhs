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
>
> import Network.URI
> import Data.Time.Calendar
> import Data.Time.LocalTime
> import System.Time

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
> type ICalDate             = Data.Time.Calendar.Day
> type ICalDateTime         = LocalTime
> type DateType             = Either ICalDateTime ICalDate
> type Duration             = TimeDiff  -- may need a sign too for direction
> data Period = PeriodExplicit 
>               { periodStart :: ICalDateTime 
>               , periodEnd   :: ICalDateTime }
>             | PeriodStart ICalDateTime Duration

The size and shape of the Recur structure is currently a mess. It suggests that there
should be a more elegant structure that will encompass the same functionlity.

> data RRule = Recur 
>     { recurFrequency :: Frequency
>     , recurUntilOrCount :: Maybe (Either (Either ICalDate ICalDateTime) Integer)  -- this could be cleaned up
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
> data Frequency = Secondly
>                | Minutely
>                | Hourly
>                | Daily
>                | Weekly
>                | Monthly
>                | Yearly
> -- WeekDayNum Sign OrdWeek DayOfWeek
> data WeekDayNum = WeekDayNum Sign Integer System.Time.Day
> type ICalTime             = TimeOfDay
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
 - TimeZone for UTC Offsets
 
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
>                 { eventStamp    :: ICalDateTime
>                 , eventUID      :: UID
>                 , eventCreated  :: ICalDateTime
>                 , lastModified  :: ICalDateTime
>                 , eventStart    :: ICalDateTime
>                 , eventEnd      :: ICalDateTime
>                 , sequence      :: Int
>                 , description   :: String
>                 , location      :: String
>                 , status        :: EventStatus
>                 , summary       :: String
>                 , transparency  :: Transparency
>                 , repeat        :: RRule
>                 , eventAlarms   :: [VAlarm]
>                 }

This is supposed to be a UID type but I think that it should be changed from a string eventually.

> type UID = String

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
>                 , alarmAttendees          :: [CalendarUserAddress] -- This list must have atleast one element.
>                 , alarmDurationAndRepeat  :: Maybe (Duration, Repeat) -- Sometimes you want both properties
>                 , alarmAttachments        :: [Attachment]
>                 }
> 
> type Repeat = Integer -- the number of times to repeat the alarm
> 
> data Attachment = Attachment MimeType URI
>

The mime type would be better organised with a implementation of RFC4288.

> -- They are all avaliable online here: http://www.iana.org/assignments/media-types/
> type MimeType = String
> 
> data Trigger = DurationTrigger Duration
>              | DateTimeTrigger ICalDateTime
> 
> data TimeOrDuration = DurationOption Duration
>                     | DateTimeOption ICalDateTime
>                     | DateOption ICalDate
> 
> data VTodo = VTodo 
>                 { todoStamp           :: ICalDateTime
>                 , todoCreated         :: Maybe ICalDateTime -- Optional
>                 , todoLastModified    :: Maybe ICalDateTime -- Optional
>                 , todoLocation        :: Maybe String -- Optional
>                 , todoStart           :: Maybe ICalDateTime -- Optional
>                 , todoDue             :: TimeOrDuration -- ICalDateTime or some other form of time Optional
>                 , todoClass           :: Maybe Classification -- Optional
>                 , todoGeo             :: Maybe (Double, Double) 
>                 , todoSummary         :: Maybe String
>                 , todoCompleted       :: Maybe ICalDateTime
>                 , todoDescription     :: Maybe String
>                 , todoAlarms          :: [VAlarm]
>                 , todoRRule           :: Maybe RRule
>                 , todoStatus          :: Maybe TodoStatus
>                 -- These seem to be common to many different places and should be refactored (they are optional)
>                 , todoResources       :: [Resource] 
>                 , todoAttachments     :: [Attachment] 
>                 , todoAttendees       :: [CalendarUserAddress] 
>                 , todoCategories      :: [Category] 
>                 , todoComments        :: [String] 
>                 , todoContact         :: [Contact]
>                 , todoExcludingDates  :: [DateType] -- Excluded dates from repeated events.
>                 , todoRequestStatus   :: Maybe RequestStatus
>                 , todoRelated         :: Maybe Relationship -- I think that this says that it is related to other issues, this suggests
>                                     -- some sort of indexing system
>                 -- , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
>                 }

Purpose: This property defines the equipment or resources anticipated for an activity specified by a 
calendar component.

> type Resource = String

Contact details are anything that can be written as a string, like a street address.

> type Contact = String

This type is defined in section __3.8.1.2__ and it states that categories are just a comma separated
value type and the only extra property that they can have is the language param. The language tag is
defined in rfc5646 and it should be implemented too so that it can be used here (section __3.2.10__).

> -- TODO Add language param support to Catergories
> type Category = String

> data Relationship = Child
>                   | Sibling
>                   | Parent
 
In section 3.8.1.11 of the Spec it gives one status type but I think that this is wrong I would sleep more 
soundly knowing that you can only put the correct types in the todo item.

> data TodoStatus = NeedsAction
>                 | Completed
>                 | InProgress
>                 | TodoCancelled

> data EventStatus = Tentative
>                  | Confirmed
>                  | EventCancelled

> data JournalStatus = Draft
>                    | Final
>                    | JournalCancelled

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
 
> data RequestStatus = RequestStatus StatusResult String (Maybe String)
 

> data VJournal = VJournal 
>                 { journalStamp          :: ICalDateTime -- Required
>                 , journalUID            :: String -- The unique identifier
>                 , journalClass          :: Maybe Classification 
>                 , journalCreated        :: ICalDateTime
>                 , journalStart          :: Maybe DateType
>                 , journalLastModified   :: Maybe ICalDateTime
>                 , journalOrganizer      :: Maybe String
>                 , journalRecurrenceID   :: Maybe DateType
>                 , journalSequence       :: Maybe Integer
>                 , journalStatus         :: Maybe JournalStatus
>                 , journalSummary        :: Maybe String
>                 , journalUrl            :: Maybe String 
>                 , journalRRule          :: Maybe RRule
>                 , journalAttachments    :: [Attachment]
>                 , journalAttendees      :: [CalendarUserAddress]
>                 , journalCategories     :: [Category]
>                 , journalComments       :: [String]
>                 , journalContact        :: [Contact]
>                 , journalDescription    :: Maybe String
>                 , journalExcludedDates  :: [DateType]
>                 , journalRelated        :: Maybe String -- The UID of the object that it is related to
>                 , journalRDate          :: Maybe RDate
>                 , journalRequestStatus  :: Maybe RequestStatus
>                 }
> 
> data Recurrence = DateRecurrence RDate
>                 | RuleRecurrence RRule
> 
> data RDate = RDateTime  [ICalDateTime]
>            | RDate      [ICalDate]
> 
> data VFreeBusy = VFreeBusy 
>                 { stamp         :: ICalDateTime  -- Required
>                 , freebusyStart :: Maybe ICalDateTime -- Optional
>                 , freebusyEnd   :: Maybe ICalDateTime -- Optional
>                 , contact       :: Maybe String -- Optional
>                 , organizer     :: Maybe String -- Optional this is a uri
>                 , url           :: Maybe String -- Optional
>                 , attendees     :: Maybe [CalendarUserAddress] -- Optional
>                 , comments      :: Maybe [String] -- Optional
>                 , requestStatus :: Maybe RequestStatus -- Optional
>                 }
> 
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
>                 , tzpOffsetTo :: TimeZone
>                 , tzpOffsetFrom :: TimeZone
>                 , tzpRRule :: Maybe RRule
>                 , tzpComment :: Maybe String
>                 , tzpRDate :: Maybe RDate
>                 , tzName :: Maybe String
>                 }
