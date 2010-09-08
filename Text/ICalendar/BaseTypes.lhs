Property Vaule Data Types
=========================

> module Text.ICalendar.BaseTypes where
>
> import Data.Time.Calendar
> import Data.Time.LocalTime
> import System.Time
> import Network.URI

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
 - Text (Includes the language of the text too)
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
> data TimeOrDuration = DurationOption Duration
>                     | DateTimeOption ICalDateTime
>                     | DateOption ICalDate
> 
> data Period = PeriodExplicit 
>               { periodStart :: ICalDateTime 
>               , periodEnd   :: ICalDateTime }
>             | PeriodStart ICalDateTime Duration
>
> data Text = Text 
>               { textLanguage :: String
>               , value :: String 
>               }

The size and shape of the Recur structure is currently a mess. It suggests that there
should be a more elegant structure that will encompass the same functionlity.

> data RRule = Recur 
>     { recurFrequency :: Frequency
>     , recurUntilOrCount :: Maybe (Either DateType Integer)  -- this could be cleaned up
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
> data Sign = Positive | Negative
> type UTCOffset = TimeZone

There are some interesting things to note about this:

 - The names Binary and Boolean do not seem so different but Binary could mean that an entire file could be
   attached in Binary to the property. Boolean means a single bit True or False as you would expect.
 - The types Boolean, Integer, Float and Text are so natural that there is no point just not using them straight up.

With respect to date types use the following:

 - LocalTime for Date-Time's
 - Day for Date's
 - TimeOfDay for Time's
 - TimeZone for UTC Offsets

This is supposed to be a UID type but I think that it should be changed from a string eventually.

> type UID = String

The mime type would be better organised with a implementation of RFC4288.

> -- They are all avaliable online here: http://www.iana.org/assignments/media-types/
> type MimeType = String

An attachment is common to all of the ICalendar Components:

> data Attachment = Attachment MimeType URI

Section __3.8.1.3__ of the spec defines the classification level that various components may have.
This seems to only be used for Journals and Todo items.

> -- First Run
> data Classification = Public
>                     | Private
>                     | Confidential

This type is defined in section __3.8.1.2__ and it states that categories are just a comma separated
value type and the only extra property that they can have is the language param. The language tag is
defined in rfc5646 and it should be implemented too so that it can be used here (section __3.2.10__).

> -- TODO Add language param support to Catergories
> type Category = String

Contact details are anything that can be written as a string, like a street address.

> type Contact = String

The request status is defined in section __3.8.8.3__ and it contains the statuse code that is
returned for a scheduling request.

> data StatusResult = PreliminarySuccess Integer
>                   | Successful         Integer
>                   | ClientError        Integer
>                   | SchedulingError    Integer
 
> data RequestStatus = RequestStatus StatusResult String (Maybe String)

The RDate property feels like something that should be a common property to use.

> data RDate = RDateTime  [ICalDateTime]
>            | RDate      [ICalDate]

Calendar User Type is defined in section __3.2.3__ of the spec.

> data CalendarUserType = Individual
>                       | Group
>                       | Resource
>                       | Room
>                       | UserUnknown

Attendee's are defined in section __3.8.4.1__ of the spec.
 
> data Attendee = Attendee 
>                   { attendeeAddress :: CalendarUserAddress
>                   , attendeeType :: CalendarUserType }

Specified in section __3.8.4.4__ of the spec.

> data Organiser = Organiser
>                   { commonName :: Maybe String -- Optional
>                   , dir :: Maybe URI -- Optional
>                   , sentbyparam :: Maybe CalendarUserAddress -- Optional
>                   , language :: Maybe String -- Optional
>                   , address :: CalendarUserAddress
>                   }
>
> type Priority = Int -- Beware: This value is in the range 0-9 where 0 is undefined and 1 is High, 5 is Normal and 9 is Low
> type Percent = Int -- This number should be in the integer range [0-100]

