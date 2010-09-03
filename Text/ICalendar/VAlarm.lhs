VAlarm Module
=============

> module Text.ICalendar.VAlarm where
> 
> import Text.ICalendar.BaseTypes

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
>                 , alarmDurationAndRepeat  :: Maybe (Duration, Repeat) -- Sometimes you want both properties
>                 , alarmAttachments        :: [Attachment]
>                 }

> data Trigger = DurationTrigger Duration
>              | DateTimeTrigger ICalDateTime

> type Repeat = Integer -- the number of times to repeat the alarm
