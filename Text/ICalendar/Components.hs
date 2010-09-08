module Components where

import Text.ICalendar.BaseTypes

{-
 - TODO Check the list of all of the properties that each component should have and make sure that they have them then resolve the differences.
 - TODO Find a nice combined representation for repeat dates.
 -
 - Conflicts:
 -  Start and End/Duration will cause conflicts.
 -  Status may or may not be optional?
 -  Summaries may be optional.
 -  The use of the word related conflicts.
 -}

data Component = VEvent 
                  { stamp :: ICalDateTime
                  , uid :: UID
                  , createdStamp :: ICalDateTime
                  , lastModifiedStamp :: ICalDateTime
                  , start :: ICalDateTime
                  , end :: ICalDateTime
                  , sequence :: Integer
                  , description :: String
                  , summary :: String
                  , location :: String -- is it really a string?
                  , eventStatus        :: EventStatus -- the status is different for each I think
                  , transparency  :: Transparency
                  , repeat        :: RRule -- I think that this is wrong, we might need an object that can handle the RRule, RDate and ExDate triplet all at once. :D That will be fun.
                  , alarms   :: [VAlarm]
                  }
               | VTodo
                  { stamp           :: ICalDateTime
                  , createdStamp         :: Maybe ICalDateTime -- Optional
                  , lastModifiedStamp    :: Maybe ICalDateTime -- Optional
                  , location        :: Maybe String -- Optional
                  , start           :: Maybe ICalDateTime -- Optional
                  , due             :: TimeOrDuration -- ICalDateTime or some other form of time Optional
                  , classification           :: Maybe Classification -- Optional
                  , geo             :: Maybe (Double, Double) 
                  , summary         :: Maybe String
                  , description     :: Maybe String
                  , completed       :: Maybe ICalDateTime
                  , alarms          :: [VAlarm]
                  , rrule           :: Maybe RRule -- this makes sense, for example weekly shopping or chores
                  , exdates  :: [DateType] -- Excluded dates from repeated events.
                  , status          :: Maybe TodoStatus
                  -- These seem to be common to many different places and should be refactored (they are optional)
                  , resources       :: [Resource] 
                  , attachments     :: [Attachment] 
                  , attendees       :: [Attendee] 
                  , categories      :: [Category] 
                  , comments        :: [String] 
                  , contact         :: [Contact]
                  , requestStatus   :: Maybe RequestStatus
                  , related         :: Maybe Relationship -- I think that this says that it is related to other issues, this suggests
                                      -- some sort of indexing system
                  -- , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
                  }
               | VJournal
                  { stamp          :: ICalDateTime -- Required
                  , uid            :: UID -- The unique identifier
                  , classification          :: Maybe Classification 
                  , createdStamp        :: ICalDateTime
                  , lastModifiedStamp   :: Maybe ICalDateTime
                  , start          :: Maybe DateType
                  , organiser      :: Maybe String
                  , recurrence_id   :: Maybe DateType
                  , sequence       :: Maybe Integer
                  , status         :: Maybe JournalStatus
                  , summary        :: Maybe String
                  , description    :: Maybe String
                  , url            :: Maybe String 
                  , rrule          :: Maybe RRule -- what does a repeat rule even mean on a journal entry?
                  , rdate          :: Maybe RDate
                  , exdates        :: [DateType]
                  , attachments    :: [Attachment]
                  , attendees      :: [Attendee]
                  , categories     :: [Category]
                  , comments       :: [String]
                  , contacts        :: [Contact]
                  , related        :: Maybe String -- The UID of the object that it is related to
                  , requestStatus  :: Maybe RequestStatus
                  } 
	             | VFreeBusy
                  { stamp         :: ICalDateTime  -- Required
                  , start :: Maybe ICalDateTime -- Optional
                  , end   :: Maybe ICalDateTime -- Optional
                  , contact       :: Maybe String -- Optional
                  , organiser     :: Maybe String -- Optional this is a uri
                  , url           :: Maybe String -- Optional
                  , attendees     :: Maybe [Attendee] -- Optional
                  , comments      :: [String] -- Optional
                  , freebusies    :: [FreeBusyPeriod]                
                  , requestStatus :: Maybe RequestStatus -- Optional
                  }
               | VTimeZone

data VTodo = VTodo 
Section __3.8.1.11__ of the spec defines the todo items status.

data TodoStatus = NeedsAction
                | Completed
                | InProgress
                | TodoCancelled

Purpose: This property defines the equipment or resources anticipated for an activity specified by a 
calendar component.

> type Resource = String

> data Relationship = Child
>                   | Sibling
>                   | Parent

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
                , alarmDurationAndRepeat  :: Maybe (Duration, Repeat) -- Sometimes you want both properties
                , alarmAttachments        :: [Attachment]
                }

data Trigger = DurationTrigger Duration
             | DateTimeTrigger ICalDateTime

type Repeat = Integer -- the number of times to repeat the alarm

-- For events
data EventStatus = Tentative
                 | Confirmed
                 | EventCancelled

data Transparency = Transparent 
                  | Opaque
 
-- For Free/Busies
data FreeBusyType = Free
                  | Busy
                  | BusyUnavaliable
                  | BusyTentative

data FreeBusyPeriod = FreeBusyPeriod
                          { freeBusyType :: Maybe FreeBusyType
                          , freeBusyPeriods :: [Period]
                          }

data VJournal = VJournal 
data JournalStatus  = Draft
                    | Final
                    | JournalCancelled
