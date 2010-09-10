module Text.ICalendar.Components where

import Text.ICalendar.BaseTypes
import Network.URI

{-
 - Rules
 -  - If the item is only allowed to appear once and it is required then it has type 'a'
 -  - If the item is only allowed to appear once but it is optional then it has type 'Maybe a'
 -  - If the item can have multiple entries then it is always displayed as a type '[a]' wether it is optional or not.
 -
 - TODO Check the list of all of the properties that each component should have and make sure that they have them then resolve the differences.
 - TODO Find a nice combined representation for repeat dates, rules and exdates so that they can be superimposed.
 - TODO Find a way to have a nice extension mechanism for all of the X-* properties.
 - TODO Actually use the Text parameter because it allows you to specify the language that is being used.
 - TODO Figure out how recurrance_id's work. It works in conjunction with sequence and uid apparently.
 - TODO Maybe change DateType to use the forall. construct so that you can just define a single class that does what you need to and
 -      pattern matching on which one it is becomes a thing of the past.
 - TODO Find out what the 'related' property is on about.
 - TODO Is there a way of merging the journal, event and todo statuses?
 -
 -}

data ICalendar = ICalendar -- Not checked
                  { version :: Double
                  , productID :: String
                  , calendarScale :: CalendarScale
                  , method :: String -- Nothing is specified but maybe a request
                  , components :: [Component]
                  }

data Component = VEvent  -- First Run
                  { stamp               :: ICalDateTime -- Required
                  , uid                 :: UID -- Required
                  , sequence            :: Maybe Integer -- Optional
                  , recurrance_id       :: Maybe DateType -- Optional
                  , createdStamp        :: Maybe ICalDateTime -- Optional
                  , lastModifiedStamp   :: Maybe ICalDateTime -- Optional
                  , start               :: Maybe DateType -- Sometimes required and sometimes optional, nasty
                  , endOrDuration       :: Maybe TimeOrDuration -- What happens when you dont have this?
                  , description         :: Maybe DescriptiveText -- Optional
                  , summary             :: Maybe DescriptiveText -- Optional
                  , organiser           :: Maybe Organiser
                  , priority            :: Maybe Priority -- Optional
                  , location            :: Maybe DescriptiveText -- Optional The value is a Text parameter but it has an altparam too
                  , geo                 :: Maybe GeographicPosition -- Optional
                  , classification      :: Maybe Classification -- Optional
                  , eventStatus         :: Maybe EventStatus -- Optional
                  , transparency        :: Maybe Transparency -- Optional
                  , rrule               :: Maybe RRule -- I think that this is wrong, we might need an object that can handle the RRule, RDate and ExDate triplet all at once. :D That will be fun.
                  , exdate              :: [DateType]
                  , rdate               :: [RDate]
                  , url                 :: Maybe URI
                  , attachments         :: [Attachment]
                  , attendees           :: [Attendee]
                  , categories          :: [Text]
                  , comments            :: [DescriptiveText]
                  , contacts            :: [DescriptiveText]
                  , requestStatus       :: [RequestStatus]
                  , related             :: [Relationship]
                  , resources           :: [DescriptiveText]
                  , alarms              :: [VAlarm]
                  }
               | VTodo -- First Run (3.6.2)
                  { stamp               :: ICalDateTime -- Required
                  , uid                 :: UID -- Required
                  , url                 :: Maybe URI
                  , createdStamp        :: Maybe ICalDateTime -- Optional
                  , lastModifiedStamp   :: Maybe ICalDateTime -- Optional
                  , start               :: Maybe DateType -- Optional
                  , due                 :: TimeOrDuration -- ICalDateTime or some other form of time Optional
                  , classification      :: Maybe Classification -- Optional
                  , location            :: Maybe DescriptiveText -- Optional
                  , geo                 :: Maybe GeographicPosition -- Optional
                  , summary             :: Maybe DescriptiveText -- Optional
                  , todoStatus          :: Maybe TodoStatus
                  , description         :: Maybe DescriptiveText -- Optional
                  , completed           :: Maybe ICalDateTime -- Optional (3.8.2.1)
                  , rrule               :: Maybe RRule -- this makes sense, for example weekly shopping or chores
                  , rdate               :: [RDate]
                  , exdates             :: [DateType] -- Excluded dates from repeated events.
                  , organiser           :: Maybe Organiser -- Optional
                  , percent             :: Maybe Percent
                  , resources           :: [DescriptiveText] 
                  , attachments         :: [Attachment] 
                  , attendees           :: [Attendee] 
                  , categories          :: [Text] 
                  , comments            :: [DescriptiveText] 
                  , contacts            :: [DescriptiveText]
                  , requestStatus       :: [RequestStatus]
                  , related             :: [Relationship] -- I think that this says that it is related to other issues, this suggests it should probably be a UID reference
                  , alarms              :: [VAlarm]
                  }
               | VJournal -- First Run
                  { stamp               :: ICalDateTime -- Required
                  , uid                 :: UID -- Required The unique identifier
                  , classification      :: Maybe Classification -- Optional
                  , createdStamp        :: Maybe ICalDateTime -- Optional
                  , lastModifiedStamp   :: Maybe ICalDateTime -- Optional
                  , start               :: Maybe DateType -- Optional
                  , organiser           :: Maybe Organiser -- Optional
                  , recurrence_id       :: Maybe DateType -- Optional
                  , sequence            :: Maybe Integer -- Optional
                  , journalStatus       :: Maybe JournalStatus -- Optional
                  , summary             :: Maybe DescriptiveText -- Optional
                  , description         :: Maybe DescriptiveText -- Optional (Technically there can be more than one of these, just append it together)
                  , url                 :: Maybe URI
                  , rrule               :: Maybe RRule -- what does a repeat rule even mean on a journal entry?
                  , rdate               :: [RDate]
                  , exdates             :: [DateType]
                  , attachments         :: [Attachment]
                  , attendees           :: [Attendee]
                  , categories          :: [Text]
                  , comments            :: [DescriptiveText]
                  , contacts            :: [DescriptiveText]
                  , related             :: [Relationship]
                  , requestStatus       :: [RequestStatus]
                  } 
	             | VFreeBusy -- First Run
                  { stamp               :: ICalDateTime  -- Required
                  , uid                 :: UID -- Required
                  , start               :: Maybe DateType -- Optional
                  , end                 :: Maybe ICalDateTime -- Optional
                  , contact             :: Maybe Text -- Optional
                  , organiser           :: Maybe Organiser -- Optional this is a uri
                  , url                 :: Maybe URI -- Optional
                  , attendees           :: [Attendee] -- Optional
                  , comments            :: [DescriptiveText] -- Optional
                  , freebusies          :: [FreeBusyPeriod] -- Optional                
                  , requestStatus       :: [RequestStatus] -- Optional
                  }
               | VTimeZone
                  { lastModified        :: Maybe ICalDateTime -- Optional
                  , tzid                :: String -- Example: TZID:/example.org/America/New_York (All TZID's must begin with a forward slash)
                  , url                 :: Maybe URI -- Optional
                  , zones               :: [TimeZoneProperty] -- need atleast one item
                  }

-- For Timezones
data TimeZoneProperty = TZP 
                            { zoneType    :: TimeZoneType
                            , timezoneStart       :: ICalDateTime
                            , offsetTo    :: UTCOffset
                            , offsetFrom  :: UTCOffset
                            , timezoneRRule       :: Maybe RRule -- Optional
                            , timezoneComments    :: [String]
                            , timezoneRDate       :: [RDate] -- Optional
                            , name        :: [Text] -- Many and optional
                            }

data TimeZoneType = StandardZone | DaylightZone
                            
data Relationship = Child
                  | Sibling
                  | Parent

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

data JournalStatus  = Draft
                    | Final
                    | JournalCancelled

-- For todo's
data TodoStatus = NeedsAction
                | Completed
                | InProgress
                | TodoCancelled
