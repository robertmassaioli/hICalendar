VTodo Module
============

> module Text.ICalendar.VTodo where
>
> import Text.ICalendar.BaseTypes
> import Text.ICalendar.VAlarm

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
>                 , todoAttendees       :: [Attendee] 
>                 , todoCategories      :: [Category] 
>                 , todoComments        :: [String] 
>                 , todoContact         :: [Contact]
>                 , todoExcludingDates  :: [DateType] -- Excluded dates from repeated events.
>                 , todoRequestStatus   :: Maybe RequestStatus
>                 , todoRelated         :: Maybe Relationship -- I think that this says that it is related to other issues, this suggests
>                                     -- some sort of indexing system
>                 -- , todoProperties :: ? -- These are extra vendor specific properties meant for extensions
>                 }

Section __3.8.1.11__ of the spec defines the todo items status.

> data TodoStatus = NeedsAction
>                 | Completed
>                 | InProgress
>                 | TodoCancelled

Purpose: This property defines the equipment or resources anticipated for an activity specified by a 
calendar component.

> type Resource = String

> data Relationship = Child
>                   | Sibling
>                   | Parent
