VFreeBusy Module
================

> module Text.ICalendar.VFreeBusy where
>
> import Text.ICalendar.BaseTypes

> data VFreeBusy = VFreeBusy 
>                 { stamp         :: ICalDateTime  -- Required
>                 , freebusyStart :: Maybe ICalDateTime -- Optional
>                 , freebusyEnd   :: Maybe ICalDateTime -- Optional
>                 , contact       :: Maybe String -- Optional
>                 , organizer     :: Maybe String -- Optional this is a uri
>                 , url           :: Maybe String -- Optional
>                 , attendees     :: Maybe [Attendee] -- Optional
>                 , comments      :: [String] -- Optional
>                 , freebusies    :: [FreeBusyPeriod]                
>                 , requestStatus :: Maybe RequestStatus -- Optional
>                 }

> data FreeBusyType = Free
>                   | Busy
>                   | BusyUnavaliable
>                   | BusyTentative

This is defined in section __3.8.2.6__ and it is for the purpose of making it clear
when the free busy times are. All of the Free/Busy Periods in a VFreeBusy must be of
the same time and in ascending order based on start time and then end time.

> data FreeBusyPeriod = FreeBusyPeriod
>                           { freeBusyType :: Maybe FreeBusyType
>                           , freeBusyPeriods :: [Period]
>                           }
