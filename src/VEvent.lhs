VEvent Module
=============

> module VEvent where
>
> import BaseTypes
> import VAlarm

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

> data EventStatus = Tentative
>                  | Confirmed
>                  | EventCancelled

> data Transparency = Transparent 
>                   | Opaque
> 
