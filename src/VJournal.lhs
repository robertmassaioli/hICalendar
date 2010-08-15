VJournal Module
===============

> module VJournal where
> 
> import BaseTypes

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
>                 , journalAttendees      :: [Attendee]
>                 , journalCategories     :: [Category]
>                 , journalComments       :: [String]
>                 , journalContact        :: [Contact]
>                 , journalDescription    :: Maybe String
>                 , journalExcludedDates  :: [DateType]
>                 , journalRelated        :: Maybe String -- The UID of the object that it is related to
>                 , journalRDate          :: Maybe RDate
>                 , journalRequestStatus  :: Maybe RequestStatus
>                 }

> data JournalStatus = Draft
>                    | Final
>                    | JournalCancelled
