iCalendar Spec in Five Minutes
==============================

(Five minutes gives me roughly 400 * 5 minutes)

The iCalendar specification may seem big and scary at first but it is really not so bad. Lets break it down piece by piece and find out
what a tame beast it really is. As we go through just keep in mind that the two purposes of this entire document is to create a
calendar of events and to help organise information surrounding events.

The Components
--------------

The iCalendar is broked down logically into components. These components and their purposes are:

 - VEvent: To record events that would occur on a calendar.
 - VTodo: To record items that need to be done/performed.
 - VJournal: To keep a comprehensive record of what occured on an event.
 - VFreeBusy: To act as a way to share calendar avaliability information without revealing the event itself.
 - VAlarm: To act as a recorder of the ways in which you can alert the user to an event or todo item.
 - VTimezone: To keep a record of timezone information.

These events, when you look at them, are surprisingly simple and they are all that an iCalendar can ever contain as is mandated
by the specification. And it should be noted that their are relationships present bettween the components (they do not live
in isolation). For example, the VAlarm component must be attatched to a VEvent and the VFreeBusy component will probably be
created based on existing events or todo's in another calendar. For the sake of clarity we will refer to these without the 'V'
character appended to the front; even though the 'V' is technically apart of a components name.

The File Structure
------------------

The iCalendar file format is very simple to understand, it is a name-params-value triplet that is written out like this:

    name;param1=paramValue1;param2='paramValue2':value[CRLF]

Hopefully that makes the patten fairly clear, though you should please note that the parameters are (or usually are) optional
in an iCalendar an you can accept the default parameter values by merely writing a name value pair like this:

    name:value[CRLF]

Also note that the CRLF is the '\r' character followed by the '\n' character and it is the way that dos systems write newlines.
