What Do We Want?
================

 - Before we actually get into the thick of the problem we have to completely generate the data structures
   that we are going to use and then reason about what we are then going to require in our programs. This 
   will likely be one of the trickiest parts of the planning process.
 - We want to get out a bunch of easy to read data that we can just use in our program.
 - It should be as simple an readICalendar :: ByteString -> ICalendar
 - Well want to iterate over the events in a nice and easy way so that we could print them out to the screen.
 - We want to just add events back to the calender whenever we please.
 - We want lazy and strict reading versions. (Maybe)
 - People will want to read and write these ICalendar files round about equally.
 - English Text box input would be nice as a feature.

Extension Framework
-------------------

 - The ICalendar spec allows for extensions so we should too. Read up on good ways to implement extension 
   frameworks.
 - Write an extension framework that is easy to use and helps everyone. It should make it simple for people to 
   add whatever features that they like to the ICalendar.

Extra Resources
---------------

 - We have to figure out how to pass around all of the useful resources (files and urls) that are supposed 
   to be attached to the calendar.
    - perhaps we should be able to export an entire calendar as a zip file or tar ball so that all the 
      resources are included
    - even urls should be downloaded and placed inside of the tar ball, common urls should only be downloaded
      once obviously.
 - If there is a localhost url that is attched that points to localhost then a warning should come up.
 - You should be able to run a resource check at any time so that you know wether or not you have all of the
   resources that you need to load the ICalendar perfectly. This would require all sorts of resource checking
   because you would have to make sure that URL's are correct and that everything is in the expected place from
   the current location (or even any location that you want to test against).

Importing And Exporting
-----------------------

 - Think about the errors and corner conditions that you are likely to encounter. The file should be able
   to be loaded in best attempt style every time. A nice, easy on the eyes screen should come up every time 
   an error is encountered.

What Components are in the Spec?
================================

    Event         - An event that occurs on a date.
    ToDo          - A ToDo item that occurrs on a date; many different events and items may be added. 
    Journal       - The Journal item is a big text entry attached to a date.
    Free / Busy   - Used to request, reply or publish free / busy time information on any other calendar.
    Time Zone     - This is for specifying differing different time zones and Daylight Savings.
    Alarm         - This provides alarms for the other components where an alarm would make sense
                    Note: Alarms should only appear in Events or ToDo components.

Events and Free / Busy Times seem like they should be related. You should be able co de-imformatise
events into free busy times.

ToDo events are useful and there should be a powerful way to deal with them. People will just want
to jot down todo items. They are simple but they should be highly customisable, as in you should be
able to add as much data to ToDo items as you can.

What are the Steps that you should take to parse the data?
==========================================================

 - Start with a raw string from the file
 - Turn the String into a stream of tokens and error check for correct syntax.
 - Turn the tokens into a parse tree with syntax checking as you go. (tokens are not in the right order)
 - Turn the parse tree into an ICalendar which is made up of the components.
 - Provide a nice interface for querying this ICalendar object.
