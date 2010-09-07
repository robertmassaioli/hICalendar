Naming Conventions
==================

We will abide by and follow the naming conventions that appear in the rfc5545 above everything else
and if we do not then we have to have an absolutely excellent reason. All naming must be consistent
accross the entire project and any ambiguities must be erased as quickly as possible. We will use the
github bugtracker for precisely this purpose.

High Level Terms
----------------

Component - Every ICalendar is made up of components, they are Events, Todos, Journals, Free/Busy Times and Alarms. Some
            components are more important than others, namely, the first three are more important than the last two.

Low Level Terms
---------------

Content Line - Every line in a .ics file is known as a content line, it is a name, params, value triplet that ends in a CRLF.
               It is the highest level parsing concept in an ICalendar file.
