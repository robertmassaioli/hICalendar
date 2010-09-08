hICalendar
==========

Author: Robert Massaioli (2010)

About
=====

This is a project written in Haskell for easy access to ICalendar data. Haskell was chosen as the language
because of its ability to reduce bugs, enable good testing and produce reliable software.
The end goal of this project is to be a one stop shop for people looking to deal with ICalendar data.

Dependancies
============

All of the Haskell package dependancies can be found in the .cabal file. The other dependancies are listed
here (complete with install command on the latest stable release of Ubuntu):

 - Pandoc for markdown generation.

Five Minute Startup
===================

Since there is no code there is no startup guide, wait for it, it will come and it will be awesome.

Reading
=======

Much of this project is already well documented in the rfc's that it is based on. I would reccomend getting
[qrfcview][1] and using it to view the following rfc's:

__Important__

 - rfc5545

__Not So Important__

 - rfc2045
 - rfc3986
 - rfc4288
 - rfc4648
 - rfc5646

Extensions
==========

There is basically only one way to make extensions to the format and that is wherever the spec has an extra
x-prop or iana-prop setting. The parser meerly needs to read this in as a string because it can do nothing 
clever with it. It is completely upto the extension writer to do something clever with that extra string.

 [1]: qrfcview.berlios.de
