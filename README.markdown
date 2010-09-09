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

You cannot do much yet for the moment other than compile the code and that is fairly simple. Perform the 
following instructions (assuming that you have cabal-install):

    cabal configure
    cabal build

And that is all that there is to it. I would not reccomend doing a cabal install just yet.

Reading
=======

Much of this project is already well documented in the rfc's that it is based on. I would reccomend getting
[qrfcview][1] and using it to view the following rfc's:

**Important**

 - rfc5545

**Not So Important**

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

 [1]: http://qrfcview.berlios.de

