ctld
====

Erlang CTLD Radius session implementation.

Version 1.3.0 - xx Feb 2015
---------------------------

* description abstract session FSM and implement framework
* moved ctld_session to new FSM
* add more RADIUS attributes
* strip TP- prefix from session internal state
* fix case of CAPWAP Power attributes in Travelping dictionary
* generalize AAA provider invokation
* Session-Ids for AAA providers
* Interim-Accounting is now asynchron
* adjust API for eradius 0.6.0

Version 1.2.1 - 06 Nov 2014
---------------------------

* Changed ctld_station_session to take WTP-Id and Session-Id arguments

Version 1.1.0 - 20 Jun 2014
---------------------------

* Added Support for sending GPS Attributes to the PCS.

Version 1.0.1 - 19 Sep 2013
---------------------------

* Support for triggered interim and batched interim accounting updates
* Support for Travelping CAPWAP Attributes in Accounting

Version 1.0.0 - 17 Sep 2013
---------------------------

* Support for radius station and WTP sessions
