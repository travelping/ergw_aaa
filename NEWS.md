ergw-aaa
========

Erlang AAA session implementation for ERGW.

Version 2.1.0 - xx May 2017
---------------------------

* added config validation
* added per provider initialization function
* switch to rebar3 for builds

Version 2.0.0 - 23 Sep 2016
---------------------------

* release as version 2.0 under GPL for ERGW

Version 1.3.1 - 25 Apr 2016
---------------------------

* fix bogus milli seconds calculation
* monitor session owner to properly shutdown when it does

Version 1.3.0 - 23 Apr 2015
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
