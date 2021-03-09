ergw-aaa
========

Erlang AAA session implementation for ERGW.

Version 3.6.10 - 16 March 2021
----------------------------

**Features** :rocket:
* [#119](https://github.com/travelping/ergw_aaa/pull/119) Implement `DIAMETER` rate limiter metric collector in `erGW-AAA`

**Dependencies** :gear:
* [#124](https://github.com/travelping/ergw_aaa/pull/124) Start use [cut](https://github.com/truqu/cut) with tag [1.0.3](https://github.com/truqu/cut/releases/tag/1.0.3)
* [#125](https://github.com/travelping/ergw_aaa/pull/125) Update [regine](https://github.com/travelping/regine) tag to [1.0.0](https://github.com/travelping/regine/releases/tag/1.0.0)
* [#126](https://github.com/travelping/ergw_aaa/pull/126) Update [setup](https://github.com/uwiger/setup) tag to [2.1.0](https://github.com/uwiger/setup/releases/tag/2.1.0)

Version 3.6.9 - 4 February 2021
----------------------------

**Features** :rocket:
* [#116](https://github.com/travelping/ergw_aaa/pull/116) Adding the `DIAMETER` `RFC` base dictionary to all services

**Dependencies** :gear:
* [#117](https://github.com/travelping/ergw_aaa/pull/117) Update [eradius](https://github.com/travelping/eradius) tag to [2.2.1](https://github.com/travelping/eradius/releases/tag/2.2.1)

Version 3.6.8 - 29 January 2021
----------------------------

**Features** :rocket:
* [#114](https://github.com/travelping/ergw_aaa/pull/114) Read session configuration everytime we run a action

Version 3.6.7 - 20 January 2021
----------------------------

* Fix `Acct-Interim-Interval` handling - [PR #109](https://github.com/travelping/ergw_aaa/pull/109)
* Remove dotfiles - [PR #111](https://github.com/travelping/ergw_aaa/pull/111)

Version 3.6.6 - 14 January 2021
----------------------------

* Fix passing `RADIUS` response IEs into session - [PR #106](https://github.com/travelping/ergw_aaa/pull/106)
* Trace pending `DIAMETER` request by `PID` - [PR #107](https://github.com/travelping/ergw_aaa/pull/107)

Version 3.6.5 - 31 December 2020
----------------------------

* Change the encoding of the `Location` in Ituma `Called-Station-Id` - [PR #100](https://github.com/travelping/ergw_aaa/pull/100)
* Remove non exist eradius env what was used for `exameter` - [PR #101](https://github.com/travelping/ergw_aaa/pull/101)
* Fix handling of ignored `RADIUS` AVPs - [PR #102](https://github.com/travelping/ergw_aaa/pull/102)
* Update [eradius](https://github.com/travelping/eradius) tag to [2.2.0](https://github.com/travelping/eradius/releases/tag/2.2.0) - [PR #104](https://github.com/travelping/ergw_aaa/pull/104)

Version 3.6.4 - 10 December 2020
----------------------------

* Termination Cause Mapping: Change asr to 'ASR' - [PR #99](https://github.com/travelping/ergw_aaa/pull/99)
* Change Ituma Called-Station-Id to 'Location;SSID' - [PR #98](https://github.com/travelping/ergw_aaa/pull/98)
* Increase max diameter peer rate limit - [PR #96](https://github.com/travelping/ergw_aaa/pull/96)
* Termination Cause Mapping: change `?MODULE` to interface names for termination cause errors - [PR #95](https://github.com/travelping/ergw_aaa/pull/95)
* Add [CODEOWNERS](https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/about-code-owners) - [PR #94](https://github.com/travelping/ergw_aaa/pull/94)

Version 3.6.3 - 4 December 2020
----------------------------

* Added retry for Ro/Gy CCR if CCA RC is temporary error
* Improve session termination reason handling: updated/added termination cause names

Version 3.6.2 - 13 November 2020
----------------------------

* Change QoS-Class-Identifier type from Enumerated to Unsigned32 [issue #71](https://github.com/travelping/ergw_aaa/issues/71)
* Added log with level `debug` for log errors of `diameter:call/4`
* Update [eradius](https://github.com/travelping/eradius) tag to [2.1.0](https://github.com/travelping/eradius/releases/tag/2.1.0)

Version 3.6.1 - 28 October 2020
----------------------------

* Fix AAA session state metrics for async invoke
* Fix vendor dicts validation
* Update [eradius](https://github.com/travelping/eradius) tag to [2.0.1](https://github.com/travelping/eradius/releases/tag/2.0.1)

Version 3.6.0 - 26 October 2020
----------------------------

* Add TLS-Pre-Shared-Key to special handing in session
* Add ergw_aaa_session:get/3 method
* Allow static handler to invoke to_session on defined handler
* Add RADIUS AVP filter and vendor dictionary support
* Add Ituma vendor RADIUS dictionary support
* Include RADIUS State AVP only in auth requests
* Termination cause mapping
* Pass `Framed-Interface-Id` to Radius
* Update [eradius](https://github.com/travelping/eradius) tag to [2.0.0](https://github.com/travelping/eradius/releases/tag/2.0.0)

Version 3.5.0 - 29 July 2020
----------------------------

* add AAA handler session state stats
* add diameter avp filter
* Use of the same accounting trigger for nasreq accounting as
  radius to fix the missing volume report AVPs

Version 3.4.0 - 11 May 2020
---------------------------

* switch from eradius modernize branch to released 1.0.0
* fix accounting of outstanding requests for load calculation
* add async mode to RADIUS accounting msgs
* add Traffic-Data-Volumes to Rf
* handle NASREQ STR sending errors
* implement RAN-Secondary-RAT-Usage-Report on Rf
* disable OTP Diameter built-in re-transmit handling, replace with
  load based control logic
* add missing IPv6 attributes in NASREQ and RADOIS
* fix use of repeated attributes in RADIUS handler
* strip X_ prefix from 3GPP attributes
* fix 3GPP-IPv6-DNS-Servers atom
* replace lager with Erlang logger

Version 3.3.0 - 26 Nov 2019
---------------------------

* added NASREQ Authentication
* support for couple and split accounting model
* rewrite handler state storage

Version 3.2.0 - 05 Nov 2019
---------------------------

* reworked rate limit and overload control mechanism
* OTP 22.1  support
* support ASR on (almost) all DIAMETER interfaces
* Gy RAR support
* updated erGW interface
* OCS hold support for Gy

Version 3.1.0 - 13 Jun 2019
---------------------------

* support for DIAMETER Gx, Rf and Ro protocol
* OTP 21.3 support
* rate limit for Gy and Ro
* enhanced config for DIAMETER transports
* updated erGW interface

Version 3.0.0 - 01 Aug 2018
---------------------------

* Rework DIAMETER support
* Rework configuration system
* Experimental support for DIAMETER Gx, Rf and Ro protocol
* Support Erlang OTP 20.1 through 21.0
* Drop support for OTP 19.x and 20.0
* Removed tetrapak support
* Removed flexible attribute support

Version 2.2.1 - 14 Mar 2018
---------------------------

* Upgrade used RADIUS library eradius to avoid memory impact when providing
  metrics

Version 2.2.0 - 26 Feb 2018
---------------------------

* Expose `acct_interim_interval`, `service_type`, `framed_protocol` option to
  RADIUS configuration (per AAA-Applications, all optional)
* Fix applying configured attribute map
* Fix fallback back to the default AAA-Application configuration

Version 2.1.0 - 02 Feb 2018
---------------------------

* Diameter support
* Provide the concept of different AAA-Applications
* Allow to disable Accounting/Authorization per RADIUS application
* Add mapping feature to build dynamic AAA attributes
* Added config validation
* Update dependencies
* Switch to rebar3 for builds

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
