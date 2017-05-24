erGW-AAA - AAA component for the [erGW project][1]
==================================================
[![Build Status][travis badge]][travis]
[![Coverage Status][coveralls badge]][coveralls]
[![Erlang Versions][erlang version badge]][travis]

This is a companion project for the [erGW project][1] to provide an abstract AAA (Authentication, Authorization and Accounting) interface for protocols based on erGW.

Supported backend providers are:

* a local dummy (mock)
* RADIUS

In the future possible other providers are:

* DIAMETER
* LDAP (Authentication and Authorization only)

BUILDING
--------

Using tetrapak:

    # tetrapak build check

Using rebar3:

    # rebar3 compile


[1]: https://github.com/travelping/ergw

<!-- Badges -->
[travis]: https://travis-ci.org/travelping/ergw_aaa
[travis badge]: https://img.shields.io/travis/travelping/ergw_aaa/master.svg?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/ergw_aaa
[coveralls badge]: https://img.shields.io/coveralls/travelping/ergw_aaa/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R19.1%20to%2019.3-blue.svg?style=flat-square
