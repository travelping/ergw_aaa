erGW-AAA - AAA component for the [erGW project][1]
==================================================
[![Build Status](https://travis-ci.org/travelping/ergw_aaa.svg?branch=master)](https://travis-ci.org/travelping/ergw_aaa)

This is a companion project for the [erGW project][1] to provide an abstract AAA (Authentication, Authorisation and Accounting) interface for protocols based on erGW.

Supported backend providers are:

* a local dummy (mock)
* RADIUS

In the future possible other providers are:

* DIAMETER
* LDAP (Authentication and Authorisation only)

BUILDING
--------

Using tetrapak:

    # tetrapak build check

Using rebar:

    # rebar get-deps
    # rebar compile


[1]: https://github.com/travelping/ergw