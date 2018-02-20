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

PER-APPLICATIONS CONFIG
-----------------------

erGW-AAA support per-application config.

Example of possible config.

```erlang
 {ergw_aaa, [
    {applications, [
        {default,
            {provider, ergw_aaa_radius, [
                {nas_identifier, <<"nas_id1">>},
                {radius_auth_server, {{127,0,0,1}, 1812, <<"secret">>}},
                {radius_acct_server, {{127,0,0,1}, 1813, <<"secret">>}},
                {acct_interim_interval, 300},  %% Interim Accounting in seconds. By default it is 10 minutes.
                {service_type, 'Framed-User'},
                {framed_protocol, 'PPP'}
            ]}
        },
        {application1,
            {provider, ergw_aaa_radius, [
                {nas_identifier, <<"nas_id3">>},
                {radius_auth_server, {{127,0,0,1}, 1812, <<"radius_password">>}},
                {radius_acct_server, {{127,0,0,1}, 1813, <<"radius_password">>}}
            ]},
            {attribute_map, [
                {'Attr-A', ['Fixed-Value', '/', 'SomeElse']},
                {'Attr-B', ['MAC']},
                {'Attr-C', disabled}  %% do not transmit Attr-C
            ]}
        },
        {application,
            {provider, ergw_aaa_mock,
                [{shared_secret, <<"MySecret">>}]
            },
            {attribute_map, [
                %% send Called-Station-Id as "12-34-56-78-9A-BC (MyWifi)"
                {'Called-Station-Id', ['BSSID', <<" (">>, 'SSID', <<")">>]},
                {'Calling-Station-Id', ['MAC']},
                {'TP-Location-Id', ['ZONE-X']}
            ]}
        }
    ]}
 ]},

```

Set 'AAA-Application-Id' key for select application config.
Default ApplicationId is 'default'.

FLEXIBLE AAA ATTRIBUTES
-----------------------

erGW-AAA have ability for dynamic attributes. You can set 'attribute\_map' for
specific application. For example:

```erlang
{attribute_map, [
    {'Attr-A', ['Fixed-Value', '/', 'SomeElse']},
    {'Attr-B', ['MAC']},
    {'Attr-C', disabled}  %% do not transmit Attr-C
]}
```

Variables in mapping rule will be replaced with value from 'ergw\_aaa\_session'
state.  If some variable in the mapping rule is not found, it will be replaced
with the name of this variable.

OPTION FOR DISABLE RADIUS AUTHENTICATION OR/AND ACCOUNTING
----------------------------------------------------------

Use option 'disabled' in Radius provider config for disabling necessary actions.

```erlang
{ergw_aaa, [
    {applications, [
        {default,
            {provider, ergw_aaa_radius, [
                {nas_identifier, <<"nas_id">>},
                {radius_auth_server, {{127,0,0,1}, 1812, <<"secret">>}},
                {radius_acct_server, {{127,0,0,1}, 1813, <<"secret">>}},
                {disabled, [acct, auth]}
            ]}
        }]
    }]
}

```

[1]: https://github.com/travelping/ergw

<!-- Badges -->
[travis]: https://travis-ci.org/travelping/ergw_aaa
[travis badge]: https://img.shields.io/travis/travelping/ergw_aaa/master.svg?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/ergw_aaa
[coveralls badge]: https://img.shields.io/coveralls/travelping/ergw_aaa/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R19.1%20to%2020.0-blue.svg?style=flat-square
