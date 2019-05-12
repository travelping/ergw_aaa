erGW-AAA - AAA component for the [erGW project][1]
==================================================
[![Build Status][travis badge]][travis]
[![Coverage Status][coveralls badge]][coveralls]
[![Erlang Versions][erlang version badge]][travis]

This is a companion project for the [erGW project][1] to provide an abstract
AAA (Authentication, Authorization and Accounting) interface for protocols
based on erGW.

Supported backend providers are:

* a local dummy (mock)
* RADIUS

Work on progress:

* DIAMETER
* flexible configuration
* the dropped attribute_map feature *might* be readded

In the future possible other providers are:

* LDAP (Authentication and Authorization only)

BUILDING
--------

Using rebar3:

    # rebar3 compile

CONFIGURATION
-------------

For all releases in the 3.x stream, the configuration syntax might change at
any point and might not be backward compatible.

Example of possible config.

```erlang
 {ergw_aaa,
  [{functions,
    [{'ergw-pgw-epc',
      [{handler, ergw_aaa_diameter},
       {'Origin-Host', <<"ergw-pgw.dia.example.net">>},
       {'Origin-Realm', <<"dia.example.net">>},
       {transports, [
           [{connect_to, <<"aaa://srv1.dia.example.net;transport=sctp">>},
            {recbuf, 32768}]
        ]},
      ]}
    ]},
   {handlers,
    [{ergw_aaa_static,
        [{'NAS-Identifier',          <<"NAS-Identifier">>},
         {'Acct-Interim-Interval',   600},
         {'Framed-Protocol',         'PPP'},
         {'Service-Type',            'Framed-User'},
         {'Node-Id',                 <<"PGW-001">>},
         {'Charging-Rule-Base-Name', <<"m2m0001">>},
         {rules, #{'Default' =>
                       #{'Rating-Group' => [3000],
                         'Flow-Information' =>
                             [#{'Flow-Description' => [<<"permit out ip from any to assigned">>],
                                'Flow-Direction'   => [1]    %% DownLink
                               },
                              #{'Flow-Description' => [<<"permit out ip from any to assigned">>],
                                'Flow-Direction'   => [2]    %% UpLink
                               }],
                         'Metering-Method'  => [1],
                         'Precedence' => [100]
                        }
                  }
         }
        ]},
     {ergw_aaa_radius, [{server, {{127,0,0,1}, 1812, <<"secret">>}}]},
     {ergw_aaa_rf, [{transport, 'ergw-pgw-epc'}]},
     {ergw_aaa_ro, [{transport, 'ergw-pgw-epc'}]}
    ]},

   {services,
    [{'Default',     [{handler, 'ergw_aaa_static'}]},
     {'RADIUS-Auth', [{handler, 'ergw_aaa_radius'},
	                  {server, {{127,1,0,1}, 1812, <<"secret">>}}]},
     {'RADIUS-Acct', [{handler, 'ergw_aaa_radius'}]},
	                  {server, {{127,2,0,1}, 1813, <<"secret">>}}]},
     {'Rf',          [{handler, 'ergw_aaa_rf'}]},
     {'Gx',          [{handler, 'ergw_aaa_gx'}]}
     {'Gy',          [{handler, 'ergw_aaa_ro'}]}
    ]},

   {apps,
    [{default,
      [{session, ['Default']},
       {procedures, [{authenticate, ['RADIUS-Auth']},
                     {authorize,    ['RADIUS-Auth']},
                     {start,     ['RADIUS-Acct', 'Rf']},
                     {interim,   ['RADIUS-Acct', 'Rf']},
                     {stop,      ['RADIUS-Acct', 'Rf']},
                     {{gx, 'CCR-Initial'},   ['Gx']},
                     {{gx, 'CCR-Update'},    ['Gx']},
                     {{gx, 'CCR-Terminate'}, ['Gx']},
                     {{gy, 'CCR-Initial'},   ['Gy']},
                     {{gy, 'CCR-Update'},    ['Gy']},
                     {{gy, 'CCR-Terminate'}, ['Gy']}]}
      ]}
    ]}
  ]},
```

[1]: https://github.com/travelping/ergw

<!-- Badges -->
[travis]: https://travis-ci.com/travelping/ergw_aaa
[travis badge]: https://img.shields.io/travis/com/travelping/ergw_aaa/master.svg?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/ergw_aaa
[coveralls badge]: https://img.shields.io/coveralls/travelping/ergw_aaa/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R20.3%20to%2021.2-blue.svg?style=flat-square
