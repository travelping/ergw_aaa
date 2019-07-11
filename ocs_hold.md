OCS Hold
========

OCS Hold is a configurable feature to allow user sessions continue with predefined granted time/volume quota in case the OCS is not available or OCS requests fail due to temporary (e.g. transport) issues. It is implemented in the *ergw_aaa_ro* module, and can be used on interfaces with this module configured as handler. These are typically Gy and Ro.

Functionality
-------------

The feature can be activated using the answer_if_xxxx configurable error handling mechanism. So far this mechanism allowed to define Diameter response AVP's (in diameter application map format) to "fake" normal responses to the session management when errors (timeout, no_connection, rate_limiting) are returned from the invocation of the OCS requests.

This mechanism is now extended to accept, next to the already possible AVP map, a tuple in the format of :
`{ocs_hold, [MSCC,...]}` where MSCC is the content or an MSCC entry in diameter application AVP map format. This allows to specify any combination of granted time/volume quota, for multiple rating groups if necessary.

If this ocs_hold tuple format is encountered as a configured answer to an OCS request error, then the session is put into an ocs_hold state, which means that when the defined granted quota is exhausted, the user data session will be stopped, without any subsequent communication with the OCS.

As an additional feature, it is also possible to grant a variable `CC-Time` per request in each GSU. To achieve this, the expansion of the MSCC structures specifically allows `CC-Time-Min` and `CC-Time-Max` keys in the GSU map. When these are encountered, then the `CC-Time` AVP value is generated randomly between these 2 values at the time of the request. 
**Note** that these 2 keys are **NOT** standard Diameter attributes, but used to generate the `CC-Time` AVP on the fly in the context of this feature. Do not attemtp to use them as normal AVP outside of the ocs_hold structure. Of course a static `CC-Time` can be used as well, if this randomized time duration is not required.

Configuration
-------------

Example ergw_aaa application configuration with OCS hold for CCR-I and CCR-U for request timeout and OCS connection down.


```
...
{services, [
    {'Default', [{handler, ergw_aaa_static}]},
    {'Ro', [
        {handler, 'ergw_aaa_ro'},
        {answers, #{
            'OCS-Hold' => {ocs_hold, [
                #{
                    'Envelope-Reporting' => [0],
                    'Granted-Service-Unit' => [
                        #{
                            'CC-Time-Min' => [1800],
                            'CC-Time-Max' => [1900]
                        }
                    ],
                    'Rating-Group' => [3000],
                    'Result-Code' => [2001],
                    'Time-Quota-Threshold' => [60]
                }
            ]},
            'Terminate-If-Down' => #{'Result-Code' => 2001}
            }
        }
    ]}
]},
{apps, [
    {default, [
        {session, ['Default']},
        {procedures, [
            {authenticate, []}, 
            {authorize, []}, 
            {start, []},
            {interim, []}, 
            {stop, []},
            {{gy, 'CCR-Initial'},[
                {'Ro', [
                    {answer_if_timeout, 'OCS-Hold'}, 
                    {answer_if_down, 'OCS-Hold'}
                ]}
            ]},
            {{gy, 'CCR-Update'}, [
                {'Ro', [
                    {answer_if_timeout, 'OCS-Hold'}, 
                    {answer_if_down, 'OCS-Hold'}
                ]}
            ]},
            {{gy, 'CCR-Terminate'}, [
                {'Ro', [
                    {answer_if_timeout, 'Terminate-If-Down'},
                    {answer_if_down, 'Terminate-If-Down'}
                ]}
            ]}
        ]}
    ]}
]}
...
```

Backward compatibility
----------------------
The existing possibility to define AVP map as error answer, is kept for backward compatibility. 

In the current implementation when a request has failed with OCS connection down (no_connection), then the state of the session was put to peer_down, and subsequent requests are not sent to the OCS. If OCS Hold is NOT configured then this behaviour is kept.

The new {ocs_hold, [MSCC, ...]} answer configuration is **NOT compatible** with earlier ergw_aaa application versions. It should be adjusted accordingly when application rollback is considered.