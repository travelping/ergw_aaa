CCR retry
=========
CCR retry is currently implemented in the ergw_aaa_ro module, and can be applied per procedure.

Behaviour
---------
CCR retry is configured for a procedure (e.g. CCR-I) with *max_retries* configuration parameter set to an integer value greater than 0. Also an optional *tx_timeout* (in milliseconds) can be configured (defaults to 5000 ms if not set). 

When there is no answer within *tx_timeout* for a request, the request will be re-sent to another connected peer. This will be repeated until *max_retries* number of attempts is reached or all the peers have been tried. The request fails with **timeout** error when *max_retries* is reached. If the number of retries is configured larger than the number of available peers, then when there are not more peers can be tried, the request fails with **no_connection** error.

The retansmitted request messages are sent to a not yet tried peer and keep the end to end ID of the original request and will have the retansmit flag set, indicating to the peer, that the request is possibly a retransmit.

Example configuration
---------------------

Configure the Gy CCR-I procedure in the **ergw_aaa** apps section with a with tx_timeout of 1 sec and max_retries of 2 :
```
...
{apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, []},
			   {authorize, []},
			   {start, []},
			   {interim, []},
			   {stop, []},
			   {{gy, 'CCR-Initial'},   [{'Ro', [{tx_timeout, 1000}, {max_retries, 2}]}]},
			   {{gy, 'CCR-Update'},    ['Ro']},
			   {{gy, 'CCR-Terminate'}, ['Ro']}
			  ]}
	    ]}
	  ]}
...
```

Rate limiting
=============
Rate limiting is implemented in the ergw_aaa_ro module and can be applied per procedure. 

Behaviour
---------
Rate limiting is depending on the jobs application. It requires a jobs queue configured and available at the time of the processing a request for which there is retry configured. This is normally done by configuring the jobs application in the release environment. The name of the configured queue is used in the *rate_limit_queue* configuration of the procedure. The the jobs queue should be configured with a rate regulator with the desired maximum rate, and a max time which limits how long a request can wait for a "slot" to be sent. If the max time is exceeded, the request fails with **rate_limit** error. 

Example configuration
---------------------
Configure the Gy CCR-T procedure in the **ergw_aaa** apps section with a rate limit queue

```
...
{apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, []},
			   {authorize, []},
			   {start, []},
			   {interim, []},
			   {stop, []},
			   {{gy, 'CCR-Initial'},   ['Ro']},
			   {{gy, 'CCR-Update'},    ['Ro']},
			   {{gy, 'CCR-Terminate'}, [{'Ro', [{rate_limit_queue, ccr_t_rate_limit}]}]}
			  ]}
	    ]}
	  ]}
...
```

And configure a corresponding **jobs** queue in the jobs application environment. e.g. to limit the rate of CCR-T messages to 100 req/sec and have requests wait for a maximum of 5 sec in the rate limiting queue.

```
{queues, [
	...
	{ccr_t_rate_limit, [{max_time, 5000}, {standard_rate, 100}]}
	]}]
```

Interaction between rate limiting and retry
===========================================
It is possible to configure both rate limiting and retry for the same procedure. In this case rate limiting will be checked first and if rate limiting passed, the retry might happen (which will not be part of rate limiting).

