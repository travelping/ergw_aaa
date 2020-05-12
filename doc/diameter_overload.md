DIAMETER request limits and retry
=================================

Support for limiting the outgoing request rate and outstanding requests, and the client
controlled failover is currently implemented in all DIAMETER client handlers, and can be
applied per interface and procedure.

Overload avoidance
------------------

To avoid overloading a DIAMETER peer, a rate limit can be applied on a per peer basis. Requests
exceeding the rate are dropped and possibly retired to a different peer. The rate limit uses a
token bucket algorithm. A rate of request per second is configured through the *rate*
configuration setting. The token refill interval is one token per 1000/*rate* milliseconds.
This achives a constant maximum request rate when the rate limit is reached.

A per peer limit for the outstanding (i.e. not yet answered requests) can be configure through
the *outstanding_requests* parameter.

When choosing a peer for a request among several candidates, the peer with the lowest relating
load based on left over outstanding request and request tokens is chosen.

The Erlang DIAMTER stack will only retry a request to a different peer if the connection to
initial peer is considered to be DOWN. The client implementation will also retry to alternate
peer if a request times out or is a request is dropped because the rate limit for a peer has
been reached.

The retry and timeout behavior is configured with the *max_retries* configuration parameter set
to an integer value greater than 0. Also an optional *tx_timeout* (in milliseconds) can be
configured (defaults to 5000 ms if not set).

When there is no answer within *tx_timeout* for a request, the request will be re-sent to
another connected peer. This will be repeated until *max_retries* number of attempts is reached
or all the peers have been tried. The request fails with **timeout** error when *max_retries*
is reached. If the number of retries is configured larger than the number of available peers,
then when there are not more peers can be tried, the request fails with **no_connection**
error.

The retansmitted request messages are sent to a not yet tried peer and keep the end to end ID
of the original request and will have the retansmit flag set, indicating to the peer, that the
request is possibly a retransmit.

Example configuration
---------------------

Configure the Gy CCR-I procedure in the **ergw_aaa** apps section with a with tx_timeout of one
sec and max_retries of 2, the peer **ocs.sample** is configured wit a rate limit of 50 req/s
and maximum number of outstanding requests of 50:

```
...
{ergw_aaa,
 [
  {rate_limits,
   [{default,      [{outstanding_requests, 10}, {rate, 10}]},
    {"ocs.sample", [{outstanding_requests, 50}, {rate, 50}]}]},
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
   ]}...
```
