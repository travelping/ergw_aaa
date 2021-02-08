# erGW-AAA
* [DIAMETER metrics](#diameter-metrics)
* [DIAMETER session metrics](#diameter-session-metrics)

`erGW-AAA` uses [prometheus.erl](https://github.com/deadtrickster/prometheus.erl) to implement various operation metrics.

# DIAMETER metrics

The following metrics exist:
| Name                                       | Type      | Labels         | Description                            |
|--------------------------------------------|-----------|----------------|----------------------------------------|
| aaa_sessions_total                         | gauge     | handler, state | AAA of active sessions                 |
| ergw_aaa_diameter_outstanding_requests     | gauge     | name, type     | The number of outstanding requests     |
| ergw_aaa_diameter_available_tokens         | gauge     | name, type     | The number of available tokens         |

# DIAMETER session metrics

`erGW-AAA` exposes prometheus gauge metric for the states of the different diameter handlers
 and the radius handler.
The metrics are labeled with the name of the handler module and the handler state. The name
 of the metric is `aaa_sessions_total`.

Most of the currently implemented handlers maintain a single active session state `started`,
 except the `ergw_aaa_ro` module which handles the `Gy` interface to `OCS` (Online Charging
 System) and can have an active state of `ocs_hold` as well (see [OCS Hold](ocs_hold.md)).


| metric             | label "handler" | label "state" | description                    |
|--------------------|-----------------|---------------|--------------------------------|
| aaa_sessions_total | ergw_aaa_ro     | started       | Nr. of active Gy sessions      |
| aaa_sessions_total | ergw_aaa_ro     | ocs_hold      | Nr. of OCS held Gy sessions    |
| aaa_sessions_total | ergw_aaa_gx     | started       | Nr. of active Gx sessions      |
| aaa_sessions_total | ergw_aaa_rf     | started       | Nr. of active Rf sessions      |
| aaa_sessions_total | ergw_aaa_nasreq | started       | Nr. of active NASREQ sessions  |
| aaa_sessions_total | ergw_aaa_radius | started       | Nr. of active Radius sessions  |
