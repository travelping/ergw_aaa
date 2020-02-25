AAA diameter session metrics
================================

ergw_aaa exposes prometheus gauge metric for the states of the different diameter handlers.
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
