-module(ergw_aaa_prometheus).
-export([declare/0]).

declare() ->
    prometheus_counter:declare([
        {name, ocs_hold_session_start},
        {labels, []},
        {help, "Total number of OCS Hold sessions started"}]),
    prometheus_counter:declare([
        {name, ocs_hold_session_end},
        {labels, []},
        {help, "Total number of OCS Hold sessions ended"}]).

