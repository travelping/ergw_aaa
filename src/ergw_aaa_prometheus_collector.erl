%% Copyright 2021, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_prometheus_collector).
-behaviour(prometheus_collector).

-include_lib("prometheus/include/prometheus.hrl").
-include("ergw_aaa_internal.hrl").

-export([
    deregister_cleanup/1,
    collect_mf/2,
    collect_metrics/2,
    gather/0
]).

-ignore_xref([gather/0, collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5, gauge_metric/2]).

-define(METRIC_NAME_PREFIX, "ergw_aaa_diameter_").

-define(OUTSTANDING, outstanding_requests).
-define(TOKENS, available_tokens).

-define(METRICS, [{?OUTSTANDING, gauge, "The number of outstanding requests"},
                  {?TOKENS, gauge, "The number of available tokens"}]).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) ->
    ok.

collect_mf(_Registry, Callback) ->
    Stats = gather(),
    [mf(Callback, Metric, Stats) || Metric <- ?METRICS],
    ok.

mf(Callback, {Name, Type, Help}, Stats) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, ?MODULE,
                       {Type, fun(S) -> maps:get(Name, S, undefined) end, Stats})),
    ok.

collect_metrics(_, {Type, Fun, Stats}) ->
    case Fun(Stats) of
        M when is_map(M) ->
            [metric(Type, Labels, Value) || {Labels, Value} <- maps:to_list(M)];
        _ ->
            undefined
    end.

metric(gauge, Labels, Value) ->
    gauge_metric(Labels, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

gather() ->
    maps:fold(fun
        (Name, #peer{outstanding = Outstanding,
                     capacity = Capacity,
                     rate = Rate,
                     tokens = Tokens}, #{?OUTSTANDING := OutstandingMap, ?TOKENS := RateMap} = Stats) ->
            Stats#{
                ?OUTSTANDING := OutstandingMap#{[{name, Name}, {type, limit}] => Capacity, [{name, Name}, {type, used}] => Outstanding},
                ?TOKENS      := RateMap#{[{name, Name}, {type, limit}] => Rate, [{name, Name}, {type, used}] => Tokens}
            };
        (_, _, Stats) ->
            Stats
    end, #{?OUTSTANDING => #{}, ?TOKENS => #{}}, ergw_aaa_diameter_srv:get_peers_info()).
