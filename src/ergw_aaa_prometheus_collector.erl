%% Copyright 2021, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_prometheus_collector).
-behaviour(prometheus_collector).

-include_lib("prometheus/include/prometheus.hrl").
-include("ergw_aaa_internal.hrl").

-export([deregister_cleanup/1,
	 collect_mf/2,
	 collect_metrics/2,
	 gather/0
	]).

-ignore_xref([gather/0, collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5, gauge_metric/2, counter_metric/2]).

-define(METRIC_NAME_PREFIX, "ergw_aaa_diameter_").

-define(OUTSTANDING, outstanding_requests).
-define(TOKENS, available_tokens).
-define(NO_TOKENS, no_tokens_available_total).
-define(NO_CAPACITY, no_capacity_left_total).

-define(METRICS, [{?OUTSTANDING, gauge, "The number of outstanding requests"},
		  {?TOKENS, gauge, "The number of available tokens"},
		  {?NO_TOKENS, counter,
		   "The number of times a request to a peer was abort because no tokens where left"},
		  {?NO_CAPACITY, counter,
		   "The number of times a request to a peer was abort because the maximum number of outstanding requests was reached"}]).

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
    gauge_metric(Labels, Value);
metric(counter, Labels, Value) ->
    counter_metric(Labels, Value).

%%%===================================================================
%%% Internal functions
%%%===================================================================

gather() ->
    Init =
	#{?OUTSTANDING => #{},
	  ?TOKENS => #{},
	  ?NO_TOKENS => #{},
	  ?NO_CAPACITY => #{}
	 },
    maps:fold(
      fun(Name, #peer{outstanding = Outstanding,
		      capacity = Capacity,
		      rate = Rate,
		      tokens = Tokens,
		      stats = #peer_stats{
				 no_tokens   = NoTokens,
				 no_capacity = NoCapacity}
		     },
	  #{?OUTSTANDING := OutstandingMap, ?TOKENS := RateMap,
	    ?NO_TOKENS := NoTokensStats, ?NO_CAPACITY := NoCapacityStats} = Stats) ->
	      NameLabel = [{name, Name}],
	      Limit = [{type, limit}|NameLabel],
	      Used = [{type, used}|NameLabel],
	      Stats#{
		     ?OUTSTANDING :=
			 OutstandingMap#{Limit => Capacity, Used => Outstanding},
		     ?TOKENS :=
			 RateMap#{Limit => Rate, Used => Tokens},
		     ?NO_TOKENS := NoTokensStats#{NameLabel => NoTokens},
		     ?NO_CAPACITY := NoCapacityStats#{NameLabel => NoCapacity}
		    };
	 (_, _, Stats) ->
	      Stats
      end, Init, ergw_aaa_diameter_srv:get_peers_info()).
