%% Copyright 2017-2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_test_lib).

-define(ERGW_AAA_NO_IMPORTS, true).

-export([meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1]).
-export([set_cfg_value/3, get_cfg_value/2]).
-export([get_stats/1, diff_stats/2, wait_for_diameter/2]).
-export([get_session_stats/0, reset_session_stats/0]).
-export([outstanding_reqs/0]).

-include("ergw_aaa_test_lib.hrl").
-include("../src/ergw_aaa_internal.hrl").

%%%===================================================================
%%% Meck support functions
%%%===================================================================

meck_init(Config) ->
    ok = meck:new(ergw_aaa_diameter, [passthrough, no_link]),

    {_, Hut} = lists:keyfind(handler_under_test, 1, Config),   %% let it crash if HUT is undefined
    ok = meck:new(Hut, [passthrough, no_link]).

meck_reset(Config) ->
    meck:reset(ergw_aaa_diameter),
    meck:reset(proplists:get_value(handler_under_test, Config)).

meck_unload(Config) ->
    meck:unload(ergw_aaa_diameter),
    meck:unload(proplists:get_value(handler_under_test, Config)).

meck_validate(Config) ->
    ?equal(true, meck:validate(ergw_aaa_diameter)),
    ?equal(true, meck:validate(proplists:get_value(handler_under_test, Config))).

%%%===================================================================
%%% Config manipulation
%%%===================================================================

set_cfg_value(Key, Value) when is_function(Value) ->
    Value(Key);
set_cfg_value(Key, Value) ->
    {Key, Value}.

set_cfg_value([Key], Value, Config) when is_boolean(Value) ->
    lists:keystore(Key, 1, proplists:delete(Key, Config), set_cfg_value(Key, Value));
set_cfg_value([{Key, Pos}], Value, Config) ->
    Tuple = lists:keyfind(Key, 1, Config),
    lists:keystore(Key, 1, Config, setelement(Pos, Tuple, set_cfg_value(Key, Value)));
set_cfg_value([Key], Value, Config) ->
    lists:keystore(Key, 1, Config, set_cfg_value(Key, Value));
set_cfg_value([{Key, Pos} | T], Value, Config) ->
    Tuple = lists:keyfind(Key, 1, Config),
    lists:keystore(Key, 1, Config,
		   setelement(Pos, Tuple, set_cfg_value(T, Value, element(Pos, Tuple))));
set_cfg_value([Pos | T], Value, Config)
  when is_integer(Pos), is_tuple(Config) ->
    setelement(Pos, Config, set_cfg_value(T, Value, element(Pos, Config)));
set_cfg_value([Pos | T], Value, Config)
  when is_integer(Pos), is_list(Config) ->
    Arr0 = array:from_list(Config),
    Arr1 = array:set(Pos - 1, set_cfg_value(T, Value, array:get(Pos - 1, Arr0)), Arr0),
    array:to_list(Arr1);
set_cfg_value([H | T], Value, Config) ->
    Prop = proplists:get_value(H, Config, []),
    lists:keystore(H, 1, Config, {H, set_cfg_value(T, Value, Prop)}).

%% add_cfg_value([Key], Value, Config) ->
%%     ct:pal("Cfg: ~p", [[{Key, Value} | Config]]),
%%     [{Key, Value} | Config];
%% add_cfg_value([H | T], Value, Config) ->
%%     Prop = proplists:get_value(H, Config, []),
%%     lists:keystore(H, 1, Config, {H, add_cfg_value(T, Value, Prop)}).

get_opt(Key, List) when is_list(List) ->
    proplists:get_value(Key, List);
get_opt(Key, Map) when is_map(Map) ->
    maps:get(Key, Map).

get_cfg_value([Key], Config) ->
    get_opt(Key, Config);
get_cfg_value([H|T], Config) ->
    get_cfg_value(T, get_opt(H, Config)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

wait_for_diameter(_SvcName, 0) ->
    "DIAMETER connection failed";
wait_for_diameter(SvcName, Cnt) ->
    [{transport, T}] = diameter:service_info(SvcName, [transport]),
    State =
	lists:foldl(
	  fun(X, {Cs, Okay} = Acc) ->
		  case {proplists:get_value(type, X),
			proplists:get_value(watchdog, X, undefined)} of
		      {connect, {_, _, okay}} -> {Cs + 1, Okay + 1};
		      {connect, _}            -> {Cs + 1, Okay};
		      _                       -> Acc
		  end
	  end, {0, 0}, T),
    case State of
	{Cs, Cs} ->
	    ok;
	_ ->
	    ct:sleep(100),
	    wait_for_diameter(SvcName, Cnt - 1)
    end.

%% pretty_print(Record) ->
%%     io_lib_pretty:print(Record, fun pretty_print/2).

%% pretty_print(diameter_gx_CCA, N) ->
%%     N = record_info(size, diameter_gx_CCA) - 1,
%%     record_info(fields, diameter_gx_CCA);
%% pretty_print(_, _) ->
%%     no.

get_stats(SvcName) ->
    Stats =
	[proplists:get_value(statistics, Transport)
	 || Transport <- diameter:service_info(SvcName, transport)],
    sum_stats(Stats, []).

sum_stats([], Summary) ->
	Summary;
sum_stats([TransportStat | Rest], Summary) ->
    NewSummary =
	lists:foldl(
	  fun({Key, Value}, Acc) ->
		  case lists:keytake(Key, 1, Acc) of
		      {value, {Key, AccVal}, Acc1} ->
			  [{Key, AccVal + Value} | Acc1];
		      _ ->
			  [{Key, Value} | Acc]
		  end
	  end,
	  TransportStat, Summary),
    sum_stats(Rest, NewSummary).

diff_stats(S1, S2) ->
    {Stats, Rest} =
	lists:mapfoldl(
	  fun({Key, Value} = S, StatsIn) ->
		  case lists:keytake(Key, 1, StatsIn) of
		      {value, {_, NewValue}, StatsOut} ->
			  {{Key, NewValue - Value}, StatsOut};
		      _ ->
			  {S, StatsIn}
		  end
	  end, S2, S1),
    Stats ++ Rest.

outstanding_reqs() ->
    lists:foldl(
      fun({_, O, _, _, _, _, _, _}, S) -> S + O end, 0, ergw_aaa_diameter_srv:peers()).


get_session_stats() ->
    lists:sort([{Handler, State, Value} ||
     {[{"handler", Handler}, {"state", State}], Value} <-
     prometheus_gauge:values(default, aaa_sessions_total)]).

reset_session_stats() ->
    %% there seems to be no easier way doing this ...
    [prometheus_gauge:remove(default, aaa_sessions_total, [Handler, State]) ||
     {[{"handler", Handler}, {"state", State}], _} <-
     prometheus_gauge:values(default, aaa_sessions_total)].
