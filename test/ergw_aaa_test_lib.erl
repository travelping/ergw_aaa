%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_test_lib).

-export([meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1]).
-export([get_stats/1, diff_stats/2, wait_for_diameter/2]).

-include("ergw_aaa_test_lib.hrl").

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
%%% Helper functions
%%%===================================================================

wait_for_diameter(_SvcName, 0) ->
    "DIAMETER connection failed";
wait_for_diameter(SvcName, Cnt) ->
    case diameter:service_info(SvcName, connections) of
	[] ->
	    ct:sleep(100),
	    wait_for_diameter(SvcName, Cnt - 1);
	_ ->
	    ok
    end.

%% pretty_print(Record) ->
%%     io_lib_pretty:print(Record, fun pretty_print/2).

%% pretty_print(diameter_gx_CCA, N) ->
%%     N = record_info(size, diameter_gx_CCA) - 1,
%%     record_info(fields, diameter_gx_CCA);
%% pretty_print(_, _) ->
%%     no.

get_stats(SvcName) ->
        sum_stats([proplists:get_value(statistics, Transport) 
				   || Transport <- diameter:service_info(SvcName, transport)],
				   []).

sum_stats([], Summary) ->
	Summary;
sum_stats([TransportStat | Rest], Summary) ->
	NewSummary = lists:foldl(
		fun({Key, Value}, Acc) ->
			case lists:keytake(Key, 1, Acc) of
				{value, {Key, AccVal}, Acc1} ->
					[{Key, AccVal + Value} | Acc1];
				_ ->
					[{Key, Value} | Acc]
			end
		end,
		TransportStat,
		Summary
	),
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
