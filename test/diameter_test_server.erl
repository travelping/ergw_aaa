%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_test_server).

-include_lib("diameter/include/diameter.hrl").
-include("../include/diameter_3gpp_ts29_061_sgi.hrl").

-export([start/0, stop/0]).
-export([get_stats/1, diff_stats/2, wait_for_diameter/2]).

%% diameter callbacks
-export([peer_up/4,
	 peer_down/4,
	 pick_peer/5,
	 prepare_request/4,
	 prepare_retransmit/4,
	 handle_answer/5,
	 handle_error/5,
	 handle_request/4]).

-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

-define(DIAMETER_DICT_NASREQ, diameter_3gpp_ts29_061_sgi).
-define(DIAMETER_APP_ID_NASREQ, ?DIAMETER_DICT_NASREQ:id()).

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

%%===================================================================
%% API
%%===================================================================

start() ->
    application:ensure_all_started(diameter),
    SvcState = #{},
    SvcOpts = [{'Origin-Host', "server.example.com"},
	       {'Origin-Realm', "example.com"},
	       {'Vendor-Id', ?VENDOR_ID_TP},
	       {'Product-Name', "Server"},
	       {'Supported-Vendor-Id', [?VENDOR_ID_3GPP,
					?VENDOR_ID_ETSI,
					?VENDOR_ID_TP]},
	       {'Auth-Application-Id', [?DIAMETER_APP_ID_NASREQ]},
	       {restrict_connections, false},
	       {string_decode, false},
	       {decode_format, map},
	       {application, [{alias, nasreq},
			      {dictionary, ?DIAMETER_DICT_NASREQ},
			      {module, [?MODULE, SvcState]}]}],
    ok = diameter:start_service(?MODULE, SvcOpts),

    Opts = [{transport_module, diameter_tcp},
	    {transport_config, [{reuseaddr, true},
				{ip, {127,0,0,1}},
				{port, 3868}]}],
    {ok, _} = diameter:add_transport(?MODULE, {listen, Opts}),
    ok.

stop() ->
    diameter:stop_service(?MODULE),
    application:stop(diameter).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State, _Extra) ->
    lager:debug("peer_up: ~p, ~p~n", [_Peer, _Extra]),
    State.

peer_down(_SvcName, _Peer, State, _Extra) ->
    State.

pick_peer(_, _, _SvcName, _State, _Extra) ->
    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_request(#diameter_packet{msg = ['ACR' | Msg]}, _SvcName, {_, Caps}, _Extra)
  when is_map(Msg) ->
    InterimAccounting = 1,
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,
    #{'Session-Id' := Id,
      'Accounting-Record-Type' := Type,
      'Accounting-Record-Number' := Number,
      'Acct-Application-Id' := AppId} = Msg,
    ACA =  #{'Session-Id' => Id,
	     'Result-Code' => 2001,
	     'Origin-Host' => OH,
	     'Origin-Realm' => OR,
	     'Acct-Interim-Interval' => [InterimAccounting],
	     'Accounting-Record-Type' => Type,
	     'Accounting-Record-Number' => Number,
	     'Acct-Application-Id' => AppId},
    case maps:get('3GPP-IMSI', Msg, []) of
	[] -> {reply, ['ACA' | ACA]};
	_IMSI -> check_3gpp(Msg, ACA)
    end;

handle_request(#diameter_packet{msg = _Msg}, _SvcName, _, _Extra) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

check_3gpp(#{'3GPP-IMSI'              := [<<"250071234567890">>],
	     '3GPP-Charging-Id'       := [<<214, 208, 226, 238>>],
	     '3GPP-PDP-Type'          := [0],
	     '3GPP-SGSN-Address'      := [<<192, 168, 1, 1>>],
	     '3GPP-IMSI-MCC-MNC'      := [<<"25999">>],
	     '3GPP-GGSN-MCC-MNC'      := [<<"25888">>],
	     '3GPP-SGSN-IPv6-Address' := [<<253,150,220,210,239,219,65,196,0,0,0,0,0,0,16,0>>],
	     '3GPP-GGSN-IPv6-Address' := [<<253,150,220,210,239,219,65,196,0,0,0,0,0,0,32,0>>],
	     '3GPP-SGSN-MCC-MNC'      := [<<"26201">>],
	     '3GPP-IMEISV'            := [<<82,21,50,96,32,80,30,0>>],
	     '3GPP-RAT-Type'          := [<<6>>],
	     '3GPP-NSAPI'             := [<<"5">>],
	     '3GPP-Selection-Mode'    := [<<"0">>]
	    }, ACA) ->
    {reply, ['ACA' | ACA]};
check_3gpp(_Msg, _ACA) ->
    {answer_message, 3001}.

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
    [Transport] = diameter:service_info(SvcName, transport),
    proplists:get_value(statistics, Transport).

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
