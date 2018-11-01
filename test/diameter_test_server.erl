%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_test_server).

-compile({parse_transform, do}).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_rfc4006_cc.hrl").
-include("../include/diameter_3gpp_ts29_061_sgi.hrl").
-include("../include/diameter_3gpp_ts29_212.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/diameter_3gpp_ts32_299_ro.hrl").

-export([start/0, stop/0, abort_session_request/4]).
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

-define(DIAMETER_DICT_GX, diameter_3gpp_ts29_212).
-define(DIAMETER_APP_ID_GX, ?DIAMETER_DICT_GX:id()).

-define(DIAMETER_DICT_RO, diameter_3gpp_ts32_299_ro).
-define(DIAMETER_APP_ID_RO, ?DIAMETER_DICT_RO:id()).

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

%%===================================================================
%% API
%%===================================================================

start() ->
    application:ensure_all_started(diameter),
    SvcOpts = [{'Origin-Host', "server.test-srv.example.com"},
	       {'Origin-Realm', "test-srv.example.com"},
	       {'Vendor-Id', ?VENDOR_ID_TP},
	       {'Product-Name', "Server"},
	       {'Supported-Vendor-Id', [?VENDOR_ID_3GPP,
					?VENDOR_ID_ETSI,
					?VENDOR_ID_TP]},
	       {'Auth-Application-Id', [?DIAMETER_APP_ID_NASREQ,
					?DIAMETER_APP_ID_GX,
					?DIAMETER_APP_ID_RO]},
	       {'Vendor-Specific-Application-Id',
		[#'diameter_base_Vendor-Specific-Application-Id'{
		    'Vendor-Id'           = ?VENDOR_ID_3GPP,
		    'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}]},
	       {restrict_connections, false},
	       {string_decode, false},
	       {decode_format, map},
	       {application, [{alias, nasreq},
			      {dictionary, ?DIAMETER_DICT_NASREQ},
			      {module, [?MODULE, nasreq]}]},
	       {application, [{alias, diameter_gx},
			      {dictionary, ?DIAMETER_DICT_GX},
			      {module, [?MODULE, gx]}]},
	       {application, [{alias, diameter_gy},
			      {dictionary, ?DIAMETER_DICT_RO},
			      {module, [?MODULE, gy]}]}],
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

abort_session_request(gy, SessionId, DH, DR) ->
    ASR = #{'Session-Id' => SessionId,
	    'Destination-Realm' => DR,
	    'Destination-Host' => DH,
	    'Auth-Application-Id' => ?DIAMETER_APP_ID_RO},
    diameter:call(?MODULE, diameter_gy, [ 'ASR' | ASR ], [detach]).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State, _Extra) ->
    lager:debug("peer_up: ~p, ~p~n", [_Peer, _Extra]),
    State.

peer_down(_SvcName, _Peer, State, _Extra) ->
    State.

pick_peer([Peer|_], _RemoteCandidates, _SvcName, _State, _Extra) ->
    {ok, Peer}.

prepare_request(#diameter_packet{msg = [T | Avps]} = Packet, _, {_PeerRef, Caps}, _Extra)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _},
		   origin_state_id = {OSid, _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Origin-State-Id' => OSid}],
    lager:debug("prepare_request Msg: ~p", [Msg]),
    {send, Packet#diameter_packet{msg = Msg}};

prepare_request(Packet, _SvcName, {PeerRef, _}, _Extra) ->
    lager:debug("prepare_request to ~p: ~p", [PeerRef, Packet]),
    {send, Packet}.

prepare_retransmit(_Packet, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer, _Extra) ->
    Msg.

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
    case check_3gpp(Msg) of
	Result when Result =:= ok;
		    Result =:= {ok, no_imsi} ->
	    {reply, ['ACA' | ACA]};
	_ ->
	    {answer_message, 5005}
    end;

handle_request(#diameter_packet{
		  msg = ['CCR' |
			 #{'Subscription-Id' :=
			       [#{'Subscription-Id-Data' := <<"FAIL">>}]}]},
	       _SvcName, _, _Extra) ->
    {answer_message, 3001};  %% DIAMETER_COMMAND_UNSUPPORTED

handle_request(#diameter_packet{msg = ['CCR' | Msg]}, _SvcName, {_, Caps}, gx) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,
    #{'Session-Id' := Id,
      'Auth-Application-Id' := AppId,
      'CC-Request-Type' := Type,
      'CC-Request-Number' := Number} = Msg,

    RuleNames = [<<"service01">>, <<"service02">>, <<"service03">>],
    Key = <<"default">>,

    Reply = #{
      'Session-Id' => Id,
      'Auth-Application-Id' => AppId,
      'Origin-Host' => OH,
      'Origin-Realm' => OR,
      'CC-Request-Type' => Type,
      'CC-Request-Number' => Number,
      'Result-Code' => [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
      'Charging-Rule-Install' =>
	  [#{'Charging-Rule-Name' => [Name]} || Name <- RuleNames],
      'Usage-Monitoring-Information' =>
	  [#{'Monitoring-Key' => [Key],
	     'Usage-Monitoring-Level' =>
		 [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL'],
	     'Granted-Service-Unit' =>
		 [#{'CC-Time' => [600]},
		  #{'CC-Total-Octets' => [1000],
		    'CC-Input-Octets' => [1000],
		    'CC-Output-Octets' => [1000]}
		 ]}]
     },
    {reply, ['CCA' | Reply]};

handle_request(#diameter_packet{
		  msg = ['CCR' |
			 #{'Service-Information' :=
			       [#{'PS-Information' := [PS]}]} = Msg]},
	       _SvcName, {_, Caps}, gy) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,
    #{'Session-Id' := Id,
      'Auth-Application-Id' := AppId,
      'CC-Request-Type' := Type,
      'CC-Request-Number' := Number} = Msg,

    CCA0 =
	#{'Session-Id' => Id,
	  'Auth-Application-Id' => AppId,
	  'Origin-Host' => OH,
	  'Origin-Realm' => OR,
	  'CC-Request-Type' => Type,
	  'CC-Request-Number' => Number,
	  'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',

	  'CC-Session-Failover' => [?'CC-SESSION-FAILOVER_SUPPORTED'],
	  'Credit-Control-Failure-Handling' =>
	      [?'CREDIT-CONTROL-FAILURE-HANDLING_RETRY_AND_TERMINATE']
	 },
    CCA = gy_ccr(Msg, CCA0),

    case do([error_m ||
		check_3gpp(PS),
		check_subscription(Msg),
		check_user_equipment(Msg)]) of
	ok ->
	    {reply, ['CCA' | CCA]};
	_Other ->
	    ct:pal("Check Fail: ~p", [_Other]),
	    {answer_message, 3001}
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
	    }) ->
    ok;
check_3gpp(Msg) ->
    case maps:get('3GPP-IMSI', Msg, []) of
	[] -> error_m:return(no_imsi);
	_  ->
	    ct:pal("3GPP attribute check failed on ~p", [Msg]),
	    error_m:fail(Msg)
    end.

check_subscription(#{'Subscription-Id' := SId}) ->
    Map = lists:foldl(fun(#{'Subscription-Id-Type' := Type,
			    'Subscription-Id-Data' := Data}, M) ->
			      M#{Type => Data}
		      end, #{}, SId),
    case Map of
	#{?'DIAMETER_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164' := <<"46702123456">>,
	  ?'DIAMETER_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI' := <<"250071234567890">>} ->
	    ok;
	_ ->
	    error_m:fail(SId)
    end;
check_subscription(Msg) ->
    error_m:fail(Msg).

check_user_equipment(#{'User-Equipment-Info' :=
			   [#{'User-Equipment-Info-Type' :=
				  ?'DIAMETER_RO_USER-EQUIPMENT-INFO-TYPE_IMEISV',
			      'User-Equipment-Info-Value' :=
				  <<82,21,50,96,32,80,30,0>>}]}) ->
    ok;
check_user_equipment(Msg) ->
    error_m:fail(Msg).

%%%===================================================================
%%% Request processing
%%%===================================================================

gy_ccr(#{'CC-Request-Type' := Type, 'Multiple-Services-Credit-Control' := MSCCreq}, CCA)
  when Type =:= ?'CC-REQUEST-TYPE_INITIAL_REQUEST';
       Type =:= ?'CC-REQUEST-TYPE_UPDATE_REQUEST' ->
    MSCC =
	lists:map(
	  fun(#{'Rating-Group' := [RatingGroup],
		'Requested-Service-Unit' := [#{}]}) ->
		  #{'Envelope-Reporting' =>
			[?'DIAMETER_3GPP_CHARGING_ENVELOPE-REPORTING_DO_NOT_REPORT_ENVELOPES'],
		    'Granted-Service-Unit' =>
			[#{'CC-Time' => [36000],
			   'CC-Total-Octets' => [10485760]}],
		    'Rating-Group' => [RatingGroup],
		    'Result-Code' => [?'DIAMETER_BASE_RESULT-CODE_SUCCESS'],
		    'Time-Quota-Threshold' => [3600],
		    'Validity-Time' => [3600],
		    'Volume-Quota-Threshold' => [1048576]
		   }
	  end, MSCCreq),
    CCA#{'Multiple-Services-Credit-Control' => MSCC};
gy_ccr(#{'CC-Request-Type' := ?'CC-REQUEST-TYPE_TERMINATION_REQUEST',
	 'Multiple-Services-Credit-Control' := MSCCreq}, CCA) ->
    MSCC =
	lists:map(
	  fun(#{'Rating-Group' := [RatingGroup]}) ->
		  #{'Rating-Group' => [RatingGroup],
		    'Result-Code' => [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']
		   }
	  end, MSCCreq),
    CCA#{'Multiple-Services-Credit-Control' => MSCC};
gy_ccr(_, CCA) ->
    CCA#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'}.


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
