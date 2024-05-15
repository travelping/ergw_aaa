%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_test_server).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-include_lib("kernel/include/logger.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("../include/diameter_rfc4006_cc.hrl").
-include("../include/diameter_3gpp_ts29_061_sgi.hrl").
-include("../include/diameter_3gpp_ts29_212.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/diameter_3gpp_ts32_299_ro.hrl").

-export([start/0, start/2,
	 start_nasreq/0, start_nasreq/2,
	 stop/0,
	 abort_session_request/4,
	 re_auth_request/5]).

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

-define(DIAMETER_DICT_BASE_ACC, diameter_3gpp_ts29_061_sgi_base_acc).
-define(DIAMETER_APP_ID_BASE_ACC, ?DIAMETER_DICT_BASE_ACC:id()).

-define(DIAMETER_DICT_NASREQ, diameter_3gpp_ts29_061_sgi).
-define(DIAMETER_APP_ID_NASREQ, ?DIAMETER_DICT_NASREQ:id()).

-define(DIAMETER_DICT_GX, diameter_3gpp_ts29_212).
-define(DIAMETER_APP_ID_GX, ?DIAMETER_DICT_GX:id()).

-define(DIAMETER_DICT_RO, diameter_3gpp_ts32_299_ro).
-define(DIAMETER_APP_ID_RO, ?DIAMETER_DICT_RO:id()).

-define(DIAMETER_DICT_RF, diameter_3gpp_ts32_299_rf).
-define(DIAMETER_APP_ID_RF, ?DIAMETER_DICT_RF:id()).

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

%%===================================================================
%% API
%%===================================================================

start() ->
    start(#{}, default_transport()).

start(CallbackOverrides, Transports) ->
    application:ensure_all_started(diameter),
    SvcOpts = [{'Origin-Host', "server.test-srv.example.com"},
	       {'Origin-Realm', "test-srv.example.com"},
	       {'Vendor-Id', ?VENDOR_ID_TP},
	       {'Product-Name', "Server"},
	       {'Supported-Vendor-Id', [?VENDOR_ID_3GPP,
					?VENDOR_ID_ETSI,
					?VENDOR_ID_TP]},
	       {'Auth-Application-Id', [?DIAMETER_APP_ID_GX, ?DIAMETER_APP_ID_RO]},
	       {'Acct-Application-Id', [?DIAMETER_APP_ID_RF]},
	       {'Vendor-Specific-Application-Id',
		[#'diameter_base_Vendor-Specific-Application-Id'{
		    'Vendor-Id'           = ?VENDOR_ID_3GPP,
		    'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}]},
	       {restrict_connections, false},
	       {string_decode, false},
	       {decode_format, map},
	       {application, [{alias, base6733},
			      {dictionary, diameter_gen_base_rfc6733},
			      {module, [?MODULE, base6733]}]},
	       {application, [{alias, diameter_gx},
			      {dictionary, ?DIAMETER_DICT_GX},
			      {module, callback_overrides(diameter_gx, CallbackOverrides, [gx])}]},
	       {application, [{alias, diameter_gy},
			      {dictionary, ?DIAMETER_DICT_RO},
			      {module, callback_overrides(diameter_gy, CallbackOverrides, [gy])}]},
	       {application, [{alias, diameter_rf},
			      {dictionary, ?DIAMETER_DICT_RF},
			      {module, callback_overrides(diameter_rf, CallbackOverrides, [rf])}]}],
    ok = diameter:start_service(?MODULE, SvcOpts),

    [{ok, _} = diameter:add_transport(?MODULE, {listen, Transport}) || Transport <- Transports],
    ok.

start_nasreq() ->
    start_nasreq(#{}, default_transport()).

start_nasreq(CallbackOverrides, Transports) ->
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
	       {'Acct-Application-Id', [?DIAMETER_APP_ID_NASREQ,
					?DIAMETER_APP_ID_BASE_ACC]},
	       {'Vendor-Specific-Application-Id',
		[#'diameter_base_Vendor-Specific-Application-Id'{
		    'Vendor-Id'           = ?VENDOR_ID_3GPP,
		    'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}]},
	       {restrict_connections, false},
	       {string_decode, false},
	       {decode_format, map},
	       {application, [{alias, base6733},
			      {dictionary, diameter_gen_base_rfc6733},
			      {module, [?MODULE, base6733]}]},
	       {application, [{alias, nasreq},
			      {dictionary, ?DIAMETER_DICT_NASREQ},
			      {module, callback_overrides(nasreq, CallbackOverrides, [nasreq])}]},
	       {application, [{alias, nasreq_base_acc},
			      {dictionary, ?DIAMETER_DICT_BASE_ACC},
			      {module, callback_overrides(nasreq, CallbackOverrides, [nasreq])}]},
	       {application, [{alias, diameter_gx},
			      {dictionary, ?DIAMETER_DICT_GX},
			      {module, callback_overrides(diameter_gx, CallbackOverrides, [gx])}]},
	       {application, [{alias, diameter_gy},
			      {dictionary, ?DIAMETER_DICT_RO},
			      {module, callback_overrides(diameter_gy, CallbackOverrides, [gy])}]}],
    ok = diameter:start_service(?MODULE, SvcOpts),

    [{ok, _} = diameter:add_transport(?MODULE, {listen, Transport}) || Transport <- Transports],
    ok.

stop() ->
    diameter:stop_service(?MODULE),
    application:stop(diameter).

re_auth_request(gx, SessionId, DH, DR, AVPs)
  when is_map(AVPs) ->
    RAR = AVPs#{'Session-Id' => SessionId,
		'Destination-Realm' => DR,
		'Destination-Host' => DH,
		'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
		'Re-Auth-Request-Type' =>
		    ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY',
		'Charging-Rule-Install' =>
		    [#{'Charging-Rule-Name' => <<"service01">>}]
	       },
    diameter:call(?MODULE, diameter_gx, [ 'RAR' | RAR ], [detach]).

abort_session_request(gx, SessionId, DH, DR) ->
    ASR = #{'Session-Id' => SessionId,
	    'Destination-Realm' => DR,
	    'Destination-Host' => DH,
	    'Auth-Application-Id' => ?DIAMETER_APP_ID_GX},
    diameter:call(?MODULE, diameter_gx, [ 'ASR' | ASR ], [detach]);
abort_session_request(gy, SessionId, DH, DR) ->
    ASR = #{'Session-Id' => SessionId,
	    'Destination-Realm' => DR,
	    'Destination-Host' => DH,
	    'Auth-Application-Id' => ?DIAMETER_APP_ID_RO},
    diameter:call(?MODULE, diameter_gy, [ 'ASR' | ASR ], [detach]);
abort_session_request(nasreq, SessionId, DH, DR) ->
    ASR = #{'Session-Id' => SessionId,
	    'Destination-Realm' => DR,
	    'Destination-Host' => DH,
	    'Auth-Application-Id' => ?DIAMETER_APP_ID_NASREQ},
    diameter:call(?MODULE, nasreq, [ 'ASR' | ASR ], [detach]).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State, _Extra) ->
    ?LOG(debug, "peer_up: ~p, ~p~n", [_Peer, _Extra]),
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
    ?LOG(debug, "prepare_request Msg: ~p", [Msg]),
    {send, Packet#diameter_packet{msg = Msg}};

prepare_request(Packet, _SvcName, {PeerRef, _}, _Extra) ->
    ?LOG(debug, "prepare_request to ~p: ~p", [PeerRef, Packet]),
    {send, Packet}.

prepare_retransmit(_Packet, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer, _Extra) ->
    Msg.

handle_error(_Reason, _Request, _SvcName, _Peer, _Extra) ->
    ?UNEXPECTED.

handle_request(#diameter_packet{
		  msg = [_ | #{'Called-Station-Id' := [<<"FAIL", _/binary>> = How]}]
		 } = Pkt, _SvcName, _, _Extra) ->
    fail_answer(How, Pkt);

handle_request(#diameter_packet{
		  msg = [_ |
			 #{'Subscription-Id' :=
			       [#{'Subscription-Id-Data' := <<"FAIL", _/binary>> = How}|_]}]
		 } = Pkt, _SvcName, _, _Extra) ->
    fail_answer(How, Pkt);

handle_request(#diameter_packet{
		  msg = [_ |
			 #{'Service-Information' :=
			       [#{'Subscription-Id' :=
				      [#{'Subscription-Id-Data' :=
					     <<"FAIL", _/binary>> = How}|_]}]}]
		 } = Pkt, _SvcName, _, _Extra) ->
    fail_answer(How, Pkt);

handle_request(#diameter_packet{msg = ['ACR' | Msg]}, _SvcName, {_, Caps}, _Extra)
  when is_map(Msg) ->
    case discard_msg(Msg) of
	true -> discard;
	false ->
	    InterimAccounting = 1,
	    #diameter_caps{origin_host = {OH, _},
			   origin_realm = {OR, _}} = Caps,
	    #{'Session-Id' := Id,
	      'Accounting-Record-Type' := Type,
	      'Accounting-Record-Number' := Number} = Msg,
	    ACA =  #{'Session-Id' => Id,
		     'Result-Code' => 2001,
		     'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Acct-Interim-Interval' => [InterimAccounting],
		     'Accounting-Record-Type' => Type,
		     'Accounting-Record-Number' => Number},
	    case check_3gpp(Msg) of
		Result when Result =:= ok;
			    Result =:= {ok, no_imsi};
				  Result =:= {ok, cgnat} ->
		    {reply, ['ACA' | ACA]};
		_ ->
		    {answer_message, 5005}
	    end
    end;

handle_request(#diameter_packet{msg = ['AAR' | Msg]}, _SvcName, {_, Caps}, _Extra)
  when is_map(Msg) ->
    InterimAccounting = 1800,
    AuthLifeTime = 3600,
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,
    #{'Session-Id' := Id,
      'Auth-Request-Type' := Type,
      'Auth-Application-Id' := AppId} = Msg,
    FramedIP = case maps:get('Framed-IP-Address', Msg, [<<0,0,0,0>>]) of
		   [<<0,0,0,0>>] -> [<<10,106,14,227>>];
		   FramedIPReq -> FramedIPReq
	       end,
    AAA0 =  #{'Session-Id' => Id,
	      'Result-Code' => 2001,
	      'Origin-Host' => OH,
	      'Origin-Realm' => OR,
  	      'SN-Primary-DNS-Server' => [{1,2,3,4}],
	      'SN-Secondary-DNS-Server' => [{5,6,7,8}],
 	      'Acct-Interim-Interval' => [InterimAccounting],
	      'Authorization-Lifetime' => [AuthLifeTime],
	      'Framed-IP-Address' => FramedIP,
	      'Framed-MTU' => 1500,
	      'Auth-Request-Type' => Type,
	      'Auth-Application-Id' => AppId},
    AAA = if is_map_key('Framed-IPv6-Prefix', Msg) ->
		  AAA0#{'3GPP-IPv6-DNS-Servers' =>
			    ergw_aaa_3gpp_dict:encode('3GPP-IPv6-DNS-Servers',
						      [{16#fe80,0,0,0,0,0,0,1},
						       {16#fe80,0,0,0,0,0,0,2}])};
	     true -> AAA0
	  end,
    {reply, ['AAA' | AAA]};

handle_request(#diameter_packet{msg = ['STR' | Msg]}, _SvcName, {_, Caps}, _Extra)
  when is_map(Msg) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,
    #{'Session-Id' := Id} = Msg,
    STA =  #{'Session-Id' => Id,
	     'Result-Code' => 2001,
	     'Origin-Host' => OH,
	     'Origin-Realm' => OR},
    {reply, ['STA' | STA]};


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
		check_user_equipment(Msg),
		check_tarif_time_change(Msg),
		%%
		%% some OCSs don't like the QoS attribute on Gy, make it optional now
		%%
		check_qos_info(get_avp(['Service-Information',
					'PS-Information',
					'QoS-Information'], Msg), false)
	    ]) of
	ok ->
	    {reply, ['CCA' | CCA]};
	_Other ->
	    ct:pal("Check Fail: ~p", [_Other]),
	    {answer_message, 3001}
    end;

handle_request(#diameter_packet{msg = _Msg}, _SvcName, _, _Extra) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

check_3gpp(#{'3GPP-IMSI'              := [<<"250071234567890">>],
	     '3GPP-Charging-Id'       := [3604013806],
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
check_3gpp(#{'3GPP-IMSI'              := [<<"250071234567890">>],
	     '3GPP-IMEISV'            := [<<82,21,50,96,32,80,30,0>>]
	    }) ->
    error_m:return(cgnat);
check_3gpp(#{'3GPP-IMSI' := [<<"noCheck">>]}) ->
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

check_usu(Units) ->
    M = lists:foldl(
	  fun(#{'Tariff-Change-Usage' := [TCU]}, A) ->
		  maps:update_with(TCU, _ + 1, 1, A);
	     (_, A) -> A
	  end, #{}, Units),
    %% ct:pal("USU Map: ~p", [M]),
    case M of
	_ when map_size(M) =:= 0 ->
	    ok;
	#{?'DIAMETER_3GPP_CHARGING_TARIFF-CHANGE-USAGE_UNIT_BEFORE_TARIFF_CHANGE' := 1,
	  ?'DIAMETER_3GPP_CHARGING_TARIFF-CHANGE-USAGE_UNIT_AFTER_TARIFF_CHANGE'  := 1} ->
	    ok;
	_ ->
	    error_m:fail(Units)
    end.

check_tarif_time_change(#{'Multiple-Services-Credit-Control' := MSCCreq}) ->
    F = fun Check([], Result) ->
		Result;
	    Check([#{'Used-Service-Unit' := Unit}|T], ok) ->
		Check(T, check_usu(Unit));
	    Check([_|T], Result) ->
		Check(T, Result)
      end,
    F(MSCCreq, ok);
check_tarif_time_change(Msg) ->
    error_m:fail(Msg).

check_qos_info(#{'APN-Aggregate-Max-Bitrate-DL' := [84000000],
		 'APN-Aggregate-Max-Bitrate-UL' := [8640000],
		 'Allocation-Retention-Priority' :=
		     [#{'Pre-emption-Capability' := [1],
			'Pre-emption-Vulnerability' := [0],
			'Priority-Level' := 10}],
		 'Guaranteed-Bitrate-DL' := [0],
		 'Guaranteed-Bitrate-UL' := [0],
		 'Max-Requested-Bandwidth-DL' := [0],
		 'Max-Requested-Bandwidth-UL' := [0],
		 'QoS-Class-Identifier' := "\b"}, _) ->
    ok;
check_qos_info(_, _Required = false) ->
    ok;
check_qos_info(Msg, _Required = true) ->
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

get_avp([], Value) ->
    Value;
get_avp([K | T], Msg)
  when is_map(Msg) ->
    case maps:get(K, Msg, #{}) of
	[Map] when is_map(Map) ->
	    get_avp(T, Map);
	V when is_map(V) ->
	    get_avp(T, V);
	Other ->
	    Other
    end;
get_avp(_, Msg) ->
    Msg.

%%% Override can be an atom (module) or {M,F,A} tuple, where the extra argument will be
%%% appended to the argument list. For now I just need handle request override, feel free
%%% to add what you need here.
callback_overrides(App, CallbackOverrides, ExtraArgs) ->
    CBR = #diameter_callback{default = ?MODULE, extra = ExtraArgs},
    case maps:get(App, CallbackOverrides, []) of
	[] -> CBR;
	AppOverrides ->
	    lists:foldl(
	      fun({handle_request, Override}, AccCBR) ->
		      AccCBR#diameter_callback{handle_request = Override};
		 ({handle_answer, Override}, AccCBR) ->
		      AccCBR#diameter_callback{handle_answer = Override};
		 ({handle_error, Override}, AccCBR) ->
		      AccCBR#diameter_callback{handle_error = Override};
		 ({pick_peer,Override}, AccCBR) ->
		      AccCBR#diameter_callback{pick_peer = Override};
		 ({peer_up, Override}, AccCBR) ->
		      AccCBR#diameter_callback{peer_up = Override};
		 ({peer_down, Override}, AccCBR) ->
		      AccCBR#diameter_callback{peer_down = Override};
		 ({prepare_request, Override}, AccCBR) ->
		      AccCBR#diameter_callback{prepare_request = Override};
		 ({prepare_retransmit, Override}, AccCBR) ->
		      AccCBR#diameter_callback{prepare_retransmit = Override}
	      end,
	      CBR,
	      AppOverrides
	     )
    end.

default_transport() ->
    [[{transport_module, diameter_tcp},
      {transport_config, [
			  {reuseaddr, true},
			  {ip, {127,0,0,1}}]}
     ]].

discard_msg(#{'Service-Information' :=
		[#{'Subscription-Id' := Ids}]}) ->
    lists:member(#{'Subscription-Id-Type' => 1,'Subscription-Id-Data' => <<"999999999999999">>}, Ids);
discard_msg(_) ->
    false.	    

fail_answer(<<"FAIL-BROKEN-ANSWER">>, Pkt) ->
    broken_answer(Pkt);
fail_answer(<<"FAIL-RC-", RC/binary>>, _Pkt) ->
    {answer_message, binary_to_integer(RC)};
fail_answer(_How, _Pkt) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

broken_answer(#diameter_packet{header = Hdr0, avps = ReqAVPs}) ->
    OH = get_avp_data('Destination-Host', ReqAVPs, <<"diameter.erlang.org">>),
    OR = get_avp_data('Destination-Realm', ReqAVPs, <<"erlang.org">>),
    SId = get_avp_data('Session-Id', ReqAVPs, <<"diameter.erlang.org;123;456">>),

    %% construct a message in raw (packet) form that is invalid
    %% by using the header/msg fields, we can bybass the syntax checks
    Hdr = Hdr0#diameter_header{
	    is_request = false,
	    is_error = true,
	    is_retransmitted = false},
    AVPs = avp([{263, SId},			%% Session-Id
		{268, <<3007:32>>},		%% Result-Code
		{264, OH},			%% Origin-Host
		{296, OR},			%% Origin-Realm
		{256, true, <<1:32>>}		%% Unassigned, used to trigger a decoder error
	       ]),
    {reply, #diameter_packet{header = Hdr, msg = ['answer-message' | AVPs]}}.

avp({{Code, Vendor}, Mandatory, Data}) ->
    #diameter_avp{code = Code, vendor_id = Vendor, is_mandatory = Mandatory, data = avp(Data)};

avp({Code, Data}) ->
    avp({Code, false, Data});

avp({Code, Mandatory, Data}) when is_integer(Code) ->
    avp({{Code, undefined}, Mandatory, Data});

avp(#diameter_avp{} = A) ->
    A;

avp(Avps) when is_list(Avps) ->
    lists:map(fun avp/1, Avps);

avp(V) when is_binary(V) ->
    V.

get_avp_data(Key, AVPs, Default) when is_list(AVPs) ->
    case lists:keyfind(Key, #diameter_avp.name, AVPs) of
	#diameter_avp{data = Data} -> Data;
	_                          -> Default
    end;
get_avp_data(_, _, Default) ->
    Default.
