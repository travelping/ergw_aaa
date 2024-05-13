%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_Gy_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("gtplib/include/gtp_packet.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").
-include("ergw_aaa_internal.hrl").

-define(HUT, ergw_aaa_ro).
-define(SERVICE, <<"diam-test">>).
-define(API, gy).
-define(SET_TC_INFO(Name, Value), set_test_info(?FUNCTION_NAME, Name, Value)).
-define(GET_TC_INFO(Name), get_test_info(?FUNCTION_NAME, Name)).
-define(GET_TC_INFO(Name, Default), get_test_info(?FUNCTION_NAME, Name)).
-define(LIST_TC_INFO(), list_test_info(?FUNCTION_NAME)).
-define(DELETE_TC_INFO(Name), delete_test_info(?FUNCTION_NAME, Name)).
-define(CLEAR_TC_INFO(), clear_test_info(?FUNCTION_NAME)).

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'  => <<"NAS">>,
		'Framed-Protocol' => 'PPP',
		'Service-Type'    => 'Framed-User'}}).

-define(DIAMETER_TRANSPORTS,
	[#{connect_to => <<"aaa://127.0.10.10">>},
	 #{connect_to => <<"aaa://127.0.10.20">>},
	 #{connect_to => <<"aaa://127.0.10.30">>},
	 #{connect_to => <<"aaa://127.0.10.40">>}]).

-define(TEST_SERVER_TRANSPORTS,
	[{{127, 0, 10, 10}, "server1.test-srv.example.com"},
	 {{127, 0, 10, 20}, "server2.test-srv.example.com"},
	 {{127, 0, 10, 30}, "server3.test-srv.example.com"},
	 {{127, 0, 10, 40}, "server4.test-srv.example.com"}]).

-define(TEST_SERVER_CALLBACK_OVERRIDE,
	#{diameter_gy => [{handle_request, {?MODULE, test_server_request, []}}]}).

-define(DIAMETER_FUNCTION,
	#{?SERVICE =>
	      #{handler => ergw_aaa_diameter,
		'Origin-Host' => ?'Origin-Host',
		'Origin-Realm' => ?'Origin-Realm',
		transports => ?DIAMETER_TRANSPORTS}}).

-define(DIAMETER_RO_CONFIG,
	#{function => ?SERVICE,
	  'Destination-Realm' => <<"test-srv.example.com">>}).

-define(CONFIG,
	#{rate_limits =>
	      #{default => #{outstanding_requests => 10, rate => 10}},
	  functions => ?DIAMETER_FUNCTION,
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_ro => ?DIAMETER_RO_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
		<<"Ro">> =>
		    #{handler => 'ergw_aaa_ro',
		      answers =>
			  #{<<"OCS-Hold">> =>
				#{avps =>
				      #{'Result-Code' => 2001,
					'Multiple-Services-Credit-Control' =>
					    [#{'Envelope-Reporting' => [0],
					       'Granted-Service-Unit' =>
						   [#{'CC-Time-Min' => [1800],
						      'CC-Time-Max' => [1900]}],
					       'Rating-Group' => [1000],
					       'Result-Code' => [2001],
					       'Time-Quota-Threshold' => [60]}]},
				  state => ocs_hold
				 }
			   }
		     }
	       },
	  apps =>
	      #{default =>
		  #{'Origin-Host' => <<"dummy.host">>,
		    procedures =>
			#{init => [#{service => <<"Default">>}],
			  authenticate => [],
			  authorize => [],
			  start => [],
			  interim => [],
			  stop => [],
			  {gy, 'CCR-Initial'} =>
			      [#{service => <<"Ro">>,
				 tx_timeout => 1000,
				 max_retries => 2,
				 answer_if_down => <<"OCS-Hold">>,
				 answer_if_timeout => <<"OCS-Hold">>}],
			  {gy, 'CCR-Update'} =>
			      [#{service => <<"Ro">>,
				 tx_timeout => 1000,
				 answer_if_down => <<"OCS-Hold">>,
				 answer_if_timeout => <<"OCS-Hold">>}],
			  {gy, 'CCR-Terminate'} =>
			      [#{service => <<"Ro">>}]}
	       }
	 }}).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [diameter_srv,
     simple_session,
     simple_session_async,
     abort_session_request,
     tarif_time_change,
     ocs_hold_initial_timeout,
     ocs_hold_update_timeout,
     ocs_hold_update_timeout_async,
     ocs_hold_unexpected_session_down,
     ccr_retry,
     % ccr_t_rate_limit,
     rate_limit,
     handle_failure,
     handle_answer_error,
     handle_3xxx_error_async,
     handle_authorization_rejected,
     diameter_metrics,
     terminate].

init_per_suite(Config0) ->
    %% the overhead of logging interfers with the rate limit tests
    logger:set_primary_config(level, none),

    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),

    meck_init(Config),

    init_test_info_ets(),

    TestTransports =
	[[{transport_module, diameter_tcp},
	  {capabilities, [{'Origin-Host', Host}]},
	  {transport_config,
	   [{reuseaddr, true}, {ip, IP}]}]
	 || {IP, Host} <- ?TEST_SERVER_TRANSPORTS],

    diameter_test_server:start(?TEST_SERVER_CALLBACK_OVERRIDE, TestTransports),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    ergw_aaa_test_lib:ergw_aaa_init(?CONFIG),

    case wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    {skip, Other}
    end.

end_per_suite(Config) ->
    meck_unload(Config),
    application:stop(prometheus),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

init_per_testcase(_, Config) ->
    reset_session_stats(),
    meck_reset(Config),
    ?match(0, outstanding_reqs()),
    Config.

end_per_testcase(_, _Config) ->
    wait_for_outstanding_reqs(10),
    ets:delete_all_objects(?MODULE),
    ok.

%%%===================================================================
%%% Helper
%%%===================================================================

init_session(Session, _Config) ->
    Defaults =
	#{
	  '3GPP-GGSN-Address'       => {172,20,16,28},
	  '3GPP-IMEISV'             => <<82,21,50,96,32,80,30,0>>,
	  '3GPP-IMSI'               => <<"250071234567890">>,
	  '3GPP-Charging-Id'        => 3604013806,
	  '3GPP-IMSI-MCC-MNC'       => {<<"259">>,<<"99">>},
	  '3GPP-GGSN-MCC-MNC'       => {<<"258">>,<<"88">>},
	  '3GPP-MS-TimeZone'        => {128,1},
	  '3GPP-MSISDN'             => <<"46702123456">>,
	  '3GPP-NSAPI'              => 5,
	  '3GPP-PDP-Type'           => 'IPv4',
	  '3GPP-RAT-Type'           => 6,
	  '3GPP-SGSN-Address'       => {192,168,1,1},
	  '3GPP-SGSN-MCC-MNC'       => {<<"262">>,<<"01">>},
	  '3GPP-Selection-Mode'     => 0,
	  'User-Location-Info' =>
	      #{'ext-macro-eNB' =>
		    #ext_macro_enb{plmn_id = {<<"001">>, <<"001">>},
				   id = rand:uniform(16#1fffff)},
		'TAI' =>
		    #tai{plmn_id = {<<"001">>, <<"001">>},
			 tac = rand:uniform(16#ffff)}},
	  'Called-Station-Id'       => <<"some.station.gprs">>,
	  'Calling-Station-Id'      => <<"543148000012345">>,
	  'Framed-IP-Address'       => {10,106,14,227},
	  'Framed-Protocol'         => 'GPRS-PDP-Context',
	  'Multi-Session-Id'        => 1012552258277823040188863251876666193415858290601,
	  'Username'                => <<"ergw">>,
	  'Password'                => <<"ergw">>,
	  'Service-Type'            => 'Framed-User',
	  'Node-Id'                 => <<"PGW-001">>,
	  'PDP-Context-Type'        => primary,
	  'Charging-Rule-Base-Name' => <<"m2m0001">>,

	  '3GPP-GPRS-Negotiated-QoS-Profile' =>   <<11,146,31,147,150,64,64,255,
						    255,255,255,17,1,1,64,64>>,
	  '3GPP-Allocation-Retention-Priority' => 2,
	  '3GPP-Charging-Characteristics' =>  <<8,0>>,
	  'Termination-Cause' => 1

	      %%
	      %% some OCSs don't like this attribute on Gy, disable it for now
	      %%
	      %% 'QoS-Information' =>
	      %%     #{
	      %%	'QoS-Class-Identifier' => 8,
	      %%	'Max-Requested-Bandwidth-DL' => 0,
	      %%	'Max-Requested-Bandwidth-UL' => 0,
	      %%	'Guaranteed-Bitrate-DL' => 0,
	      %%	'Guaranteed-Bitrate-UL' => 0,
	      %%	'Allocation-Retention-Priority' =>
	      %%	    #{'Priority-Level' => 10,
	      %%	      'Pre-emption-Capability' => 1,
	      %%	      'Pre-emption-Vulnerability' => 0},
	      %%	'APN-Aggregate-Max-Bitrate-DL' => 84000000,
	      %%	'APN-Aggregate-Max-Bitrate-UL' => 8640000
	      %%      }
	 },
    maps:merge(Defaults, Session).

wait_for_outstanding_reqs(0) ->
    ct:fail(timeout);
wait_for_outstanding_reqs(Cnt) ->
    case outstanding_reqs() of
	0 -> ok;
	_ ->
	    ct:sleep(100),
	    wait_for_outstanding_reqs(Cnt - 1)
    end.

%%%===================================================================
%%% Test cases
%%%===================================================================

diameter_srv() ->
    [{doc, "Test some diameter_srv functions"}].
diameter_srv(_Config) ->
    Ref0 = make_ref(),
    Ref1 = make_ref(),

    ?equal({Ref0, 'conn-0'},
	   ergw_aaa_diameter_srv:pick_connection(
	     [{Ref1, 'conn-1'}, {Ref0, 'conn-0'}],
	     #{Ref0 => 0, Ref1 => 1})),

    ?match({_, 'conn-0'},
	   ergw_aaa_diameter_srv:pick_connection(
	     [{Ref1, 'conn-1'}, {Ref0, 'conn-0'}, {make_ref(), 'conn-0'}],
	     #{Ref0 => 0, Ref1 => 1})),

    ?match({ok, {Ref0, _}},
	   ergw_aaa_diameter_srv:pick_peer_h(
	     [{Ref0, #diameter_caps{origin_host = {undefined, <<"conn-0">>}}},
	      {Ref1, #diameter_caps{origin_host = {undefined, <<"conn-1">>}}}],
	     undefined,
	     #{<<"conn-0">> => #peer{outstanding =  0, capacity = 100, rate = 100, tokens = 100},
	       <<"conn-1">> => #peer{outstanding = 50, capacity = 100, rate = 100, tokens = 100}})),
    ?match({ok, {Ref1, _}},
	   ergw_aaa_diameter_srv:pick_peer_h(
	     [{Ref0, #diameter_caps{origin_host = {undefined, <<"conn-0">>}}},
	      {Ref1, #diameter_caps{origin_host = {undefined, <<"conn-1">>}}}],
	     undefined,
	     #{<<"conn-0">> => #peer{outstanding = 50, capacity = 100, rate = 100, tokens = 100},
	       <<"conn-1">> => #peer{outstanding =  0, capacity = 100, rate = 100, tokens = 100}})),
    ?match({ok, {Ref1, _}},
	   ergw_aaa_diameter_srv:pick_peer_h(
	     [{Ref0, #diameter_caps{origin_host = {undefined, <<"conn-0">>}}},
	      {Ref1, #diameter_caps{origin_host = {undefined, <<"conn-0">>}}}],
	     undefined,
	     #{Ref0 => 10,
	       <<"conn-0">> => #peer{outstanding = 0, capacity = 100, rate = 100, tokens = 100}})),
    ok.

simple_session() ->
    [{doc, "Simple Gy session"}].
simple_session(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, Session1, Events1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    ?match([{update_credits,[_,_,_]}], Events1),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session1)),

    UsedCredits =
	#{3000 => #{'CC-Input-Octets'  => [1092],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [1092],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  2000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 },
    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => maps:to_list(UsedCredits)},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),

    ?match([{update_credits,[_,_,_]}], Events2),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session2)),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    ?equal([{ergw_aaa_ro, started, 0}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

simple_session_async() ->
    [{doc, "Simple Gy session"}].
simple_session_async(Config) ->
    Session = init_session(#{}, Config),
    SOpts = #{async => true},
    GyOpts =
	#{credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, Session1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, SOpts),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    Events1 =
	receive
	    {update_session, _, Ev1} -> Ev1
	after 100 -> ct:fail(timeout)
	end,
    ?match([{update_credits,[_,_,_]}], Events1),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session1)),

    UsedCredits =
	#{3000 => #{'CC-Input-Octets'  => [1092],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [1092],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  2000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 },
    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => maps:to_list(UsedCredits)},
    {ok, Session2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, SOpts),

    ?equal([{ergw_aaa_ro, started, 0}], get_session_stats()),

    Events2 =
	receive
	    {update_session, _, Ev2} -> Ev2
	after 100 -> ct:fail(timeout)
	end,
    ?match([{update_credits,[_,_,_]}], Events2),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session2)),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

abort_session_request() ->
    [{doc, "Stop Gy session with ASR"}].
abort_session_request(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),
    StatsTestSrv0 = get_stats(diameter_test_server),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, Session1, Events1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),
    ?match([{update_credits,[_,_,_]}], Events1),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session1)),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    SessionId = maps:get('Diameter-Session-Id', Session1),
    ?equal(ok, diameter_test_server:abort_session_request(gy, SessionId, ?'Origin-Host', ?'Origin-Realm')),

    receive
	#aaa_request{procedure = {?API, 'ASR'}} = Request ->
	    ergw_aaa_session:response(Request, ok, #{}, #{})
    after 1000 ->
	    ct:fail("no ASR")
    end,

    UsedCredits =
	#{3000 => #{'CC-Input-Octets'  => [1092],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [1092],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  2000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']},
	  1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 },
    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => maps:to_list(UsedCredits)},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),

    ?equal([{ergw_aaa_ro, started, 0}], get_session_stats()),

    ?match([{update_credits,[_,_,_]}], Events2),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session2)),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    StatsTestSrv = diff_stats(StatsTestSrv0, get_stats(diameter_test_server)),

    %% check that client has recieved CCA
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% check that client has send ACA
    ?equal(1, proplists:get_value({{4, 274, 0}, send, {'Result-Code',2001}}, Stats1)),

    %% check that test server has recieved ACA
    ?equal(1, proplists:get_value({{4, 274, 0}, recv, {'Result-Code',2001}}, StatsTestSrv)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

tarif_time_change() ->
    [{doc, "Simple Gy session"}].
tarif_time_change(Config) ->
    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, Session1, Events1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    ?match([{update_credits,[_]}], Events1),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session1)),

    UsedCredits =
	[{1000, #{'CC-Input-Octets'  => [0],
		  'CC-Output-Octets' => [0],
		  'CC-Time'          => [60],
		  'CC-Total-Octets'  => [0],
		  'Tariff-Change-Usage' =>
		      [?'DIAMETER_3GPP_CHARGING_TARIFF-CHANGE-USAGE_UNIT_AFTER_TARIFF_CHANGE'],
		  'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}},
	 {1000, #{'CC-Input-Octets'  => [0],
		  'CC-Output-Octets' => [0],
		  'CC-Time'          => [60],
		  'CC-Total-Octets'  => [0],
		  'Tariff-Change-Usage' =>
		      [?'DIAMETER_3GPP_CHARGING_TARIFF-CHANGE-USAGE_UNIT_BEFORE_TARIFF_CHANGE'],
		  'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}}
	],
    GyUpdate = #{used_credits => UsedCredits},
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, GyUpdate, {gy, 'CCR-Update'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),

    ?equal([{ergw_aaa_ro, started, 0}], get_session_stats()),

    ?match([{update_credits,[_]}], Events2),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session2)),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(3, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

ccr_retry(Config) ->
    DTRA =
	fun(#diameter_packet{header = Header}, _SVC, {PeerRef, _}, _Extra) ->
		#diameter_header{is_retransmitted = Retransmit,
				 end_to_end_id = E2EId,
				 hop_by_hop_id = H2HId} = Header,
		PrevRequests = ?GET_TC_INFO(ccr_i_retries),
		ReqData = {PeerRef, Retransmit, E2EId, H2HId},
		?SET_TC_INFO(ccr_i_retries, [ReqData | PrevRequests]),
		%% assuming config of 2 retries and 1s TX timeout in the suite config for CCR-I
		case PrevRequests of
		    [] -> discard;
		    [_] -> discard;
		    [_, _] -> ok
		end
	end,

    ?SET_TC_INFO(ccr_i_retries, []),

    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    Stats0 = get_stats(?SERVICE),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, DiameterSId} = ergw_aaa_session:get(SId, 'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId, DTRA),

    {ok, _Session1, _Events1} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    RequestsInfo = ?GET_TC_INFO(ccr_i_retries),

    %% last 2 requests have retry flag set, the 1st not
    ?equal([true, true, false], [RetryFlag || {_, RetryFlag, _, _} <- RequestsInfo]),

    %% all requests have the same end to end id
    ?equal(1, length(lists:usort([E2EId || {_, _, E2EId, _} <- RequestsInfo]))),

    %% each request has different hop by hop id
    ?equal(3, length(lists:usort([H2HId || {_, _, _, H2HId} <- RequestsInfo]))),

    %% each request came on different peer
    ?equal(3, length(lists:usort([PeerRef || {PeerRef, _, _, _} <- RequestsInfo]))),
    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),

    %% diameter discards the timeout, so will be counting only 1
    ?equal(1, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),

    ets:delete_all_objects(?MODULE),
    ?match(ok, ergw_aaa_session:terminate(SId)),
    wait_for_session(ergw_aaa_ro, started, 0, 10),
    ok.

rate_limit(_Config) ->
    Self = self(),

    CollectFun =
	fun CollectFun(_, [], Acc) ->
		Acc;
	    CollectFun(Key, [H|T], Acc) ->
		receive
		    {H, Key, {R, _, _}} ->
			maps:update_with(R, fun(X) -> X + 1 end, 1, CollectFun(Key, T, Acc))
		end
	end,

    %% make sure the token bucket is completely filled, previous test might have drained it
    ct:sleep(2000),

    %% with 4 peers, a rate limit at 10 req/s and 2 retries (= 3 attempts max) we should
    %% be able to get 40 requests through
    SRefs = [begin Ref = make_ref(), spawn(?MODULE, async_session, [Self, Ref]), Ref end
	     || _ <- lists:seq(1, 60)],

    CCRi = CollectFun('CCR-Initial', SRefs, #{}),
    CCRt = CollectFun('CCR-Terminate', SRefs, #{}),

    %% make sure to refill the token buckets, so that the following tests don't fail
    ct:sleep(1100),

    ?match(#{ok := OkayI, {error,rate_limit} := LimitI}
	     when OkayI >= 40 andalso
		  LimitI /= 0 andalso
		  OkayI + LimitI =:= 60, CCRi),
    ?match(#{ok := OkayT, {error,rate_limit} := LimitT}
	     when OkayT >= 40 andalso
		  LimitT /= 0 andalso
		  OkayT + LimitT =:= 60, CCRt),
    ok.
ccr_t_rate_limit() ->
    [{doc, "older version of the rate limit test, does not work, keep for reference"}].
ccr_t_rate_limit(Config) ->
    %% this test is somewhat trickier : we need to generate sessions with a rate independent
    %% from separate processes to avoid dependency on rate limiter settings. Also it should
    %% work on dev and CI machines with different performance

    %% spawning 50 sessions, with the jobs config above (2 s max time and 10 req/s) this should
    %% send around 20 requests (give and take some computing noise)
    [spawn_link(fun basic_session/0) || _ <- lists:seq(1,50)],

    %% wait a little to make sure all CCR-I messages are done
    timer:sleep(500),

    %% collect stats for CCR-Ts
    ReceivedRate = stat_check(diameter_test_server, 1000, {{4, 272, 1},recv}, 3),

    %% collect the CCR-T results
    ResCnt = ets:new(resultcount, [ordered_set]),
    [ets:update_counter(ResCnt, Result, 1, {Result, 0}) || {{_, _}, Result} <- ?LIST_TC_INFO()],
    ResList = ets:tab2list(ResCnt),
    ct:pal("ResList: ~p", [ResList]),
    Ok = proplists:get_value(ok, ResList, 0),
    RateLimited = proplists:get_value(rate_limit, ResList, 0),

    ct:pal("ReceivedRate: ~p", [ReceivedRate]),
    %% no received rate sample is greatet than 10 req/s as defined in the rate limit config
    ?equal([], lists:filter(fun(Recv_s) -> Recv_s > 10 end, ReceivedRate)),

    %% same amount of requests returned ok (i.e. not rate limited), as received on the other side
    ?equal(Ok, lists:sum(ReceivedRate)),

    %% all 50 requests were either rate limited or ok (no other error)
    ?equal(50, Ok + RateLimited),

    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),
    ok.

ocs_hold_initial_timeout(Config) ->
    %% Time out all requests on the test server to trigger ocs_hold on CCR-I
    DTRA = fun(_Request, _Svc, _Peers, _Extra) -> discard end,
    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, DiameterSId} = ergw_aaa_session:get(SId, 'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId, DTRA),

    {ok, _Session1, Events} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, ocs_hold, 1}], get_session_stats()),

    ?match([{update_credits,
	     [#{'Granted-Service-Unit' := [#{'CC-Time' := [Time]}]}]}
	   ] when Time > 1800 andalso Time =< 1900, Events),

    %% Invoke Update and check if it stops the session without triggering a CCR-U
    Stats0 = get_stats(?SERVICE),
    UsedCredits =
	#{1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_VALIDITY_TIME']}
	 },
    GyTerm = #{used_credits => maps:to_list(UsedCredits)},
    ?match({{error, ocs_hold_end}, _Session2, [{stop, {?API, ocs_hold_end}}]},
	   ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Update'}, [])),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),

    %% Make sure we didn't send anything
    ?equal(0, proplists:get_value({{4, 272, 1}, send}, Stats1)),

    ?equal([{ergw_aaa_ro, ocs_hold, 0}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),
    ok.

ocs_hold_update_timeout(Config) ->
    %% Respond to CCR-I and time out CCR-U to trigger OCS Hold there
    DTRA =
	fun (#diameter_packet{msg = ['CCR' | #{'CC-Request-Type' := 2}]},
	     _Svc, _Peer, _Extra) ->
		timer:sleep(2000),
		ok;
	    (_Packet, _Svc, _Peer, _Extra) -> ok
	end,

    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, DiameterSId} = ergw_aaa_session:get(SId, 'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId, DTRA),

    %% Send CCR-I
    Stats0 = get_stats(?SERVICE),

    {ok, _Session1, Events} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    %% Check if the data is coming from the test server
    ?match([{update_credits, [#{'Volume-Quota-Threshold' := [1048576]}]}], Events),
    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(1, proplists:get_value({{4, 272, 0}, recv, {'Result-Code', 2001}}, Stats1)),

    %% Invoke Update and check that the data is returned from OCS Hold config
    UsedCredits =
	#{1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_VALIDITY_TIME']}
	 },
    GyUpdate = #{used_credits => maps:to_list(UsedCredits)},
    {ok, _Session2, Events2} = ergw_aaa_session:invoke(SId, GyUpdate, {gy, 'CCR-Update'}, []),
    ?match([{update_credits,
	     [#{'Granted-Service-Unit' := [#{'CC-Time' := [Time]}]}]}
	   ] when Time > 1800 andalso Time =< 1900, Events2),

    ?equal([{ergw_aaa_ro, ocs_hold, 1}, {ergw_aaa_ro, started, 0}], get_session_stats()),

    %% Invoke Terminate and check if the session is terminated, while not sending CCR-T
    Stats2 = get_stats(?SERVICE),

    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    {{error, ocs_hold_end}, _Session3, [{stop, {?API, ocs_hold_end}}]} = ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),

    ?equal([{ergw_aaa_ro, ocs_hold, 0}, {ergw_aaa_ro, started, 0}], get_session_stats()),

    Stats3 = diff_stats(Stats2, get_stats(?SERVICE)),

    %% Make sure we didn't send anything
    ?equal(0, proplists:get_value({{4, 272, 1}, send}, Stats3)),

    ?equal([{ergw_aaa_ro,ocs_hold,0},{ergw_aaa_ro,started,0}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),
    ok.

ocs_hold_update_timeout_async(Config) ->
    %% Respond to CCR-I and time out CCR-U to trigger OCS Hold there
    DTRA =
	fun (#diameter_packet{msg = ['CCR' | #{'CC-Request-Type' := 2}]},
	     _Svc, _Peer, _Extra) ->
		timer:sleep(2000),
		ok;
	    (_Packet, _Svc, _Peer, _Extra) -> ok
	end,

    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, DiameterSId} = ergw_aaa_session:get(SId, 'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId, DTRA),

    %% Send CCR-I
    Stats0 = get_stats(?SERVICE),

    {ok, _Session1, Events} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    %% Check if the data is coming from the test server
    ?match([{update_credits, [#{'Volume-Quota-Threshold' := [1048576]}]}], Events),
    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(1, proplists:get_value({{4, 272, 0}, recv, {'Result-Code', 2001}}, Stats1)),

    %% Invoke Update and check that the data is returned from OCS Hold config
    UsedCredits =
	#{1000 => #{'CC-Input-Octets'  => [0],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [0],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_VALIDITY_TIME']}
	 },
    GyUpdate = #{used_credits => maps:to_list(UsedCredits)},
    {ok, _Session2} = ergw_aaa_session:invoke(SId, GyUpdate, {gy, 'CCR-Update'}, #{async => true}),
    Events2 =
	receive
	    {update_session, _, Ev1} -> Ev1
	after 5000 -> ct:fail(timeout)
	end,

    ?match([{update_credits,
	     [#{'Granted-Service-Unit' := [#{'CC-Time' := [Time]}]}]}
	   ] when Time > 1800 andalso Time =< 1900, Events2),

    ?equal([{ergw_aaa_ro, ocs_hold, 1}, {ergw_aaa_ro, started, 0}], get_session_stats()),

    %% Invoke Terminate and check if the session is terminated, while not sending CCR-T
    Stats2 = get_stats(?SERVICE),

    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    {{error, ocs_hold_end}, _Session3, [{stop, {?API, ocs_hold_end}}]} = ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),

    ?equal([{ergw_aaa_ro, ocs_hold, 0}, {ergw_aaa_ro, started, 0}], get_session_stats()),

    Stats3 = diff_stats(Stats2, get_stats(?SERVICE)),

    %% Make sure we didn't send anything
    ?equal(0, proplists:get_value({{4, 272, 1}, send}, Stats3)),

    ?equal([{ergw_aaa_ro,ocs_hold,0},{ergw_aaa_ro,started,0}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),
    ok.

ocs_hold_unexpected_session_down(Config) ->
    %% Time out all requests on the test server to trigger ocs_hold on CCR-I
    DTRA = fun(_Request, _Svc, _Peers, _Extra) -> discard end,
    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, DiameterSId} = ergw_aaa_session:get(SId, 'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId, DTRA),

    {ok, _Session1, _Events} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),

    ?equal([{ergw_aaa_ro, ocs_hold, 1}], get_session_stats()),

    Stats0 = get_stats(?SERVICE),

    %% pretend we went down
    SId ! {'DOWN', make_ref(), process, self(), normal},
    ct:sleep(100),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),

    %% Make sure we didn't send anything
    ?equal(0, proplists:get_value({{4, 272, 1}, send}, Stats1)),

    ?equal([{ergw_aaa_ro, ocs_hold, 0}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ?CLEAR_TC_INFO(),
    ok.

handle_failure(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{'3GPP-IMSI' => <<"FAIL">>,
	  '3GPP-MSISDN' => <<"FAIL">>,
	  credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    %% the retry logic causes this to be reported as error instead of fail
    ?match({{error, 3001}, _, _},
	   ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [])),

    ?equal([], get_session_stats()),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(3, proplists:get_value({{4, 272, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(3, proplists:get_value({{4, 272, 0}, recv, {'Result-Code', 3001}}, Statistics)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

handle_answer_error(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{'3GPP-IMSI' => <<"FAIL-BROKEN-ANSWER">>,
	  '3GPP-MSISDN' => <<"FAIL-BROKEN-ANSWER">>,
	  credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    ?match({{error, 3007}, _, _},
	   ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [])),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

handle_3xxx_error_async(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{'3GPP-IMSI' => <<"FAIL-RC-3002">>,
	  '3GPP-MSISDN' => <<"FAIL-RC-3002">>,
	  credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _} =
	   ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, #{async => true}),
    Events1 =
	receive
	    {update_session, _, Ev1} -> Ev1
	after 100 -> ct:fail(timeout)
	end,
    ?equal([{stop,{gy,peer_reject}}], Events1),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

handle_authorization_rejected() ->
    [{doc, "check that a DIAMETER_AUTHORIZATION_REJECTED does not retransmit"}].
handle_authorization_rejected(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{'3GPP-IMSI' => <<"FAIL-RC-5003">>,
	  '3GPP-MSISDN' => <<"FAIL-RC-5003">>,
	  credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    ?match({{fail, 5003}, _, _},
	   ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [])),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),

    %% check that we didn't retransmit or fail over
    ?equal(1, proplists:get_value({{4,272,1},send}, Stats1)),
    ?equal(1, proplists:get_value({{4,272,0},recv}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

diameter_metrics() ->
    [{doc, "check DIAMETER metrics"}].
diameter_metrics(_Config) ->
    ?equal(ok, prometheus_registry:register_collector(ergw_aaa, ergw_aaa_prometheus_collector)),
    Metrics = prometheus_text_format:format(ergw_aaa),
    ct:pal("Metrics: ~s", [Metrics]),
    ?match({match, _}, re:run(Metrics, "ergw_aaa_diameter_outstanding_requests")),
    ?match({match, _}, re:run(Metrics, "ergw_aaa_diameter_available_tokens")),
    ?match({match, _}, re:run(Metrics, "ergw_aaa_diameter_no_tokens_available_total")),
    ?match({match, _}, re:run(Metrics, "ergw_aaa_diameter_no_capacity_left_total")).

terminate() ->
    [{doc, "Simulate unexpected owner termination"}].
terminate(Config) ->
    Session = init_session(#{}, Config),
    GyOpts =
	#{credits =>
	      #{1000 => empty,
		2000 => empty,
		3000 => empty
	       }
	 },

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, _, _} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),
    ?equal([{ergw_aaa_ro, started, 1}], get_session_stats()),

    ?match(ok, ergw_aaa_session:terminate(SId)),
    wait_for_session(ergw_aaa_ro, started, 0, 10),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(2, proplists:get_value(stats({'CCR', send}), Stats1)),
    ?equal(2, proplists:get_value(stats({'CCA', recv, {'Result-Code',2001}}), Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

%%%======================================================================
%%% Rate limit test helper to generate the requests in separate processes
%%%======================================================================
basic_session() ->
    basic_session(1100).
basic_session(CCR_I_T_Delay) ->
    Session = init_session(#{}, []),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    GyOpts = #{credits => #{1000 => empty}},
    Result =
	case ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []) of
	    {ok, _, _} ->
		timer:sleep(CCR_I_T_Delay),

		UsedCredits =
		    [{1000, #{'CC-Input-Octets'  => [0],
			      'CC-Output-Octets' => [0],
			      'CC-Time'          => [60],
			      'CC-Total-Octets'  => [0],
			      'Reporting-Reason' =>
				  [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
		     }],
		GyTerm = #{'Termination-Cause' =>
			       ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
			   used_credits => UsedCredits},
		case ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []) of
		    {ok, _, _} -> ok;
		    {{error, rate_limit},_ ,_} -> rate_limit;
		    {Err, _, _} -> {'CCR-I', Err}
		end;
	    {{error, rate_limit},_ ,_} -> rate_limit;
	    {Err, _, _} -> {'CCR-T', Err}
	end,
    set_test_info(ccr_t_rate_limit, {session, SId}, Result).

async_session(Owner, Ref) ->
    async_session(Owner, Ref, 1100).

async_session(Owner, Ref, Delay) ->
    Session = init_session(#{}, []),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    GyOpts = #{credits => #{1000 => empty}},
    IResult = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),
    Owner ! {Ref, 'CCR-Initial', IResult},

    timer:sleep(Delay),
    UsedCredits =
	[{1000, #{'CC-Input-Octets'  => [0],
		  'CC-Output-Octets' => [0],
		  'CC-Time'          => [60],
		  'CC-Total-Octets'  => [0],
		  'Reporting-Reason' =>
		      [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 }],
    GyTerm = #{'Termination-Cause' =>
		   ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    TResult = ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),
    Owner ! {Ref, 'CCR-Terminate', TResult},

    ok.

%%%===================================================================
%%% Test server request handler override
%%%===================================================================

test_server_request(#diameter_packet{msg = [_ | #{'Session-Id' := Sid}]} = Packet, Svc, Peer, Extra) ->
    Resp =
	case get_diameter_session_handler(Sid) of
	    Fun when is_function(Fun) ->
		Fun(Packet, Svc, Peer, Extra);
	    _ ->
		ok
	end,
    case Resp of
	ok ->
	    diameter_test_server:handle_request(Packet, Svc, Peer, Extra);
	_ ->
	    Resp
    end.

%%%===================================================================
%%% Generic helpers
%%%===================================================================


init_test_info_ets() ->
    spawn(fun() ->
		  ets:new(?MODULE, [named_table, public, {write_concurrency, true}]),
		  receive
		      stop -> ok
		  end
	  end).

stop_test_info_ets() ->
    ets:info(?MODULE, owner) ! stop.

set_test_info(TC, Name, Value) ->
    ets:insert(?MODULE, {{TC,Name}, Value}).

get_test_info(TC, Name) ->
    case ets:lookup(?MODULE, {TC, Name}) of
	[] -> undefined;
	[{_, Value}] -> Value
    end.

delete_test_info(TC, Name) ->
    case ets:lookup(?MODULE, {TC, Name}) of
	[] -> undefined;
	[{_, Value}] ->
	    ets:delete(?MODULE, {TC, Name}),
	    Value
    end.

clear_test_info(TC) ->
    ets:match_delete(?MODULE, {{TC, '_'}, '_'}).

list_test_info(TC) ->
    [list_to_tuple(Res) || Res <- ets:match(?MODULE, {{TC, '$1'}, '$2'})].

set_diameter_session_handler(SId, Fun) ->
    ets:insert(?MODULE, {{diameter_session_handler, SId}, Fun}).

get_diameter_session_handler(SId) ->
    case ets:lookup(?MODULE, {diameter_session_handler, SId}) of
	[] -> undefined;
	[{_, Fun}] -> Fun
    end.


delete_diameter_session_handler(SId) ->
    ets:delete(?MODULE, {diameter_session_handler, SId}).

stat_check(Svc, Period, Key, Count) ->
    Value = proplists:get_value(Key, get_stats(Svc), 0),
    stat_check(Svc, Period, Key, Count, [Value]).
stat_check(_Svc, _Period, _Key, 0, Result) ->
    [First | Data] = lists:reverse(Result),
    {ResList, _} =
	lists:mapfoldl(fun(Value, PrevValue) -> {Value - PrevValue, Value} end, First, Data),
    ResList;
stat_check(Svc, Period, Key, Count, Acc) ->
    timer:sleep(Period),
    Value = proplists:get_value(Key, get_stats(Svc), 0),
    stat_check(Svc, Period, Key, Count-1, [Value | Acc]).

stats('CCR') -> {4, 272, 1};
stats('CCA') -> {4, 272, 0};
stats(Tuple) when is_tuple(Tuple) ->
    setelement(1, Tuple, stats(element(1, Tuple)));
stats(V) -> V.
