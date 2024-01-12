%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(complex_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_ituma.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("gtplib/include/gtp_packet.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/diameter_3gpp_ts32_299_rf.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(HUT, ergw_aaa_rf).
-define(SERVICE, <<"combined-test">>).
%% -define(SERVICE, <<"aaa-test">>).

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'  => <<"NAS">>,
		'Framed-Protocol' => 'PPP',
		'Service-Type'    => 'Framed-User'}}).

-define(RADIUS_CONFIG,
	#{server =>
	      #{host => {127,0,0,1},
		port => 1812,
		secret => <<"secret">>}}).

-define(DIAMETER_TRANSPORT,
	#{connect_to => <<"aaa://127.0.0.1">>}).

-define(DIAMETER_FUNCTION,
	#{?SERVICE =>
	      #{handler => ergw_aaa_diameter,
		'Origin-Host' => ?'Origin-Host',
		'Origin-Realm' => ?'Origin-Realm',
		transports => [?DIAMETER_TRANSPORT]}}).

-define(DIAMETER_RF_CONFIG,
	#{function => ?SERVICE,
	  'Destination-Realm' => <<"test-srv.example.com">>}).

-define(CONFIG,
	%% #{rate_limits =>
	%%       #{default => #{outstanding_requests => 50, rate => 1000}},
	#{rate_limits =>
	      #{default => #{outstanding_requests => 3, rate => 20}},
	  functions => ?DIAMETER_FUNCTION,
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_radius => ?RADIUS_CONFIG,
		ergw_aaa_rf => ?DIAMETER_RF_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
		<<"Rf">> =>
		    #{handler => 'ergw_aaa_rf'},
		<<"RADIUS-Auth">> =>
		    #{handler => 'ergw_aaa_radius',
		      server =>
			  #{host => {127,0,0,1},
			    port => 1812,
			    secret => <<"secret">>}},
		<<"RADIUS-Acct">> =>
		    #{handler => 'ergw_aaa_radius',
		      server =>
			  #{host => {127,0,0,1},
			    port => 1813,
			    secret => <<"secret">>}}},
	  apps =>
	      #{default =>
		    #{'Origin-Host' => <<"dummy.host">>,
		      procedures =>
			  #{init => [#{service => <<"Default">>}],
			    authenticate => [#{service => <<"RADIUS-Auth">>}],
			    authorize    => [#{service => <<"RADIUS-Auth">>}],
			    start        => [#{service => <<"RADIUS-Acct">>}],
			    interim      => [#{service => <<"RADIUS-Acct">>}],
			    stop         => [#{service => <<"RADIUS-Acct">>}],
			    {rf, 'Initial'}   => [#{service => <<"Rf">>}],
			    {rf, 'Update'}    => [#{service => <<"Rf">>}],
			    {rf, 'Terminate'} => [#{service => <<"Rf">>}]
			  }
	            }
	      }
       }).



%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [radius_rf_tdv_crash
    ].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),

    eradius_test_handler:start(),

    meck_init(Config),

    diameter_test_server:start(),
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
    eradius_test_handler:stop(),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

init_per_testcase(_, Config) ->
    reset_session_stats(),
    meck_reset(Config),
    Config.

end_per_testcase(_, _Config) ->
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
	  '3GPP-Charging-Characteristics' =>      <<8,0>>,

	  'QoS-Information' =>
	      #{
		'QoS-Class-Identifier' => 255,
		'Max-Requested-Bandwidth-DL' => 0,
		'Max-Requested-Bandwidth-UL' => 0,
		'Guaranteed-Bitrate-DL' => 0,
		'Guaranteed-Bitrate-UL' => 0,
		'Allocation-Retention-Priority' =>
		    #{'Priority-Level' => 10,
		      'Pre-emption-Capability' => 1,
		      'Pre-emption-Vulnerability' => 0},
		'APN-Aggregate-Max-Bitrate-DL' => 84000000,
		'APN-Aggregate-Max-Bitrate-UL' => 8640000
	       }
	 },
    maps:merge(Defaults, Session).

%%%===================================================================
%%% Test cases
%%%===================================================================

radius_rf_tdv_crash() ->
    [{doc, "Rf session with in octets clashing with the TDVs"}].
radius_rf_tdv_crash(Config) ->
    CustomSession = #{'3GPP-IMSI' => <<"999999999999999">>,
		      'Framed-IP-Address' => {10,10,10,10},
		      'Framed-IPv6-Prefix' => {{16#fe80,0,0,0,0,0,0,0}, 64},
		      'Framed-Pool' => <<"pool-A">>,
		      'Framed-IPv6-Pool' => <<"pool-A">>,
		      'Framed-Interface-Id' => {0,0,0,0,0,0,0,1}},
    Session = init_session(CustomSession, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    %% ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    {ok, SOut, Events} = ergw_aaa_session:invoke(SId, #{}, authenticate, []),
    ?match(#{'MS-Primary-DNS-Server' := {8,8,8,8}}, SOut),
    ?match(#{'Framed-MTU' := 1500}, SOut),

    %% ?equal(0, get_session_stats(ergw_aaa_radius, started)),

    ?match([{set, {{accounting, 'IP-CAN', periodic}, {periodic, 'IP-CAN', 1800, []}}}],
	   Events),
    ?match({ok, _, _}, ergw_aaa_session:invoke(SId, #{}, authorize, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(SId, #{}, start, [])),

    ?equal(1, get_session_stats(ergw_aaa_radius, started)),

    SDC0 =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{12,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{12,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{12,21,00}},
	   'Time-Usage'               => 60}
	],

    TD0 =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{12,30,00}}]},
	  #{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [0],
	   'Change-Time'      => [{{2020,2,20},{12,34,00}}]}],

    SDC1 =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    TD1 =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [3],
	   'Accounting-Output-Octets' => [4],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{13,30,00}}]},
	  #{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [3],
	   'Accounting-Output-Octets' => [4],
	   'Change-Condition' => [0],
	   'Change-Time'      => [{{2020,2,20},{13,34,00}}]}],

    SDC2 =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 2092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{14,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{14,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{14,21,00}},
	   'Time-Usage'               => 60}
	],

    TD2 =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [5],
	   'Accounting-Output-Octets' => [6],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{14,30,00}}]},
	  #{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [5],
	   'Accounting-Output-Octets' => [6],
	   'Change-Condition' => [0],
	   'Change-Time'      => [{{2020,2,20},{14,34,00}}]}],

    RfUpdCont1 = #{service_data => SDC0, traffic_data => TD0},
    %% RfUpdCDR1  = #{service_data => SDC1, traffic_data => TD1},
    RfUpdCont2 = #{service_data => SDC2, traffic_data => TD2},
    %% The diameter server discards any request with this IMSI
    {_, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont1, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),
    ct:pal("Rf state (after container closure): ~p", [sys:get_state(SId)]),
    %% ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    %% The diameter server discards any request with this IMSI
    {_, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont2, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),
    ct:pal("Rf state (after container closure): ~p", [sys:get_state(SId)]),

    %% We ensure the session has several TDVs so the In/OutOctets from_session clause crashes
%%     {_, _, _} =
%% 	ergw_aaa_session:invoke(SId, RfUpdCDR1, {rf, 'Update'},
%% 				SOpts#{'gy_event' => cdr_closure}),
%%     ct:pal("Rf state (after cdr closure): ~p", [sys:get_state(SId)]),

    InterimData = #{
	'InPackets' => 10,
	'OutPackets' => 20,
	'InOctets' => 100,
	'OutOctets' => 200},
    ?match({ok, _, _}, ergw_aaa_session:invoke(SId, InterimData, interim, [])),

    %% ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    %% The diameter server discards any request with this IMSI
    {_, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont2, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),
    ct:pal("Rf state (after container closure): ~p", [sys:get_state(SId)]),

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC2, traffic_data => TD2},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {_, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    %% ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    %% ?equal(4, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    TermOpts = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'},
    ?match({ok, _, _}, ergw_aaa_session:invoke(SId, TermOpts, stop, [])),

    ?equal(0, get_session_stats(ergw_aaa_radius, started)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

%%%===================================================================
%%% Generic helpers
%%%===================================================================

stats('ACR') -> {3, 271, 1};
stats('ACA') -> {3, 271, 0};
stats(Tuple) when is_tuple(Tuple) ->
    setelement(1, Tuple, stats(element(1, Tuple)));
stats(V) -> V.
