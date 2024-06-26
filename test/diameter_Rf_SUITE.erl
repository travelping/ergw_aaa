%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_Rf_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("gtplib/include/gtp_packet.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/diameter_3gpp_ts32_299_rf.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(HUT, ergw_aaa_rf).
-define(SERVICE, <<"diam-test">>).

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'  => <<"NAS">>,
		'Framed-Protocol' => 'PPP',
		'Service-Type'    => 'Framed-User'}}).

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
	#{rate_limits =>
	      #{default => #{outstanding_requests => 20, rate => 20}},
	  functions => ?DIAMETER_FUNCTION,
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_rf => ?DIAMETER_RF_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
		<<"Rf">> =>
		    #{handler => 'ergw_aaa_rf'}},
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
    [simple_session,
     multi_event_session,
     inoctets_crash,
     async,
     secondary_rat_usage_data_report,
     handle_failure,
     handle_answer_error,
     terminate,
     rate_limit,
     encode_error,
     plmn_change
    ].

init_per_suite(Config0) ->
    %% the overhead of logging interfers with the rate limit tests
    logger:set_primary_config(level, none),

    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),

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
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

init_per_testcase(secondary_rat_usage_data_report, Config) ->
    reset_session_stats(),
    meck_reset(Config),
    meck:new(diameter_test_server, [passthrough, no_link]),
    Config;
init_per_testcase(plmn_change, Config) ->
    reset_session_stats(),
    meck_reset(Config),
    meck:new(diameter_test_server, [passthrough, no_link]),
    Config;
init_per_testcase(_, Config) ->
    reset_session_stats(),
    meck_reset(Config),
    Config.

end_per_testcase(secondary_rat_usage_data_report, _Config) ->
    meck:unload(diameter_test_server),
    ok;
end_per_testcase(plmn_change, _Config) ->
    meck:unload(diameter_test_server),
    ok;
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

simple_session() ->
    [{doc, "Simple Rf session"}].
simple_session(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {ok, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(2, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

multi_event_session() ->
    [{doc, "Rf session with multiple charing events"}].
multi_event_session(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    TD =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{13,34,00}}]}],

    RfUpdCont = #{service_data => SDC},
    RfUpdCDR  = #{service_data => SDC, traffic_data => TD},
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCDR, {rf, 'Update'},
				SOpts#{'gy_event' => cdr_closure}),

    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC, traffic_data => TD},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {ok, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(3, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

inoctets_crash() ->
    [{doc, "Rf session with in octets clashing with the TDVs"}].
inoctets_crash(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'         => 4,
	   'Change-Time'              => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    TD =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{13,30,00}}]},
	  #{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [0],
	   'Change-Time'      => [{{2020,2,20},{13,34,00}}]}],

    RfUpdCont = #{service_data => SDC},
    RfUpdCDR  = #{'InOctets' => 1, 'OutOctets' => 1, service_data => SDC, traffic_data => TD},
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCDR, {rf, 'Update'},
				SOpts#{'gy_event' => cdr_closure}),

    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCont, {rf, 'Update'},
				SOpts#{'gy_event' => container_closure}),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC, traffic_data => TD},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {ok, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(3, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

async() ->
    [{doc, "Rf session with multiple charing events, async API"}].
async(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    AsyncSOpts = SOpts#{async => true},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    RfUpd = #{service_data => SDC},
    ?match({ok, _},
	ergw_aaa_session:invoke(SId, RfUpd, {rf, 'Update'},
				AsyncSOpts#{'gy_event' => container_closure})),
    ?match({ok, _},
	ergw_aaa_session:invoke(SId, RfUpd, {rf, 'Update'},
				AsyncSOpts#{'gy_event' => container_closure})),
    ?match({ok, _},
	ergw_aaa_session:invoke(SId, RfUpd, {rf, 'Update'},
				AsyncSOpts#{'gy_event' => cdr_closure})),

    ?match({ok, _},
	ergw_aaa_session:invoke(SId, RfUpd, {rf, 'Update'},
				AsyncSOpts#{'gy_event' => container_closure})),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC},
    ergw_aaa_session:invoke(SId, #{}, stop, AsyncSOpts),
    {ok, _Session2} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, AsyncSOpts),
    ct:sleep(100),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(3, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

secondary_rat_usage_data_report() ->
    [{doc, "Rf session with RAN-Secondary-RAT-Usage-Report"}].
secondary_rat_usage_data_report(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SecRatReport0 =
	#{'RAN-Secondary-RAT-Usage-Report' =>
	      [#{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0, 0,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0, 5,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		},
	       #{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0, 5,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0,10,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		},
	       #{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0,10,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0,15,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		}]},
    ergw_aaa_session:invoke(SId, SecRatReport0, {rf, 'Update'}, SOpts#{async => false}),

    SecRatReport1 =
	#{'RAN-Secondary-RAT-Usage-Report' =>
	      [#{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0,15,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0,20,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		},
	       #{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0,20,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0,25,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		},
	       #{'3GPP-Charging-Id' => [3779765295],
		 'Accounting-Input-Octets' => [1],
		 'Accounting-Output-Octets' => [2],
		 'RAN-Start-Timestamp' => [{{2020,1,1}, {0,30,0}}],
		 'RAN-End-Timestamp' =>   [{{2020,1,1}, {0,35,0}}],
		 'Secondary-RAT-Type' => [<<0>>]
		}]},
    ergw_aaa_session:invoke(SId, SecRatReport1, {rf, 'Update'}, SOpts#{async => false}),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {ok, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(2, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    DReqs = lists:foldr(
	      fun({_, {_, handle_request,
		       [#diameter_packet{
			   msg = ['ACR' |
				  #{'Accounting-Record-Type' := Type} = Msg]}, _, _, _]
		      }, _}, A) ->
		      maps:update_with(Type, fun(X) -> [Msg|X] end, [Msg], A);
		 (_, A) -> A
	      end, #{}, meck:history(diameter_test_server)),
    ?equal(true, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_START_RECORD', DReqs)),
    ?equal(false, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD', DReqs)),
    ?equal(true, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD', DReqs)),

    #{?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_START_RECORD' := [StartR],
      ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD' := [StopR]} = DReqs,

    DiamGet = fun DiamGet([K|T], [V]) when is_map(V) ->
		      DiamGet(T, maps:get(K, V, undefined));
		  DiamGet([K|T], V) when is_map(V) ->
		      DiamGet(T, maps:get(K, V, undefined));
		  DiamGet(_, V) -> V
	      end,
    RANKey = ['Service-Information', 'PS-Information', 'RAN-Secondary-RAT-Usage-Report'],

    ?match(undefined, DiamGet(RANKey, StartR)),
    SecRatR = DiamGet(RANKey, StopR),
    ?equal(6, length(SecRatR)),
    ?match(#{'3GPP-Charging-Id' := [3779765295],
	     'Accounting-Input-Octets' := [1],
	     'Accounting-Output-Octets' := [2],
	     'RAN-End-Timestamp'   := [{{2020,1,1},{0,_,0}}],
	     'RAN-Start-Timestamp' := [{{2020,1,1},{0,_,0}}],
	     'Secondary-RAT-Type' := [<<0>>]}, hd(SecRatR)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

handle_failure(Config) ->
    SOpts =
	#{now => erlang:monotonic_time(),
	  '3GPP-IMSI' => <<"FAIL">>,
	  '3GPP-MSISDN' => <<"FAIL">>},
    Session = init_session(SOpts, Config),

    Stats0 = get_stats(?SERVICE),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    ?match({{fail, 3001}, _, _},
	   ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts)),

    %% a session that has been rejected can not be in a `started` state
    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(1, proplists:get_value({{3, 271, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(1, proplists:get_value({{3, 271, 0}, recv, {'Result-Code', 3001}}, Statistics)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

handle_answer_error(Config) ->
    SOpts =
	#{now => erlang:monotonic_time(),
	  '3GPP-IMSI' => <<"FAIL-BROKEN-ANSWER">>,
	  '3GPP-MSISDN' => <<"FAIL-BROKEN-ANSWER">>},
    Session = init_session(SOpts, Config),

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    ?match({{error, 3007}, _, _},
	   ergw_aaa_session:invoke(SId, SOpts, {rf, 'Initial'}, [])),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

terminate() ->
    [{doc, "Simulate unexpected owner termination"}].
terminate(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    ?match(ok, ergw_aaa_session:terminate(SId)),
    wait_for_session(ergw_aaa_rf, started, 0, 10),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(2, proplists:get_value(stats({'ACR', send}), Stats1)),
    ?equal(2, proplists:get_value(stats({'ACA', recv, {'Result-Code',2001}}), Stats1)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

rate_limit() ->
    [{doc, "Test rate limiting"}].
rate_limit(_Config) ->
    Self = self(),

    CollectFun =
	fun CollectFun(_, [], Acc) ->
		Acc;
	    CollectFun(Key, [H|T], Acc) ->
		receive
		    {H, Key, {R, _, _}} ->
			maps:update_with(R, fun(X) -> X + 1 end, 1, CollectFun(Key, T, Acc))
		after 2000 ->
			error
		end
	end,

    %% make sure the token bucket is completely filled, previous test might have drained it
    ct:sleep(2000),

    %% with 1 peers, a rate limit at 20 req/s and 2 retries (= 3 attempts max) we should
    %% be able to get 20 requests through
    SRefs = [begin Ref = make_ref(), spawn(?MODULE, async_session, [Self, Ref]), Ref end
	     || _ <- lists:seq(1, 60)],

    CCRi = CollectFun('Initial', SRefs, #{}),
    CCRt = CollectFun('Terminate', SRefs, #{}),

    %% make sure to refill the token buckets, so that the following tests don't fail
    ct:sleep(1100),

    ?match(#{ok := OkayI, {error,rate_limit} := LimitI}
	     when OkayI >= 20 andalso
		  LimitI /= 0 andalso
		  OkayI + LimitI =:= 60, CCRi),
    ?match(#{ok := OkayT, {error,rate_limit} := LimitT}
	     when OkayT >= 20 andalso
		  LimitT /= 0 andalso
		  OkayT + LimitT =:= 60, CCRt),
    ok.

encode_error() ->
    [{doc, "Check that a message encode error does not leave the "
      "outstanding requests counter for the peer incremented"}].
encode_error(Config) ->
    BrokenQoS =
	#{
	  'QoS-Information' =>
	      #{
		%% 32bit overflow, will fail to encode
		'QoS-Class-Identifier' => 4294968000,
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
    Session = init_session(BrokenQoS, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    R = ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),
    ct:pal("R: ~p", [R]),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(1, proplists:get_value({{3, 271, 1}, send, error}, Stats1)),

    %% there should be not outstanding requests
    ?match(0, outstanding_reqs()),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

plmn_change() ->
    [{doc, "Rf session with PLMN change event"}].
plmn_change(Config) ->
    Session = init_session(#{}, Config),
    Stats0 = get_stats(?SERVICE),

    SOpts = #{now => erlang:monotonic_time()},
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 1}], get_session_stats()),

    SDC =
	[#{'Rating-Group'	      => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'	      => 4,
	   'Change-Time'	      => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'	      => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'	      => {{2018,11,30},{13,21,00}},
	   'Time-Usage'		      => 60},
	 #{'Rating-Group'	      => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'	      => 4,
	   'Change-Time'	      => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'	      => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'	      => {{2018,11,30},{13,21,00}},
	   'Time-Usage'		      => 60},
	 #{'Rating-Group'	      => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Change-Condition'	      => 4,
	   'Change-Time'	      => {{2018,11,30},{13,22,00}},
	   'Time-First-Usage'	      => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'	      => {{2018,11,30},{13,21,00}},
	   'Time-Usage'		      => 60}
	],

    TD =
	[#{'3GPP-Charging-Id' => [123456],
	   'Accounting-Input-Octets' => [1],
	   'Accounting-Output-Octets' => [2],
	   'Change-Condition' => [4],
	   'Change-Time'      => [{{2020,2,20},{13,34,00}}]}],

    SessionUpdate =
	#{'Change-Condition'        => 6,
          '3GPP-RAT-Type'	    => 2,
	  '3GPP-SGSN-Address'	    => {192,168,100,1},
	  '3GPP-SGSN-MCC-MNC'	    => {<<"901">>,<<"01">>},
	  'User-Location-Info' =>
	      #{'ext-macro-eNB' =>
		    #ext_macro_enb{plmn_id = {<<"901">>, <<"01">>},
				   id = rand:uniform(16#1fffff)},
		'TAI' =>
		    #tai{plmn_id = {<<"001">>, <<"01">>},
			 tac = rand:uniform(16#ffff)}}
	  },
    ergw_aaa_session:set(SId, SessionUpdate),

    RfUpdCDR  = #{service_data => SDC, traffic_data => TD},
    {ok, _, _} =
	ergw_aaa_session:invoke(SId, RfUpdCDR, {rf, 'Update'},
				SOpts#{'gy_event' => cdr_closure}),

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC, traffic_data => TD},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    {ok, _Session2, _} =
	ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),

    ?equal([{ergw_aaa_rf, started, 0}], get_session_stats()),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ct:pal("Stats: ~p~n", [Stats1]),
    ?equal(3, proplists:get_value({{3, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    DReqs = lists:foldr(
	      fun({_, {_, handle_request,
		       [#diameter_packet{
			   msg = ['ACR' |
				  #{'Accounting-Record-Type' := Type} = Msg]}, _, _, _]
		      }, _}, A) ->
		      maps:update_with(Type, fun(X) -> [Msg|X] end, [Msg], A);
		 (_, A) -> A
	      end, #{}, meck:history(diameter_test_server)),
    ?equal(true, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_START_RECORD', DReqs)),
    ?equal(true, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD', DReqs)),
    ?equal(true, maps:is_key(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD', DReqs)),

    #{?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_START_RECORD' := [StartR],
      ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD' := [InterimR],
      ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD' := [StopR]} = DReqs,

    DiamGet = fun DiamGet([K|T], [V]) when is_map(V) ->
		      DiamGet(T, maps:get(K, V, undefined));
		  DiamGet([K|T], V) when is_map(V) ->
		      DiamGet(T, maps:get(K, V, undefined));
		  DiamGet(_, V) -> V
	      end,
    PsKey = ['Service-Information', 'PS-Information'],
    ?match([#{'3GPP-SGSN-MCC-MNC' := [<<"26201">>]}], DiamGet(PsKey, StartR)),
    ?match([#{'3GPP-SGSN-MCC-MNC' := [<<"90101">>]}], DiamGet(PsKey, InterimR)),
    ?match([#{'3GPP-SGSN-MCC-MNC' := [<<"90101">>]}], DiamGet(PsKey, StopR)),

    PrevKey = ['Service-Information', 'TP-Previous-PS-Information'],
    ?match(undefined, DiamGet(PrevKey, StartR)),
    ?match([#{'3GPP-SGSN-MCC-MNC' := [<<"26201">>]}], DiamGet(PrevKey, InterimR)),
    ?match([#{'3GPP-SGSN-MCC-MNC' := [<<"90101">>]}], DiamGet(PrevKey, StopR)),

    %% make sure nothing crashed
    ?match(0, outstanding_reqs()),
    meck_validate(Config),
    ok.

%%%======================================================================
%%% Rate limit test helper to generate the requests in separate processes
%%%======================================================================

async_session(Owner, Ref) ->
    async_session(Owner, Ref, 1100).

async_session(Owner, Ref, Delay) ->
    SOpts = #{now => erlang:monotonic_time()},
    Session = init_session(#{}, []),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),

    {ok, _Session1, _} =
	ergw_aaa_session:invoke(SId, #{}, start, SOpts),
    IResult = ergw_aaa_session:invoke(SId, #{}, {rf, 'Initial'}, SOpts),
    Owner ! {Ref, 'Initial', IResult},

    timer:sleep(Delay),

    SDC =
	[#{'Rating-Group'             => 3000,
	   'Accounting-Input-Octets'  => 1092,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 2000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60},
	 #{'Rating-Group'             => 1000,
	   'Accounting-Input-Octets'  => 0,
	   'Accounting-Output-Octets' => 0,
	   'Time-First-Usage'         => {{2018,11,30},{13,20,00}},
	   'Time-Last-Usage'          => {{2018,11,30},{13,21,00}},
	   'Time-Usage'               => 60}
	],

    RfTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       service_data => SDC},
    ergw_aaa_session:invoke(SId, #{}, stop, SOpts),
    TResult = ergw_aaa_session:invoke(SId, RfTerm, {rf, 'Terminate'}, SOpts),
    Owner ! {Ref, 'Terminate', TResult},

    ok.

%%%===================================================================
%%% Generic helpers
%%%===================================================================

stats('ACR') -> {3, 271, 1};
stats('ACA') -> {3, 271, 0};
stats(Tuple) when is_tuple(Tuple) ->
    setelement(1, Tuple, stats(element(1, Tuple)));
stats(V) -> V.
