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
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").

-import(ergw_aaa_test_lib, [meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1,
			    get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(HUT, ergw_aaa_ro).
-define(SERVICE, 'diam-test').

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

-define(STATIC_CONFIG,
	[{'NAS-Identifier',        <<"NAS">>},
	 {'Framed-Protocol',       'PPP'},
	 {'Service-Type',          'Framed-User'}]).

-define(DIAMETER_TRANSPORTS,
	[[{connect_to, <<"aaa://127.0.0.1:3870">>}],
	 [{connect_to, <<"aaa://127.0.0.1:3971">>}],
	 [{connect_to, <<"aaa://127.0.0.1:3872">>}],
	 [{connect_to, <<"aaa://127.0.0.1:3873">>}]]).

-define(TEST_SERVER_TRANSPORTS,
	[{3870, "server1.test-srv.example.com"},
	 {3871, "server2.test-srv.example.com"},
	 {3872, "server3.test-srv.example.com"},
	 {3873, "server4.test-srv.example.com"}]).

-define(TEST_SERVER_CALLBACK_OVERRIDE,
	#{diameter_gy => [{handle_request, {?MODULE, test_server_request, []}}]}).

-define(DIAMETER_FUNCTION,
	{?SERVICE,
	 [{handler, ergw_aaa_diameter},
	  {'Origin-Host', ?'Origin-Host'},
	  {'Origin-Realm', ?'Origin-Realm'},
	  {transports, ?DIAMETER_TRANSPORTS}
	 ]}).
-define(DIAMETER_RO_CONFIG,
	[{function, ?SERVICE},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).
-define(DIAMETER_SERVICE_OPTS, []).

-define(CCR_T_RATE_LIMIT_QUEUE_NAME, ccr_t_rate_limit).
-define(CCR_T_RATE_LIMIT_QUEUE_CONFIG, [{max_time, 2000}, {regulators, [{rate, [{limit, 10}]}]}]).

-define(CONFIG,
	[{functions, [?DIAMETER_FUNCTION]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_ro, ?DIAMETER_RO_CONFIG}
	  ]},
	 {services,
	  [{'Default',
	    [{handler, 'ergw_aaa_static'}]},
	   {'Ro',
	    [{handler, 'ergw_aaa_ro'}]}
	  ]},

	 {apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, []},
			   {authorize, []},
			   {start, []},
			   {interim, []},
			   {stop, []},
			   {{gy, 'CCR-Initial'},   [{'Ro', [{tx_timeout, 1000}, {max_retries, 2}]}]},
			   {{gy, 'CCR-Update'},    ['Ro']},
			   {{gy, 'CCR-Terminate'}, [{'Ro', [{rate_limit_queue, ?CCR_T_RATE_LIMIT_QUEUE_NAME}]}]}
			  ]}
	    ]}
	  ]}
	]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [simple_session, abort_session_request, tarif_time_change, ccr_retry, ccr_t_rate_limit].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    [application:set_env(ergw_aaa, Key, Opts) || {Key, Opts} <- ?CONFIG],

    meck_init(Config),

    init_test_info_ets(),

    TestTransports =
	[[{transport_module, diameter_tcp},
	  {capabilities, [{'Origin-Host', Host}]},
	  {transport_config,
	   [{reuseaddr, true}, {ip, {127, 0, 0, 1}},
	    {port, Port}]}]
	 || {Port, Host} <- ?TEST_SERVER_TRANSPORTS],

    diameter_test_server:start(?TEST_SERVER_CALLBACK_OVERRIDE, TestTransports),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    lager_common_test_backend:bounce(debug),

    jobs:add_queue(?CCR_T_RATE_LIMIT_QUEUE_NAME, ?CCR_T_RATE_LIMIT_QUEUE_CONFIG),

    case wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    {skip, Other}
    end.

end_per_suite(Config) ->
    stop_test_info_ets(),
    meck_unload(Config),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

init_per_testcase(Config) ->
    meck_reset(Config),
    Config.

end_per_testcase(_Config) ->
    clean_test_info(),
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
	  '3GPP-IMSI-MCC-MNC'       => <<"25999">>,
	  '3GPP-GGSN-MCC-MNC'       => <<"25888">>,
	  '3GPP-MS-TimeZone'        => {128,1},
	  '3GPP-MSISDN'             => <<"46702123456">>,
	  '3GPP-NSAPI'              => 5,
	  '3GPP-PDP-Type'           => 'IPv4',
	  '3GPP-RAT-Type'           => 6,
	  '3GPP-SGSN-Address'       => {192,168,1,1},
	  '3GPP-SGSN-MCC-MNC'       => <<"26201">>,
	  '3GPP-Selection-Mode'     => 0,
	  '3GPP-User-Location-Info' => <<24,98,242,16,64,163,98,242,16,1,156,232,0>>,
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
	  '3GPP-Charging-Characteristics' =>  <<8,0>>

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

%%%===================================================================
%%% Test cases
%%%===================================================================

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
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [], false),
    ?match([{update_credits,[_,_,_]}], Events1),
    ?match(#{'Multiple-Services-Credit-Control' := [_,_,_]}, Session1),

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
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, [], false),
    ?match([{update_credits,[_,_,_]}], Events2),
    ?match(#{'Multiple-Services-Credit-Control' := [_,_,_]}, Session2),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
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
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [], false),
    ?match([{update_credits,[_,_,_]}], Events1),
    ?match(#{'Multiple-Services-Credit-Control' := [_,_,_]}, Session1),

    SessionId = maps:get('Diameter-Session-Id', Session1),
    ?equal(ok, diameter_test_server:abort_session_request(gy, SessionId, ?'Origin-Host', ?'Origin-Realm')),

    receive
	#aaa_request{procedure = {_, 'ASR'}} ->
	    ergw_aaa_session:response(SId, ok, #{})
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
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, [], false),
    ?match([{update_credits,[_,_,_]}], Events2),
    ?match(#{'Multiple-Services-Credit-Control' := [_,_,_]}, Session2),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    StatsTestSrv = diff_stats(StatsTestSrv0, get_stats(diameter_test_server)),

    %% check that client has recieved CCA
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% check that client has send ACA
    ?equal(1, proplists:get_value({{4, 274, 0}, send, {'Result-Code',2001}}, Stats1)),

    %% check that test server has recieved ACA
    ?equal(1, proplists:get_value({{4, 274, 0}, recv, {'Result-Code',2001}}, StatsTestSrv)),

    %% make sure nothing crashed
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
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [], false),
    ?match([{update_credits,[_]}], Events1),
    ?match(#{'Multiple-Services-Credit-Control' := [_]}, Session1),

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
	ergw_aaa_session:invoke(SId, GyUpdate, {gy, 'CCR-Update'}, [], false),

    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, [], false),
    ?match([{update_credits,[_]}], Events2),
    ?match(#{'Multiple-Services-Credit-Control' := [_]}, Session2),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(3, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

ccr_retry(Config) ->
    DTRA =
	fun([#diameter_packet{header = Header}, {PeerRef, _}]) ->
		#diameter_header{is_retransmitted = Retransmit,
				 end_to_end_id = E2EId,
				 hop_by_hop_id = H2HId} = Header,
		PrevRequests = get_test_info(ccr_i_retries),
		ReqData = {PeerRef, Retransmit, E2EId, H2HId},
		set_test_info(ccr_i_retries, [ReqData | PrevRequests]),
		%% assuming config of 2 retries and 1s TX timeout in the suite config for CCR-I
		case PrevRequests of
		    [] -> timer:sleep(2000);
		    [_] -> timer:sleep(2000);
		    [_, _] -> ok
		end
	end,
    set_test_info(ccr_i_retries, []),
    set_test_info(diameter_test_request_action, DTRA),

    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{1000 => empty}},
    Stats0 = get_stats(?SERVICE),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, _Session1, _Events1} = ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [], false),
    RequestsInfo = get_test_info(ccr_i_retries),

    %% last 2 requests have retry flag set, the 1st not
    ?equal([true, true, false], [RetryFlag || {_, RetryFlag, _, _} <- RequestsInfo]),

    %% all requests have the same end to end id
    ?equal(1, length(lists:usort([E2EId || {_, _, E2EId, _} <- RequestsInfo]))),

    %% each requests has different hop by hop id
    ?equal(3, length(lists:usort([H2HId || {_, _, _, H2HId} <- RequestsInfo]))),

    %% each requests came on different peer
    ?equal(3, length(lists:usort([PeerRef || {PeerRef, _, _, _} <- RequestsInfo]))),
    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),

    %% diameter discards the for the timeout, so will be counting only 1
    ?equal(1, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    %% make sure nothing crashed
    meck_validate(Config),
    clean_test_info(),
    ok.

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
    [ets:update_counter(ResCnt, Result, 1, {Result, 0}) || {{_, _}, Result} <- list_test_info()],
    ResList = ets:tab2list(ResCnt),
    Ok = proplists:get_value(ok, ResList, 0),
    RateLimited = proplists:get_value(rate_limit, ResList, 0),

    %% no received rate sample is greatet than 10 req/s as defined in the rate limit config
    ?equal([], lists:filter(fun(Recv_s) -> Recv_s > 10 end, ReceivedRate)),

    %% same amount of requests returned ok (i.e. not rate limited), as received on the other side
    ?equal(Ok, lists:sum(ReceivedRate)),

    %% all 500 requests were either rate limited or ok (no other error)
    ?equal(50, Ok + RateLimited),

    meck_validate(Config),
    clean_test_info(),
    ok.

%%%===================================================================
%%% Rate limit test helper to generate the requests in a separate
%%%===================================================================
basic_session() ->
    basic_session(1000).
basic_session(CCR_I_T_Delay) ->
    Session = init_session(#{}, []),
    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, _Session1, _Events1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, [], false),

    timer:sleep(CCR_I_T_Delay),

    UsedCredits =
	[{1000, #{'CC-Input-Octets'  => [0],
		  'CC-Output-Octets' => [0],
		  'CC-Time'          => [60],
		  'CC-Total-Octets'  => [0],
		  'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 }],
    GyTerm = #{'Termination-Cause' =>
		   ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => UsedCredits},
    Result = case ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, [], false) of
		 {ok, _, _} -> ok;
		 {{error, rate_limit},_ ,_} -> rate_limit;
		 {Err, _, _} -> Err
	     end,
    set_test_info({session, SId}, Result).


%%%===================================================================
%%% Test server request handler override
%%%===================================================================

test_server_request(Packet, Svc, Peer, Extra) ->
    case get_test_info(diameter_test_request_action) of
	Fun when is_function(Fun) -> Fun([Packet, Peer]);
	_ -> ok
    end,
    diameter_test_server:handle_request(Packet, Svc, Peer, Extra).

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

set_test_info(Name, Value) ->
    ets:insert(?MODULE, {Name, Value}).

get_test_info(Name) ->
    case ets:lookup(?MODULE, Name) of
	[] -> undefined;
	[{_, Value}] -> Value
    end.

delete_test_info(Name) ->
    case ets:lookup(?MODULE, Name) of
	[] -> undefined;
	[{_, Value}] ->
	    ets:delete(?MODULE, Name),
	    Value
    end.

clean_test_info() ->
    ets:delete_all_objects(?MODULE).

list_test_info() ->
    ets:tab2list(?MODULE).

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
