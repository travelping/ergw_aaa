%% Copyright 2017-2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_nasreq_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("ergw_aaa_test_lib.hrl").

-import(ergw_aaa_test_lib, [meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1,
			    set_cfg_value/3,
			    get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(HUT, ergw_aaa_nasreq).
-define(SERVICE, 'diam-test').

-define(STATIC_CONFIG,
	[{'NAS-Identifier',        <<"NAS">>},
	 {'Framed-Protocol',       'PPP'},
	 {'Service-Type',          'Framed-User'}]).

-define(DIAMETER_TRANSPORT,
	[
	 {connect_to, <<"aaa://127.0.0.1">>}
	]).
-define(DIAMETER_FUNCTION,
	{?SERVICE,
	 [{handler, ergw_aaa_diameter},
	  {'Origin-Host', <<"127.0.0.1">>},
	  {'Origin-Realm', <<"example.com">>},
	  {transports, [?DIAMETER_TRANSPORT]}
	 ]}).
-define(DIAMETER_CONFIG,
	[{function, ?SERVICE},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).

-define(CONFIG,
	[{rate_limits,
	  [{default, [{outstanding_requests, 10}, {rate, 1000}]}]},
	 {functions, [?DIAMETER_FUNCTION]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_nasreq, ?DIAMETER_CONFIG}
	  ]},
	 {services,
	  [{'Default', [{handler, 'ergw_aaa_static'}]},
	   {'NASREQ',  [{handler, 'ergw_aaa_nasreq'}]}
	  ]},

	 {apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, ['NASREQ']},
			   {authorize, ['NASREQ']},
			   {start, ['NASREQ']},
			   {interim, ['NASREQ']},
			   {stop, ['NASREQ']}
			  ]}
	    ]}
	  ]}
	]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [compat,
     simple,
     accounting,
     acct_interim_interval,
     attrs_3gpp].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],
    application:load(ergw_aaa),
    [application:set_env(ergw_aaa, Key, Opts) || {Key, Opts} <- ?CONFIG],

    meck_init(Config),

    diameter_test_server:start_nasreq(),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    lager_common_test_backend:bounce(debug),

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

init_per_testcase(Config) ->
    meck_reset(Config),
    Config.

end_per_testcase(_Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

compat() ->
    [{doc, "Check that the old API is still working"}].
compat(Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?equal(ok, ergw_aaa_session:start(Session, #{})),
    ?equal(ok, ergw_aaa_session:interim(Session, #{})),
    ?equal(ok, ergw_aaa_session:stop(Session, #{})),
    ct:sleep(100),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    ct:pal("Statistics: ~p", [Statistics]),
    [?equal(Cnt, stats(Msg, Config, Statistics)) ||
	{Cnt, Msg} <- [{3, 'ACR'}, {3, {'ACA', 2001}}]],

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

simple() ->
    [{doc, "Simple NASREQ session"}].

simple(Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),

    {ok, _, Events} = ergw_aaa_session:invoke(Session, #{}, authenticate, []),
    ?match([{set, {{ergw_aaa_nasreq, 'IP-CAN', periodic}, {periodic, 'IP-CAN', 1800, []}}}],
	   Events),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, authorize, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, start, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, interim, [])),
    TermOpts = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'},
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, TermOpts, stop, [])),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    ct:pal("Statistics: ~p", [Statistics]),
    [?equal(Cnt, stats(Msg, Config, Statistics)) ||
	{Cnt, Msg} <- [{1, 'AAR'}, {1, {'AAA', 2001}},
		       {3, 'ACR'}, {3, {'ACA', 2001}},
		       {1, 'STR'}, {1, {'STA', 2001}}
		      ]],
    meck_validate(Config),
    ok.

accounting() ->
    [{doc, "Check that we can successfully send ACR's and get ACA's"}].
accounting(Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    ct:pal("Statistics: ~p", [Statistics]),
    % check that client has sent ACR/ACA
    [?equal(Cnt, stats(Msg, Config, Statistics)) ||
	{Cnt, Msg} <- [{3, 'ACR'}, {3, {'ACA', 2001}}]],

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

acct_interim_interval() ->
    [{doc, "test diameter provider can reset interim interval"
      "by data from ACA Acct-Interim-Interval"}].
acct_interim_interval(Config) ->
    Fun = fun(_, S) -> S end,
    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'Accouting-Update-Fun' => Fun}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    StartRes = ergw_aaa_session:start(Session, #{}, []),
    ?match({ok, _, _}, StartRes),

    {_, SessionOpts, Ev} = StartRes,
    ?match(#{'Session-Id' := _,
	     'Diameter-Session-Id' := _,
	     'Service-Type' := 'Framed-User',
	     'Framed-Protocol' := 'PPP'
	    }, SessionOpts),
    ?match([{set, {{ergw_aaa_nasreq, 'IP-CAN', periodic},
		   {periodic, 'IP-CAN', 1, _}}}], Ev),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

attrs_3gpp() ->
    [{doc, "Check encoding of 3GPP attributes"}].
attrs_3gpp(Config) ->
    Attrs = #{
	      '3GPP-GGSN-Address'       => {199,255,4,125},
	      '3GPP-IMEISV'             => <<82,21,50,96,32,80,30,0>>,
	      '3GPP-IMSI'               => <<"250071234567890">>,
	      '3GPP-Charging-Id'        => 3604013806,
	      '3GPP-IMSI-MCC-MNC'       => <<"25999">>,
	      '3GPP-GGSN-MCC-MNC'       => <<"25888">>,
	      '3GPP-MS-TimeZone'        => {128,1},
	      '3GPP-NSAPI'              => 5,
	      '3GPP-PDP-Type'           => 'IPv4',
	      '3GPP-RAT-Type'           => 6,
	      '3GPP-SGSN-Address'       => {192,168,1,1},
	      '3GPP-SGSN-MCC-MNC'       => <<"26201">>,
	      '3GPP-SGSN-IPv6-Address'  => {16#fd96, 16#dcd2, 16#efdb, 16#41c4, 0, 0, 0, 16#1000},
	      '3GPP-GGSN-IPv6-Address'  => {16#fd96, 16#dcd2, 16#efdb, 16#41c4, 0, 0, 0, 16#2000},
	      '3GPP-Selection-Mode'     => 0,
	      '3GPP-User-Location-Info' => <<24,98,242,16,64,163,98,242,16,1,156,232,0>>,
	      'Called-Station-Id'       => <<"some.station.gprs">>,
	      'Calling-Station-Id'      => <<"543148000012345">>,
	      'Framed-IP-Address'       => {0,0,0,0},
	      'Framed-Protocol'         => 'GPRS-PDP-Context',
	      'Multi-Session-Id'        => 1012552258277823040188863251876666193415858290601,
	      'Password'                => <<"ergw">>,
	      'Service-Type'            => 'Framed-User',
	      'Session-Id'              => 1012552258277823040188863251876666193415858290601,
	      'Username'                => <<"ergw">>
	     },

    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), Attrs),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),

    Stats1 = diff_stats(Stats0, get_stats(?SERVICE)),
    ?equal(1, stats({'ACA', 2001}, Config, Stats1)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

stats('AAR', _, Stats) ->
    proplists:get_value({{1, 265, 1}, send}, Stats);
stats({'AAA', RC}, _, Stats) ->
    proplists:get_value({{1, 265, 0}, recv, {'Result-Code', RC}}, Stats);
stats('STR', _, Stats) ->
    proplists:get_value({{1, 275, 1}, send}, Stats);
stats({'STA', RC}, _, Stats) ->
    proplists:get_value({{1, 275, 0}, recv, {'Result-Code', RC}}, Stats);
stats('ACR', _, Stats) ->
    proplists:get_value({{1, 271, 1}, send}, Stats);
stats({'ACA', RC}, _, Stats) ->
    proplists:get_value({{1, 271, 0}, recv, {'Result-Code', RC}}, Stats).
