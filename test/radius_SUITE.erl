%% Copyright 2017-2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(radius_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("ergw_aaa_test_lib.hrl").

-import(ergw_aaa_test_lib, [meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1,
			    set_cfg_value/3,
			    get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(HUT, ergw_aaa_radius).
-define(SERVICE, 'aaa-test').

-define(STATIC_CONFIG,
	[{'NAS-Identifier',        <<"NAS">>},
	 {'Framed-Protocol',       'PPP'},
	 {'Service-Type',          'Framed-User'}]).

-define(RADIUS_CONFIG,
	[{server, {{127,0,0,1}, 1812, <<"secret">>}}]).

-define(CONFIG,
	[{rate_limits,
	  [{default, [{outstanding_requests, 10}, {rate, 1000}]}]},
	 {functions, []},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_radius, ?RADIUS_CONFIG}
	  ]},
	 {services,
	  [{'Default', [{handler, 'ergw_aaa_static'}]},
	   {'RADIUS-Auth', [{handler, 'ergw_aaa_radius'},
			    {server, {{127,0,0,1}, 1812, <<"secret">>}}]},
	   {'RADIUS-Acct', [{handler, 'ergw_aaa_radius'},
			    {server, {{127,0,0,1}, 1813, <<"secret">>}}]}
	  ]},

	 {apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, ['RADIUS-Auth']},
			   {authorize,    ['RADIUS-Auth']},
			   {start,   ['RADIUS-Acct']},
			   {interim, ['RADIUS-Acct']},
			   {stop,    ['RADIUS-Acct']}
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
     accounting_async,
     attrs_3gpp].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],
    application:load(ergw_aaa),
    [application:set_env(ergw_aaa, Key, Opts) || {Key, Opts} <- ?CONFIG],
    eradius_test_handler:start(),

    meck_init(Config),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    Config.

end_per_suite(Config) ->
    meck_unload(Config),
    eradius_test_handler:stop(),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    ok.

init_per_testcase(accounting_async, Config) ->
    meck_reset(Config),
    meck:new(eradius_client, [passthrough, no_link]),
    Config;
init_per_testcase(_, Config) ->
    meck_reset(Config),
    Config.

end_per_testcase(accounting_async, _Config) ->
    meck:unload(eradius_client),
    ok;
end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

compat() ->
    [{doc, "Check that the old API is still working"}].
compat(Config) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?equal(ok, ergw_aaa_session:start(Session, #{})),
    ?equal(ok, ergw_aaa_session:interim(Session, #{})),
    ?equal(ok, ergw_aaa_session:stop(Session, #{})),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

simple() ->
    [{doc, "Simple NASREQ session"}].

simple(Config) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(
		      self(),
		      #{'Framed-IP-Address' => {10,10,10,10},
			'Framed-IPv6-Prefix' => {{16#fe80,0,0,0,0,0,0,0}, 64},
			'Framed-Pool' => <<"pool-A">>,
			'Framed-IPv6-Pool' => <<"pool-A">>}),

    {ok, _, Events} = ergw_aaa_session:invoke(Session, #{}, authenticate, []),

    ?match([{set, {{accounting, 'IP-CAN', periodic}, {periodic, 'IP-CAN', 1800, []}}}],
	   Events),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, authorize, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, start, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, interim, [])),
    TermOpts = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'},
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, TermOpts, stop, [])),

    meck_validate(Config),
    ok.

accounting() ->
    [{doc, "Check that we can successfully send ACR's and get ACA's"}].
accounting(Config) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

accounting_async() ->
    [{doc, "Check that Start / Stop msg work in async mode"}].
accounting_async(Config) ->
    eradius_test_handler:ready(),
    OrigApps = set_service_pars([{async, true}, {retries, 1}, {timeout, 1000}]),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    %% wait for async requests
    ct:sleep(5000),

    %% make sure nothing crashed
    meck_validate(Config),
    SR = lists:filter(
	   fun({_, {eradius_client, send_request,
		    [_,  #radius_request{cmd = accreq}, Opts]}, _}) ->
		   ?match(
		      #{retries := 1, timeout := 1000}, maps:from_list(Opts)),
		   true;
	      (_) -> false
	   end, meck:history(eradius_client)),
    ?equal(3, length(SR)),

    application:set_env(ergw_aaa, apps, OrigApps),
    ok.

attrs_3gpp() ->
    [{doc, "Check encoding of 3GPP attributes"}].
attrs_3gpp(Config) ->
    eradius_test_handler:ready(),

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

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), Attrs),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
%% set async modes and config eradius retries and timeouts for start / stop
set_service_pars(NewOpts) ->
    {ok, Apps0} = application:get_env(ergw_aaa, apps),
    Upd = fun(M) -> add_opts(M, NewOpts) end,
    Apps1 = maps_update_with([default, procedures, start], Upd, Apps0),
    Apps2 = maps_update_with([default, procedures, interim], Upd, Apps1),
    Apps = maps_update_with([default, procedures, stop], Upd, Apps2),
    application:set_env(ergw_aaa, apps, Apps),
    Apps0.

maps_update_with([Key], Fun, Map) ->
    maps:update_with(Key, Fun, Map);
maps_update_with([H|T], Fun, Map) ->
    maps:update_with(H, fun(M) -> maps_update_with(T, Fun, M) end, Map).

add_opts(Map, []) ->
    Map;
add_opts(Map, [{Par, Val}| T]) ->
    Map1 = lists:keymap(fun(X) -> maps:put(Par, Val, X) end, 2, Map),
    add_opts(Map1, T).
