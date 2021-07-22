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
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_ituma.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("gtplib/include/gtp_packet.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(HUT, ergw_aaa_radius).
-define(SERVICE, <<"aaa-test">>).

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

-define(CONFIG,
	#{rate_limits =>
	      #{default => #{outstanding_requests => 50, rate => 1000}},
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_radius => ?RADIUS_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
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
		    #{init         => [#{service => <<"Default">>}],
		      authenticate => [#{service => <<"RADIUS-Auth">>}],
		      authorize    => [#{service => <<"RADIUS-Auth">>}],
		      start        => [#{service => <<"RADIUS-Acct">>}],
		      interim      => [#{service => <<"RADIUS-Acct">>}],
		      stop         => [#{service => <<"RADIUS-Acct">>}]}
	       }
	 }).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [compat,
     simple,
     simple_normal_terminate,
     accounting,
     accounting_async,
     attrs_3gpp,
     avp_filter,
     vendor_dicts,
     terminate].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],
    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),
    eradius_test_handler:start(),

    meck_init(Config),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    ergw_aaa_test_lib:ergw_aaa_init(?CONFIG),

    Config.

end_per_suite(Config) ->
    meck_unload(Config),
    eradius_test_handler:stop(),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    ok.

init_per_testcase(accounting_async, Config) ->
    meck_reset(Config),
    check_stats_per_testcase(),
    meck:new(eradius_client, [passthrough, no_link]),
    Config;
init_per_testcase(_, Config) ->
    meck_reset(Config),
    check_stats_per_testcase(),
    Config.

end_per_testcase(accounting_async, _Config) ->
    reset_session_stats(),
    check_stats_per_testcase(),
    meck:unload(eradius_client),
    ok;
end_per_testcase(_, _Config) ->
    check_stats_per_testcase(),
    ok.

check_stats_per_testcase() ->
    case get_session_stats() of
	[] -> ok;
	[{ergw_aaa_radius, started, 0}] -> ok;
	Other -> ct:fail(Other)
    end.

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
    simple(Config, #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'}).

simple_normal_terminate() ->
    [{doc, "Simple terminate NASREQ session with `normal` atom"}].

simple_normal_terminate(Config) ->
    simple(Config, #{'Termination-Cause' => "User Request"}).

accounting() ->
    [{doc, "Check that we can successfully send ACR's and get ACA's"}].
accounting(Config) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

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

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

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
	      '3GPP-IMSI-MCC-MNC'       => {<<"259">>,<<"99">>},
	      '3GPP-GGSN-MCC-MNC'       => {<<"258">>,<<"88">>},
	      '3GPP-MS-TimeZone'        => {128,1},
	      '3GPP-NSAPI'              => 5,
	      '3GPP-PDP-Type'           => 'IPv4',
	      '3GPP-RAT-Type'           => 6,
	      '3GPP-SGSN-Address'       => {192,168,1,1},
	      '3GPP-SGSN-MCC-MNC'       => {<<"262">>,<<"01">>},
	      '3GPP-SGSN-IPv6-Address'  => {16#fd96, 16#dcd2, 16#efdb, 16#41c4, 0, 0, 0, 16#1000},
	      '3GPP-GGSN-IPv6-Address'  => {16#fd96, 16#dcd2, 16#efdb, 16#41c4, 0, 0, 0, 16#2000},
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

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, stop, [])),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

avp_filter() ->
    [{doc, "AVP filter"}].
avp_filter(Config) ->
    eradius_test_handler:ready(),

    %% turn avp_filter into internal format...
    Opts = ergw_aaa_radius:validate_procedure(default, all, all,
					      [{avp_filter, [?Framed_IPv6_Pool ]}], #{}),
    OrigApps = set_service_pars(Opts),

    {ok, Session} = ergw_aaa_session_sup:new_session(
		      self(),
		      #{'Username' => <<"AVP-Filter">>,
			'Framed-IP-Address' => {10,10,10,10},
			'Framed-IPv6-Prefix' => {{16#fe80,0,0,0,0,0,0,0}, 64},
			'Framed-Pool' => <<"pool-A">>,
			'Framed-IPv6-Pool' => <<"pool-A">>}),

    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, authenticate, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, stop, [])),

    meck_validate(Config),
    application:set_env(ergw_aaa, apps, OrigApps),
    ok.

vendor_dicts() ->
    [{doc, "Vendor Dicts"}].
vendor_dicts(Config) ->
    eradius_test_handler:ready(),

    %% turn avp_filter into internal format...
    Opts = ergw_aaa_radius:validate_procedure(default, all, all,
					      [{vendor_dicts, [?'Ituma']}], #{}),
    OrigApps = set_service_pars(Opts),

    {ok, Session} = ergw_aaa_session_sup:new_session(
		      self(),
		      #{'Username' => <<"Vendor-Dicts">>,
			'BSSID' => "08-08-08-08-08-08",
			'CAPWAP-GPS-Altitude' => "62.4",
			'CAPWAP-GPS-Hdop' => "0.7",
			'CAPWAP-GPS-Latitude' => "5207.6688N",
			'CAPWAP-GPS-Longitude' => "01137.8028E",
			'CAPWAP-GPS-Timestamp' => "2014-09-03T15:47:50.000Z",
			'CAPWAP-Session-Id' =>
			    <<143,196,99,94,218,207,226,25,122,187,116,116,38,124,132,10>>,
			'Calling-Station-Id' => "01-02-03-04-05-06",
			'Framed-Protocol' => 'TP-CAPWAP',
			'Location-Id' => <<"654321">>,
			'MAC' => <<1,2,3,4,5,6>>,
			'SSID' => <<"DEV CAPWAP WIFI">>,
			'Service-Type' => 'TP-CAPWAP-STA',
			'Tunnel-Client-Endpoint' => <<"127.0.0.1">>,
			'Tunnel-Medium-Type' => 'IPv4',
			'Tunnel-Type' => 'CAPWAP',
			'WLAN-AKM-Suite' => 'PSK',
			'WLAN-Authentication-Mode' => secured,
			'WLAN-Group-Cipher' => 'CCMP',
			'WLAN-Group-Mgmt-Cipher' => undefined,
			'WLAN-Pairwise-Cipher' => 'CCMP'}),

    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, authenticate, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, stop, [])),

    meck_validate(Config),
    application:set_env(ergw_aaa, apps, OrigApps),
    ok.

terminate() ->
    [{doc, "Simulate unexpected owner termiantion"}].
terminate(Config) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(
		      self(),
		      #{'Framed-IP-Address' => {10,10,10,10},
			'Framed-IPv6-Prefix' => {{16#fe80,0,0,0,0,0,0,0}, 64},
			'Framed-Pool' => <<"pool-A">>,
			'Framed-IPv6-Pool' => <<"pool-A">>,
			'Framed-Interface-Id' => {0,0,0,0,0,0,0,1}}),

    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, start, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match(ok, ergw_aaa_session:terminate(Session)),
    wait_for_session(ergw_aaa_radius, started, 0, 10),

    meck_validate(Config),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
%% set async modes and config eradius retries and timeouts for start / stop
set_service_pars(NewOpts) ->
    {ok, Apps0} = application:get_env(ergw_aaa, apps),
    Upd = fun(M) -> add_opts(M, NewOpts) end,
    Apps =
	lists:foldl(
	  fun(P, A) ->
		  maps_update_with([default, P], Upd, A)
	  end, Apps0, [authenticate, start, interim, stop]),
    application:set_env(ergw_aaa, apps, Apps),
    Apps0.

maps_update_with([Key], Fun, Map) ->
    maps:update_with(Key, Fun, Map);
maps_update_with([H|T], Fun, Map) ->
    maps:update_with(H, fun(M) -> maps_update_with(T, Fun, M) end, Map).

add_opts(Map, Opts) when is_map(Opts) ->
    lists:map(fun(X) -> maps:merge(X, Opts) end, Map);
add_opts(Map, []) ->
    Map;
add_opts(Map, [{Par, Val}| T]) ->
    Map1 = lists:map(fun(X) -> maps:put(Par, Val, X) end, Map),
    add_opts(Map1, T).

simple(Config, Opts) ->
    eradius_test_handler:ready(),

    {ok, Session} = ergw_aaa_session_sup:new_session(
		      self(),
		      #{'Framed-IP-Address' => {10,10,10,10},
			'Framed-IPv6-Prefix' => {{16#fe80,0,0,0,0,0,0,0}, 64},
			'Framed-Pool' => <<"pool-A">>,
			'Framed-IPv6-Pool' => <<"pool-A">>,
			'Framed-Interface-Id' => {0,0,0,0,0,0,0,1}}),

    {ok, SOut, Events} = ergw_aaa_session:invoke(Session, #{}, authenticate, []),
    ?match(#{'MS-Primary-DNS-Server' := {8,8,8,8}}, SOut),

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

    ?match([{set, {{accounting, 'IP-CAN', periodic}, {periodic, 'IP-CAN', 1800, []}}}],
	   Events),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, authorize, [])),
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, start, [])),

    ?equal([{ergw_aaa_radius, started, 1}], get_session_stats()),

    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, #{}, interim, [])),
    TermOpts = #{'Termination-Cause' => maps:get(reason, Opts, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT')},
    ?match({ok, _, _}, ergw_aaa_session:invoke(Session, TermOpts, stop, [])),

    ?equal([{ergw_aaa_radius, started, 0}], get_session_stats()),

    meck_validate(Config),
    ok.
