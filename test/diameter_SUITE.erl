%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-import(diameter_test_server, [get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(SERVICE, 'diam-test').

-define(STATIC_CONFIG,
	[{'NAS-Identifier',        <<"NAS">>},
	 {'Framed-Protocol',       'PPP'},
	 {'Service-Type',          'Framed-User'}]).

-define(DIAMETER_TRANSPORT,
	{?SERVICE,
	 [{handler, ergw_aaa_diameter},
	  {'Origin-Host', <<"127.0.0.1">>},
	  {'Origin-Realm', <<"example.com">>},
	  {connect_to, <<"aaa://127.0.0.1">>}
	 ]}).
-define(DIAMETER_CONFIG,
	[{transport, ?SERVICE}]).
-define(DIAMETER_SERVICE_OPTS, []).

-define(CONFIG,
	[{transports, [?DIAMETER_TRANSPORT]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_nasreq, ?DIAMETER_CONFIG}
	  ]},
	 {services,
	  [{'Default',
	    [{handler, 'ergw_aaa_static'}]},
	   {'DIAMETER-Service',
	    [{handler, 'ergw_aaa_nasreq'}]}
	  ]},

	 {apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, []},
			   {authorize, []},
			   {start, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]},
			   {interim, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]},
			   {stop, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]}
			  ]}
	    ]}
	  ]}
	]).

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
	 (Expected@@@, Actual@@@) ->
	     ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
		    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
	     false
     end)(Expected, Actual) orelse error(badmatch)).

-define(match(Guard, Expr),
	((fun () ->
		  case (Expr) of
		      Guard -> ok;
		      V -> ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
				   [?FILE, ?LINE, ??Expr, ??Guard, V]),
			    error(badmatch)
		  end
	  end)())).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [compat,
     accounting,
     acct_interim_interval,
     attrs_3gpp].

init_per_suite(Config) ->
    application:load(ergw_aaa),
    [application:set_env(ergw_aaa, Key, Opts) || {Key, Opts} <- ?CONFIG],

    diameter_test_server:start(),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    lager_common_test_backend:bounce(debug),

    case diameter_test_server:wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    {skip, Other}
    end.

end_per_suite(_Config) ->
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

compat() ->
    [{doc, "Check that the old API is still working"}].
compat(_Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?equal(ok, ergw_aaa_session:start(Session, #{})),
    ?equal(ok, ergw_aaa_session:interim(Session, #{})),
    ?equal(ok, ergw_aaa_session:stop(Session, #{})),
    ergw_aaa_session:sync(Session),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    ct:pal("Statistics: ~p", [Statistics]),
    % check that client has sent ACR
    ?equal(3, proplists:get_value({{1, 271, 1}, send}, Statistics)),
    % check that client has received ACA
    ?equal(3, proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, Statistics)),
    ok.

accounting() ->
    [{doc, "Check that we can successfully send ACR's and get ACA's"}].
accounting(_Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(),
						     #{'Framed-IP-Address' => {10,10,10,10}}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),
    ?match({ok, _, _}, ergw_aaa_session:start(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:interim(Session, #{}, [])),
    ?match({ok, _, _}, ergw_aaa_session:stop(Session, #{}, [])),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    ct:pal("Statistics: ~p", [Statistics]),
    % check that client has sent ACR
    ?equal(3, proplists:get_value({{1, 271, 1}, send}, Statistics)),
    % check that client has received ACA
    ?equal(3, proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, Statistics)),
    ok.

acct_interim_interval() ->
    [{doc, "test diameter provider can reset interim interval"
      "by data from ACA Acct-Interim-Interval"}].
acct_interim_interval(_Config) ->
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
    ?match([{set,{{ergw_aaa_nasreq,'IP-CAN',time},
		  {time,'IP-CAN',1000,[recurring]}}}], Ev),
    ok.

attrs_3gpp() ->
    [{doc, "Check encoding of 3GPP attributes"}].
attrs_3gpp(_Config) ->
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
    ?equal(1, proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, Stats1)),

    ok.
