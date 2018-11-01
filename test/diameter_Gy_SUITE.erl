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
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/ergw_aaa_session.hrl").

-import(diameter_test_server, [get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(SERVICE, 'diam-test').

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

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
	  {'Origin-Host', ?'Origin-Host'},
	  {'Origin-Realm', ?'Origin-Realm'},
	  {transports, [?DIAMETER_TRANSPORT]}
	 ]}).
-define(DIAMETER_RO_CONFIG,
	[{function, ?SERVICE},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).
-define(DIAMETER_SERVICE_OPTS, []).

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
			   {{gy, 'CCR-Initial'},   ['Ro']},
			   {{gy, 'CCR-Update'},    ['Ro']},
			   {{gy, 'CCR-Terminate'}, ['Ro']}
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
    [simple_session, abort_session_request].

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
	  '3GPP-Charging-Characteristics' =>      <<8,0>>
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
    ?equal(2, proplists:get_value({{4, 272, 0}, recv, {'Result-Code',2001}}, Stats1)),

    ok.
