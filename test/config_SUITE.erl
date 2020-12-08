%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(config_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(error_option(Config),
	?match({error,{_, _}}, (catch ergw_aaa_config:validate_config(Config)))).
	%% ?match({error,{options, _}}, (catch ergw_aaa_config:validate_config(Config)))).

-define(ok_option(Config),
	?match(#{}, ergw_aaa_config:validate_config(Config))).

-define(ok_set(Opt, Value), ?ok_option(set_cfg_value(Opt, Value, ?CONFIG))).
-define(error_set(Opt, Value), ?error_option(set_cfg_value(Opt, Value, ?CONFIG))).

-define(STATIC_CONFIG,
	[{'NAS-Identifier',        <<"NAS-Identifier">>},
	 {'Acct-Interim-Interval', 600},
	 {'Framed-Protocol',       'PPP'},
	 {'Service-Type',          'Framed-User'}]).

-define(RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING, #{
   normal => 1,
   administrative => 6,
   link_broken => 2,
   upf_failure => 9,
   remote_failure => 2,
   inactivity_timeout => 4,
   peer_restart => 7
}).

-define(RADIUS_AUTH_CONFIG,
	[{server, {{127,0,0,1}, 1812, <<"secret">>}},
	 {retries, 3}, {timeout, 5000},
	 {vendor_dicts, []}, {avp_filter, #{}},
	 {termination_cause_mapping, ?RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING}]).
-define(RADIUS_ACCT_CONFIG,
	[{server,    {{0,0,0,0,0,0,0,1}, 1813, <<"secret">>}},
	 {retries, 3}, {timeout, 5000},
	 {vendor_dicts, []}, {avp_filter, #{}},
	 {termination_cause_mapping, ?RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING}
	 ]).
-define(RADIUS_SERVICE_OPTS, []).

-define(DIAMETER_TRANSPORT,
	[
	 {connect_to, <<"aaa://127.0.0.1">>}
	]).
-define(DIAMETER_FUNCTION,
	{'diam-test',
	 [{handler,    ergw_aaa_diameter},
	  {'Origin-Host',       <<"127.0.0.1">>},
	  {'Origin-Realm',      <<"test-clnt.example.com">>},
	  {transports, [?DIAMETER_TRANSPORT]}
	 ]}).
-define(DIAMETER_CONFIG,
	[{function, 'diam-test'},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).
-define(DIAMETER_SERVICE_OPTS, []).

-define(RF_CONFIG,
	[{function, 'diam-test'},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).

-define(RO_CONFIG,
	[{function, 'diam-test'},
	 {'Destination-Realm', <<"test-srv.example.com">>}]).

-define(CONFIG,
	[{functions, [?DIAMETER_FUNCTION]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_radius, ?RADIUS_ACCT_CONFIG},
	   {ergw_aaa_nasreq, ?DIAMETER_CONFIG},
	   {ergw_aaa_rf, ?RF_CONFIG},
	   {ergw_aaa_ro, ?RF_CONFIG}
	  ]},

	 {services,
	  [{'Default',
	    [{handler, 'ergw_aaa_static'}]},
	   {'RADIUS-Service',
	    [{handler, 'ergw_aaa_radius'}]},
	   {'DIAMETER-Service',
	    [{handler, 'ergw_aaa_nasreq'}]},
	   {'RF-Service',
	    [{handler, 'ergw_aaa_rf'} | ?RF_CONFIG]},
	   {'RO-Service',
	    [{handler, 'ergw_aaa_ro'} | ?RO_CONFIG]}
	  ]},

	 {apps,
	  [{'RADIUS-Application',
	    [{session, ['Default']},
	     {procedures, [{authenticate, [{'RADIUS-Service', ?RADIUS_AUTH_CONFIG ++
						?RADIUS_SERVICE_OPTS},
					   {'Default', [{'TLS-Pre-Shared-Key', <<"secret">>}]}
					  ]},
			   {authorize, []},
			   {start, []},
			   {interim, []},
			   {stop, []}]}
	    ]},
	   {'DIAMETER-Application',
	    [{session, ['Default']},
	     {procedures, [{authenticate, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]},
			   {authorize, []},
			   {start, []},
			   {interim, []},
			   {stop, []}]}
	    ]}
	  ]}
	]).

%%%===================================================================
%%% Helpers
%%%===================================================================

match_map(Match, Map, File, Line) ->
    maps:fold(
      fun(Key, Expected, R) ->
	      case maps:is_key(Key, Map) of
		  true ->
		      Actual = maps:get(Key, Map),
		      case erlang:match_spec_test({Actual, ok}, [{{Expected, '$1'}, [], ['$1']}], table) of
			  {ok, ok, _, _} ->
			      R andalso true;
			  {ok, false, _, _} ->
			      ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
				     [File, Line, Key, Expected, Actual]),
			      false
		      end;
		  _ ->
		      ct:pal("MAP KEY MISSING(~s:~b, ~s)~n", [File, Line, Key]),
		      false
	      end
      end, true, Match) orelse error(badmatch),
    ok.

-define(match_map(Expected, Actual), match_map(Expected, Actual, ?FILE, ?LINE)).

%%%===================================================================
%%% Tests + API
%%%===================================================================

all() ->
    [config].

config() ->
    [{doc, "Test the config validation"}].
config(_Config)  ->
    ?ok_option(?CONFIG),
    ValidatedCfg = ergw_aaa_config:validate_config(?CONFIG),
    ?ok_option(ValidatedCfg),
    ?ok_set([vsn], "1.0.0"),

    ?ok_set([product_name], "PRODUCT"),
    ?ok_set([product_name], <<"PRODUCT">>),
    ?error_set([product_name], 1),

    ?ok_set([rate_limits, default], [{outstanding_requests, 50}, {rate, 50}]),
    ?ok_set([rate_limits, default], [{outstanding_requests, 50}]),
    ?ok_set([rate_limits, default], [{rate, 50}]),
    ?error_set([rate_limits, default], [{outstanding_requests, atom}, {rate, 50}]),
    ?error_set([rate_limits, default], [{outstanding_requests, 50}, {rate, atom}]),
    ?error_set([rate_limits, default], [{outstanding_requests, 50}, {rate, -1}]),
    ?error_set([rate_limits, default], [{outstanding_requests, 50}, {rate, 200000}]),
    ?error_set([rate_limits, default], [{outstanding_requests, 0}, {rate, 50}]),

    ?error_set([handlers], invalid),
    ?error_set([handlers, invalid_handler], []),
    ?error_set([services], invalid),

    %% make sure the handler config is passed through to the service
    DefCfgMap = maps:from_list([{handler, 'ergw_aaa_static'} | ?STATIC_CONFIG]),
    ?equal(DefCfgMap, get_cfg_value([services, 'Default'], ValidatedCfg)),
    %% make sure the handler config is also passed through to the session
    ?equal(maps:from_list(?STATIC_CONFIG),
	   get_cfg_value([apps, 'RADIUS-Application', session, 'Default'], ValidatedCfg)),

    %% ?error_set([handlers, ergw_aaa_static, invalid_option], []),
    %% ?error_set([handlers, ergw_aaa_static, shared_secret], invalid_secret),
    ?ok_set([handlers, ergw_aaa_static], []),
    %% ?ok_set([handlers, ergw_aaa_static, shared_secret], <<"secret">>),

    ?error_set([handlers, ergw_aaa_radius], []),
    ?error_set([handlers, ergw_aaa_radius, invalid_option], []),
    ?error_set([handlers, ergw_aaa_radius, server], invalid_id),
    ?error_set([handlers, ergw_aaa_radius, server], invalid_id),
    ?error_set([handlers, ergw_aaa_radius, server],
	       {"undefined.example.net",1812,<<"secret">>}),
    ?error_set([handlers, ergw_aaa_radius, server],
	       {invalid_ip,1812,<<"secret">>}),
    ?error_set([handlers, ergw_aaa_radius, server],
	       {{127,0,0,1},invalid_port,<<"secret">>}),
    ?error_set([handlers, ergw_aaa_radius, server],
	       {{127,0,0,1},1812,invalid_secret}),
    ?error_set([handlers, ergw_aaa_radius, async], invalid),
    ?error_set([handlers, ergw_aaa_radius, retries], invalid),
    ?error_set([handlers, ergw_aaa_radius, timeout], invalid),

    ?ok_set([handlers, ergw_aaa_radius, server],
	    {"localhost",1812,<<"secret">>}),
    ?ok_set([handlers, ergw_aaa_radius, async], true),
    ?ok_set([handlers, ergw_aaa_radius, async], false),
    ?ok_set([handlers, ergw_aaa_radius, retries], 1),
    ?ok_set([handlers, ergw_aaa_radius, timeout], 1000),

    ?ok_set([handlers, ergw_aaa_radius, vendor_dicts], []),
    ?ok_set([handlers, ergw_aaa_radius, vendor_dicts], [ituma]),
    ?ok_set([handlers, ergw_aaa_radius, vendor_dicts], [52315]),
    ?error_set([handlers, ergw_aaa_radius, vendor_dicts], [atom]),
    ?error_set([handlers, ergw_aaa_radius, vendor_dicts], atom),
    ?error_set([handlers, ergw_aaa_radius, vendor_dicts], 52315),
    ?error_set([handlers, ergw_aaa_radius, vendor_dicts], ituma),

    ?error_set([services, 'RADIUS-Service', handler], invalid_handler),

    ?error_set([services, 'RADIUS-Test-Service'], ?RADIUS_AUTH_CONFIG),
    ?ok_set([services, 'RADIUS-Test-Service'], [{handler, 'ergw_aaa_radius'}
						| ?RADIUS_AUTH_CONFIG]),

    %% make sure the handler config is passed through to the service
    RadCfgMap = maps:from_list([{handler, 'ergw_aaa_radius'} | ?RADIUS_ACCT_CONFIG]),
    ?equal(RadCfgMap, get_cfg_value([services, 'RADIUS-Service'], ValidatedCfg)),
    %% make sure the handler config is also passed through to the session
    ?equal(maps:from_list(?RADIUS_SERVICE_OPTS ++ ?RADIUS_AUTH_CONFIG),
	   get_cfg_value([apps, 'RADIUS-Application', procedures,
			  authenticate, 'RADIUS-Service'], ValidatedCfg)),

    ?error_set([functions, 'diam-test'], []),
    ?error_set([functions, 'diam-test', invalid_option], []),
    ?error_set([functions, 'diam-test', 'Origin-Host'], invalid_host),
    ?error_set([functions, 'diam-test', 'Origin-Host'], <<"undefined.example.net">>),
    ?error_set([functions, 'diam-test', 'Origin-Realm'], invalid_realm),

    ?error_set([functions, 'diam-test', transports], []),
    ?error_set([functions, 'diam-test', transports], {}),
    ?error_set([functions, 'diam-test', transports, 1, connect_to], invalid_uri),
    ?error_set([functions, 'diam-test', transports, 1, connect_to], <<"http://example.com:12345">>),

    % transport options
	?ok_set([functions, 'diam-test', transports, 1, fragment_timer], 200),
	?ok_set([functions, 'diam-test', transports, 1, recbuf], 424242),
    ?ok_set([functions, 'diam-test', transports, 1, sndbuf], 424242),
    ?ok_set([functions, 'diam-test', transports, 1, unordered], false),
    ?ok_set([functions, 'diam-test', transports, 1, reuseaddr], false),
    ?ok_set([functions, 'diam-test', transports, 1, nodelay], false),
	?error_set([functions, 'diam-test', transports, 1, fragment_timer], invalid),
    ?error_set([functions, 'diam-test', transports, 1, recbuf], invalid),
    ?error_set([functions, 'diam-test', transports, 1, recbuf], 13),
    ?error_set([functions, 'diam-test', transports, 1, sndbuf], invalid),
    ?error_set([functions, 'diam-test', transports, 1, sndbuf], 13),
    ?error_set([functions, 'diam-test', transports, 1, unordered], invalid),
    ?error_set([functions, 'diam-test', transports, 1, reuseaddr], invalid),
    ?error_set([functions, 'diam-test', transports, 1, nodelay], invalid),

    ?error_set([handlers, ergw_aaa_nasreq], []),
    ?error_set([handlers, ergw_aaa_nasreq, invalid_option], []),
    ?error_set([handlers, ergw_aaa_nasreq, accounting], []),
    ?error_set([handlers, ergw_aaa_nasreq, avp_filter], invalid),
    ?ok_set([handlers, ergw_aaa_nasreq, accounting], split),
    ?ok_set([handlers, ergw_aaa_nasreq, accounting], coupled),
    ?ok_set([handlers, ergw_aaa_nasreq, avp_filter], ['3GPP-IMSI', ['3GPP-MSISDN']]),

    ?error_set([services, 'DIAMETER-Service', handler], invalid_handler),

    ?error_set([services, 'DIAMETER-Test-Service'], ?DIAMETER_CONFIG),
    ?ok_set([services, 'DIAMETER-Test-Service'], [{handler, 'ergw_aaa_nasreq'}
						  | ?DIAMETER_CONFIG]),

    %% make sure the handler config is passed through to the service
    ?equal(proplists:get_value(function, ?DIAMETER_CONFIG),
	   get_cfg_value([services, 'DIAMETER-Service', function], ValidatedCfg)),
    %% make sure the handler config is also passed through to the session
    ?match_map(maps:from_list(?DIAMETER_CONFIG ++ ?DIAMETER_SERVICE_OPTS),
	       get_cfg_value([apps, 'DIAMETER-Application', procedures,
			      authenticate, 'DIAMETER-Service'], ValidatedCfg)),

    ?error_set([handlers, ergw_aaa_rf], []),
    ?error_set([handlers, ergw_aaa_rf, invalid_option], []),

    % termination cause mapping tests config
    ?ok_set([handlers, ergw_aaa_nasreq, termination_cause_mapping], [{normal, 1}]),
    ?error_set([handlers, ergw_aaa_nasreq, termination_cause_mapping], invalid),
    ?ok_set([handlers, ergw_aaa_radius, termination_cause_mapping], [{normal, 1}]),
    ?error_set([handlers, ergw_aaa_radius, termination_cause_mapping], invalid),
    ?ok_set([handlers, ergw_aaa_rf, termination_cause_mapping], [{normal, 1}]),
    ?error_set([handlers, ergw_aaa_rf, termination_cause_mapping], invalid),
    ?ok_set([handlers, ergw_aaa_ro, termination_cause_mapping], [{normal, 1}]),
    ?error_set([handlers, ergw_aaa_ro, termination_cause_mapping], invalid),
    ok.
