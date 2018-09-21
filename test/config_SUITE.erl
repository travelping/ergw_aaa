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

-define(RADIUS_AUTH_CONFIG,
	[{server, {{127,0,0,1}, 1812, <<"secret">>}}]).
-define(RADIUS_ACCT_CONFIG,
	[{server,    {{0,0,0,0,0,0,0,1}, 1813, <<"secret">>}}]).
-define(RADIUS_SERVICE_OPTS, []).

-define(DIAMETER_TRANSPORT,
	{'diam-test',
	 [{handler,    ergw_aaa_diameter},
	  {'Origin-Host',       <<"127.0.0.1">>},
	  {'Origin-Realm',      <<"example.com">>},
	  {connect_to, <<"aaa://127.0.0.1:3868">>}
	 ]}).
-define(DIAMETER_CONFIG,
	[{transport, 'diam-test'}]).
-define(DIAMETER_SERVICE_OPTS, []).

-define(RF_CONFIG,
	[{transport, 'diam-test'}]).

-define(CONFIG,
	[{transports, [?DIAMETER_TRANSPORT]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_radius, ?RADIUS_ACCT_CONFIG},
	   {ergw_aaa_nasreq, ?DIAMETER_CONFIG},
	   {ergw_aaa_rf, ?RF_CONFIG}
	  ]},

	 {services,
	  [{'Default',
	    [{handler, 'ergw_aaa_static'}]},
	   {'RADIUS-Service',
	    [{handler, 'ergw_aaa_radius'}]},
	   {'DIAMETER-Service',
	    [{handler, 'ergw_aaa_nasreq'}]},
	   {'RF-Service',
	    [{handler, 'ergw_aaa_rf'} | ?RF_CONFIG]}
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
%%% Config manipulation
%%%===================================================================

set_cfg_value(Key, Value) when is_function(Value) ->
    Value(Key);
set_cfg_value(Key, Value) ->
    {Key, Value}.

set_cfg_value([Key], Value, Config) when is_boolean(Value) ->
    lists:keystore(Key, 1, proplists:delete(Key, Config), set_cfg_value(Key, Value));
set_cfg_value([{Key, Pos}], Value, Config) ->
    Tuple = lists:keyfind(Key, 1, Config),
    lists:keystore(Key, 1, Config, setelement(Pos, Tuple, set_cfg_value(Key, Value)));
set_cfg_value([Key], Value, Config) ->
    lists:keystore(Key, 1, Config, set_cfg_value(Key, Value));
set_cfg_value([{Key, Pos} | T], Value, Config) ->
    Tuple = lists:keyfind(Key, 1, Config),
    lists:keystore(Key, 1, Config,
		   setelement(Pos, Tuple, set_cfg_value(T, Value, element(Pos, Tuple))));
set_cfg_value([Pos | T], Value, Config)
  when is_integer(Pos), is_tuple(Config) ->
    setelement(Pos, Config, set_cfg_value(T, Value, element(Pos, Config)));
set_cfg_value([H | T], Value, Config) ->
    Prop = proplists:get_value(H, Config, []),
    lists:keystore(H, 1, Config, {H, set_cfg_value(T, Value, Prop)}).

add_cfg_value([Key], Value, Config) ->
    ct:pal("Cfg: ~p", [[{Key, Value} | Config]]),
    [{Key, Value} | Config];
add_cfg_value([H | T], Value, Config) ->
    Prop = proplists:get_value(H, Config, []),
    lists:keystore(H, 1, Config, {H, add_cfg_value(T, Value, Prop)}).

get_opt(Key, List) when is_list(List) ->
    proplists:get_value(Key, List);
get_opt(Key, Map) when is_map(Map) ->
    maps:get(Key, Map).

get_cfg_value([Key], Config) ->
    get_opt(Key, Config);
get_cfg_value([H|T], Config) ->
    get_cfg_value(T, get_opt(H, Config)).

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

    ?ok_set([handlers, ergw_aaa_radius, server],
	    {"localhost",1812,<<"secret">>}),

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

    ?error_set([transports, 'diam-test'], []),
    ?error_set([transports, 'diam-test', invalid_option], []),
    ?error_set([transports, 'diam-test', 'Origin-Host'], invalid_host),
    ?error_set([transports, 'diam-test', 'Origin-Realm'], invalid_realm),
    ?error_set([transports, 'diam-test', connect_to], invalid_uri),
    ?error_set([transports, 'diam-test', 'Origin-Host'], <<"undefined.example.net">>),
    ?error_set([transports, 'diam-test', connect_to], <<"http://example.com:12345">>),

    ?error_set([handlers, ergw_aaa_nasreq], []),
    ?error_set([handlers, ergw_aaa_nasreq, invalid_option], []),

    ?error_set([services, 'DIAMETER-Service', handler], invalid_handler),

    ?error_set([services, 'DIAMETER-Test-Service'], ?DIAMETER_CONFIG),
    ?ok_set([services, 'DIAMETER-Test-Service'], [{handler, 'ergw_aaa_nasreq'}
						  | ?DIAMETER_CONFIG]),

    %% make sure the handler config is passed through to the service
    ?equal(proplists:get_value(transport, ?DIAMETER_CONFIG),
	   get_cfg_value([services, 'DIAMETER-Service', transport], ValidatedCfg)),
    %% make sure the handler config is also passed through to the session
    ?equal(maps:from_list(?DIAMETER_CONFIG ++ ?DIAMETER_SERVICE_OPTS),
	   get_cfg_value([apps, 'DIAMETER-Application', procedures,
			  authenticate, 'DIAMETER-Service'], ValidatedCfg)),

    ?error_set([handlers, ergw_aaa_rf], []),
    ?error_set([handlers, ergw_aaa_rf, invalid_option], []),

    ok.
