%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(config_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(error_option(Config),
	?match({error,{options, _}}, (catch ergw_aaa_config:load_config(Config)))).

-define(ok_option(Config),
	?match([_|_], ergw_aaa_config:load_config(Config))).

-define(RADIUS_OK_CFG,
	[{nas_identifier,<<"NAS-Identifier">>},
	 {radius_auth_server,{{127,0,0,1},1812,<<"secret">>}},
	 {radius_acct_server,{{0,0,0,0,0,0,0,1},1813,<<"secret">>}}]).

-define(RADIUS_CFG(Key, Value),
	lists:keystore(Key, 1, ?RADIUS_OK_CFG, {Key, Value})).

-define(DIAMETER_OK_CFG,
	[{nas_identifier,<<"NAS-Identifier">>},
	 {host, <<"127.0.0.1">>},
	 {realm, <<"example.com">>},
	 {connect_to, <<"aaa://127.0.0.1:3868">>}]).

-define(DIAMETER_CFG(Key, Value),
	lists:keystore(Key, 1, ?DIAMETER_OK_CFG, {Key, Value})).

%%%===================================================================
%%% API
%%%===================================================================

all() ->
    [config].

config() ->
    [{doc, "Test the config validation"}].
config(_Config)  ->
    ?ok_option([{vsn, "1.0.0"}]),

    ?error_option([{ergw_aaa_provider, invalid_option}]),
    ?error_option([{ergw_aaa_provider, {invalid_handler, []}}]),

    ?error_option([{ergw_aaa_provider, {ergw_aaa_mock, [{invalid_option, []}]}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_mock, [{shared_secret, invalid_secret}]}}]),
    ?ok_option([{ergw_aaa_provider, {ergw_aaa_mock, []}}]),
    ?ok_option([{ergw_aaa_provider, {ergw_aaa_mock, [{shared_secret, <<"secret">>}]}}]),

    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, []}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, [{invalid_option, []} | ?RADIUS_OK_CFG]}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(nas_identifier, invalid_id)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_auth_server, invalid_id)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, invalid_id)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, {"undefined.example.net",1812,<<"secret">>})}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, {invalid_ip,1812,<<"secret">>})}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, {{127,0,0,1},invalid_port,<<"secret">>})}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, {{127,0,0,1},1812,invalid_secret})}}]),

    ?ok_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_OK_CFG}}]),
    ?ok_option([{ergw_aaa_provider, {ergw_aaa_radius, ?RADIUS_CFG(radius_acct_server, {"localhost",1812,<<"secret">>})}}]),

    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, []}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, [{invalid_option, []} | ?DIAMETER_OK_CFG]}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(nas_identifier, invalid_id)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(host, invalid_host)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(realm, invalid_realm)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(connect_to, invalid_uri)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(host, <<"undefined.example.net">>)}}]),
    ?error_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_CFG(connect_to, <<"http://example.com:12345">>)}}]),

    ?ok_option([{ergw_aaa_provider, {ergw_aaa_diameter, ?DIAMETER_OK_CFG}}]),

    ok.
