%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_SUITE).

%% Common Test callbacks
-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% Test cases
-export([check_CER_CEA/1, accounting/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [check_CER_CEA,
     accounting].


init_per_suite(Config) ->
    Opts = [{nas_identifier, <<"NAS">>},
            {host, <<"127.0.0.1">>},
            {realm, <<"example.com">>},
            {connect_to, <<"aaa://127.0.0.1:3868">>}
           ],
    application:load(ergw_aaa),
    application:set_env(ergw_aaa, ergw_aaa_provider, {ergw_aaa_diameter, Opts}),

    diameter_test_server:start(),
    application:ensure_all_started(ergw_aaa),

    timer:sleep(100),
    Config.

end_per_suite(_Config) ->
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

check_CER_CEA(_Config) ->
    Statistics = get_stats(),
    % check that client has sent CER
    1 = proplists:get_value({{0, 257, 1}, send}, Statistics),
    % check that client has received CEA
    1 = proplists:get_value({{0, 257, 0}, recv}, Statistics),
    ok.

accounting(_Config) ->
    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{}),
    success = ergw_aaa_session:authenticate(Session, #{}),
    ergw_aaa_session:start(Session, #{}),

    timer:sleep(100),
    Statistics = get_stats(),

    % check that client has sent ACR
    1 = proplists:get_value({{1, 271, 1}, send}, Statistics),
    % check that client has received ACA
    %1 = proplists:get_value({{1, 271, 0}, recv}, Statistics),
    ok.

get_stats() ->
    [Transport] = diameter:service_info(ergw_aaa_diameter, transport),
    proplists:get_value(statistics, Transport).
