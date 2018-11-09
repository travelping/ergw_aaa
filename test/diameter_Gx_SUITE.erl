%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(diameter_Gx_SUITE).

-compile([nowarn_export_all, export_all]).

-include_lib("common_test/include/ct.hrl").
-include("ergw_aaa_test_lib.hrl").

-import(ergw_aaa_test_lib, [meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1,
			    get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(HUT, ergw_aaa_gx).
-define(SERVICE, ergw_aaa_gx).

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
-define(DIAMETER_SERVICE_OPTS, []).

-define(CONFIG,
	[{functions, [?DIAMETER_FUNCTION]},
	 {handlers,
	  [{ergw_aaa_static, ?STATIC_CONFIG},
	   {ergw_aaa_gx, ?DIAMETER_CONFIG}
	  ]},
	 {services,
	  [{'Default',
	    [{handler, 'ergw_aaa_static'}]},
	   {'DIAMETER-Service',
	    [{handler, 'ergw_aaa_gx'}]}
	  ]},

	 {apps,
	  [{default,
	    [{session, ['Default']},
	     {procedures, [{authenticate, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]},
			   {authorize, []},
			   {start, []},
			   {interim, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]},
			   {stop, [{'DIAMETER-Service', ?DIAMETER_SERVICE_OPTS}]}
			  ]}
	    ]}
	  ]}
	]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [handle_failure,
     authenticate].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    [application:set_env(ergw_aaa, Key, Opts) || {Key, Opts} <- ?CONFIG],

    meck_init(Config),

    diameter_test_server:start(),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    lager_common_test_backend:bounce(debug),

    case wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    ct:fail(Other)
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

handle_failure(Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'3GPP-IMSI' => <<"FAIL">>}),
    ?match({fail, 3001}, ergw_aaa_session:authenticate(Session, #{})),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(1, proplists:get_value({{16777238, 272, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(1, proplists:get_value({{16777238, 272, 0}, recv, {'Result-Code',3001}}, Statistics)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

authenticate(Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'3GPP-IMSI' => <<"IMSI">>}),
    ?equal(success, ergw_aaa_session:authenticate(Session, #{})),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(1, proplists:get_value({{16777238, 272, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(1, proplists:get_value({{16777238, 272, 0}, recv, {'Result-Code',2001}}, Statistics)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
