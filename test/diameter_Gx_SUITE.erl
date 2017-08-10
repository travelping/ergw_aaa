%% Copyright 2017, Travelping GmbH <info@travelping.com>
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

-import(diameter_test_server, [get_stats/1, diff_stats/2, wait_for_diameter/2]).

-define(SERVICE, ergw_aaa_gx).

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
             false
     end)(Expected, Actual) orelse error(badmatch)).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [handle_failure,
     authenticate].

init_per_suite(Config) ->
    diameter_test_server:start(),

    Opts = [{host, <<"127.0.0.1">>},
            {realm, <<"example.com">>},
            {connect_to, <<"aaa://127.0.0.1">>}
           ],
    application:load(ergw_aaa),
    application:set_env(ergw_aaa, ergw_aaa_provider, {ergw_aaa_diameter_gx, []}),
    application:set_env(ergw_aaa, apis, [{?SERVICE, Opts}]),

    application:ensure_all_started(ergw_aaa),

    case diameter_test_server:wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    ct:fail(Other)
    end.

end_per_suite(_Config) ->
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

handle_failure(_Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'3GPP-IMSI' => <<"FAIL">>}),
    fail = ergw_aaa_session:authenticate(Session, #{}),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(1, proplists:get_value({{16777238, 272, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(1, proplists:get_value({{16777238, 272, 0}, recv, {'Result-Code',3001}}, Statistics)),
    ok.

authenticate(_Config) ->
    Stats0 = get_stats(?SERVICE),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'3GPP-IMSI' => <<"IMSI">>}),
    success = ergw_aaa_session:authenticate(Session, #{}),

    Statistics = diff_stats(Stats0, get_stats(?SERVICE)),

    % check that client has sent CCR
    ?equal(1, proplists:get_value({{16777238, 272, 1}, send}, Statistics)),
    % check that client has received CCA
    ?equal(1, proplists:get_value({{16777238, 272, 0}, recv, {'Result-Code',2001}}, Statistics)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
