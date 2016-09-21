%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_session/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_session(Owner, SessionOpts) ->
    supervisor:start_child(?SERVER, [Owner, SessionOpts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
	  [{ergw_aaa_session, {ergw_aaa_session, start_link, []}, temporary, 1000, worker, [ergw_aaa_session]}]}}.
