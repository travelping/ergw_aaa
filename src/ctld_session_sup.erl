%% Copyright 2010-2012, Travelping GmbH <info@travelping.com>

-module(ctld_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_session/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_session(LMod, Leader, A3Handler, A3Opts, SessionData) ->
    supervisor:start_child(?SERVER, [LMod, Leader, A3Handler, A3Opts, SessionData]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
	  [{ctld_session, {ctld_session, start_link, []}, temporary, 1000, worker, [ctld_session]}]}}.
