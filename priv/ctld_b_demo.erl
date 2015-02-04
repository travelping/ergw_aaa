-module(ctld_b_simple).

-behavior(ctld_behavior).

%% external API
-export([start/0, start/2, start_link/0, start_link/2]).
-export([handle_call/4]).
-export([terminate/3, code_change/4]).

%% ctld_behavior API
-export([init/1]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_behavior.hrl").

start() ->
    start(#{}, []).

start(Args, Opts) when is_map(Args) ->
    ctld_behavior:start(?MODULE, Args, Opts).

start_link() ->
    start_link(#{}, []).

start_link(Args, Opts) when is_map(Args) ->
    ctld_behavior:start_link(?MODULE, Args, Opts).

init(Args) ->
    lager:debug("Debug..."),
    {ok, ?OffLine, Args}.

handle_call(_Request, _From, StateName, State) ->
    {reply, ok, StateName, State};

terminate(_Reason, _StateModule, _State) ->
    ok.

code_change(_OldVsn, StateModule, State, _Extra) ->
    {ok, StateModule, State}.
