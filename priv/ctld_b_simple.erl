-module(ctld_b_simple).

-behavior(ctld_behavior).

%% external API
-export([start/0, start/2, start_link/0, start_link/2]).
-export([handle_call/4]).
-export([terminate/3, code_change/4]).

%% ctld_behavior API
-export([init/1]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_simple.hrl").

%%===================================================================
%% Simple Session API
%%===================================================================

start() ->
    start(#{}, []).

start(Args, Opts) when is_map(Args) ->
    ctld_behavior:start(?MODULE, set_owner(Args), Opts).

start_link() ->
    start_link(#{}, []).

start_link(Args, Opts) when is_map(Args) ->
    ctld_behavior:start_link(?MODULE, set_owner(Args), Opts).

authenticate(Session, SessionOpts) when is_map(SessionOpts) ->
    ctld_behavior:send_event(Session, {'AuthenticationRequest', self(), SessionOpts}),
    receive
	{'AuthenticationRequestReply', Result} ->
	    Result
    after ?AAA_TIMEOUT ->
	    {error, timeout}
    end.

authorize(Session, SessionOpts) when is_map(SessionOpts) ->
    ctld_behavior:send_event(Session, {'AuthorizationRequest', self(), SessionOpts}),
    receive
	{'AuthorizationRequestReply', Result} ->
	    Result
    after ?AAA_TIMEOUT ->
	    {error, timeout}
    end.

start(Session, SessionOpts) when is_map(SessionOpts) ->
    ctld_behavior:send_event(Session, {'AccountingStart', SessionOpts}).

interim(Session, SessionOpts) when is_map(SessionOpts) ->
    ctld_behavior:send_event(Session, {'AccountingInterim', SessionOpts}).

interim_batch(Session, SessionOptsList) when is_list(SessionOptsList) ->
    ctld_behavior:send_event(Session, {'AccountingInterimBatch', SessionOptsList}).

stop(Session, SessionOpts) when is_map(SessionOpts) ->
    ctld_behavior:send_event(Session, {'AccountingStop', SessionOpts}).

%%===================================================================

init(Args) ->
    lager:debug("Debug..."),
    State = Args#{'SessionOpts' => #{}},
    {ok, ?Simple, State}.

handle_call(_Request, _From, StateName, State) ->
    {reply, ok, StateName, State}.

terminate(_Reason, _StateModule, _State) ->
    ok.

code_change(_OldVsn, StateModule, State, _Extra) ->
    {ok, StateModule, State}.

%%===================================================================

set_owner(Map) ->
    set_owner(Map, self).

set_owner(Map, Owner) ->
    Map#{'Owner' = Owner}.
