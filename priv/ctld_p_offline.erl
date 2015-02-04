%%
%% OffLine policy
%%
-module(ctld_p_offline).
-behavior(ctld_profile).

-export([enter/1, event/2, handle_event/2]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_behavior.hrl").

-on_load(init/0).

init() ->
    ?register(?OffLine).

enter(State) ->
    %%
    %% a Monad that keeps the state, allows returns and aborts
    %% on anything not ok would be nice, but none of the existing
    %% ones fits that bill.....
    %% ... for simplicty sake use the plain state_t for the moment....
    %%
    StateT = state_t:new(identity_m),
    SM = StateT:modify(_),
    SMR = StateT:modify_and_return(_),
    M = do([StateT ||
	       SM(dummy1(_)),
	       T <- SMR(dummy2(_)),
	       SM(dummy3(T, _))
	   ]),
    NewState = StateT:exec(M, State),
    {ok, NewState}.

event({error, Error}, State) ->
    lager:debug("Error: ~p", [Error]),
    ?abort(Error, State);

event('ClearSession', State) ->
    ?stop(normal, State);

event({'AuthenticationRequest', AuthData}, State) ->
    ?action('Authenticate', AuthData, State),
    ?next(?AuthPending, State);

event(Event, State) ->
    lager:error("unexpected event: ~p", [Event]),
    {ok, State}.


%%
%% generate this, somehow....
%%
handle_event(TimeOut, State) ->
    receive
	%% boilerplate, sytem event handling for OTP integration
        Ev = {system, _From, _Req} ->
	    ?sys_event(Ev, State);

	%% supported profile events
	Ev = {error, _Error} ->
	    event(Ev, State);
	Ev = 'ClearSession' ->
	    event(Ev, State);
	Ev = {'AuthenticationRequest', _AuthData} ->
	    event(Ev, State)

    after TimeOut ->
	    event(timeout, State)
    end.

dummy1(State) ->
    lager:debug("State #1: ~p", [State]),
    State#{ dummy => 1 }.

dummy2(State) ->
    lager:debug("State #2: ~p", [State]),
    {test, State#{ dummy => 2 }}.

dummy3(T, State) ->
    lager:debug("T: ~p", [T]),
    lager:debug("State #3: ~p", [State]),
    State#{ dummy => 3 }.

%% dummy1(State) ->
%%     lager:debug("State #1: ~p", [State]),
%%     1.

%% dummy2(State) ->
%%     lager:debug("State #2: ~p", [State]),
%%     2.

%% dummy3(State) ->
%%     lager:debug("State #3: ~p", [State]),
%%     3.

%% dummy3(T, State) ->
%%     lager:debug("T: ~p", [T]),
%%     lager:debug("State #3: ~p", [State]),
%%     3.
