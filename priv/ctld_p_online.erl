%%
%% OnLine policy
%%
-module(ctld_p_online).
-behavior(ctld_profile).

-export([enter/1, event/2, handle_event/2]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_behavior.hrl").

-on_load(init/0).

init() ->
    ?register(?OnLine).

enter(State) ->
    StateT = state_t:new(identity_m),
    SM = StateT:modify(_),
    M = do([StateT ||
	       SM(set_session_timeout(_)),
	       SM(start_accounting(_))
	   ]),
    NewState = StateT:exec(M, State),
    lager:debug("Monad Return: ~p", [NewState]),
    {ok, NewState}.

event({error, Error}, State) ->
    lager:debug("Error: ~p", [Error]),
    ?abort(Error, State);

event('ClearSession', State) ->
    ?next(?StopAccounting, State#{'NextProfile' => stop, 'Reason' => 'ClearSession'});

event('StopSession', State) ->
    ?next(?StopAccounting, State#{'NextProfile' => ?OffLine, 'Reason' => 'StopSession'});

event(timeout, State) ->
    ?next(?StopAccounting, State#{'NextProfile' => ?OffLine, 'Reason' => timeout});

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
	Ev = 'StopSession' ->
	    event(Ev, State);

	_Ev = ?def_timer_event('SessionTimeout', Event, TimerRef) ->
	    handle_session_timeout(Event, TimerRef, State);

	Ev ->
	    %% drain all other events...
	    lager:debug("unexpected Event: ~p", [Ev]),
	    {ok, State}

    after TimeOut ->
	    event(timeout, State)
    end.

%%===================================================================
%% localy defined actions
%%===================================================================

set_session_timeout(State) ->
    lager:warning("TODO: implement something to start session timeout"),
    State#{ 'SessionTimeout' => ctld_variable:new('SessionTimeout', timer, 0, [{'SessionTimeout', limit, 30000}]) }.

handle_session_timeout(Event, _TimerRef, State0) ->
    lager:warning("TODO: implement something to handle session timeout"),
    State = stop_active_timers(State0),
    ?next(?StopAccounting, State#{'NextProfile' => ?OffLine, 'Reason' => Event}).

start_accounting(State) ->
    lager:warning("TODO: implement something to start accounting"),
    State.

stop_active_timers(State) ->
    lager:warning("TODO: implement something to stop all active timers"),
    State.
