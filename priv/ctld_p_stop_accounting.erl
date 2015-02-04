%%
%% AuthPending policy
%%
-module(ctld_p_stop_accounting).
-behavior(ctld_profile).

-export([enter/1, event/2, handle_event/2]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_behavior.hrl").

-on_load(init/0).

init() ->
    ?register(?StopAccounting).

enter(State) ->
    consolidate_accounting(State),
    {ok, State}.

event({error, Error}, State) ->
    lager:debug("Error: ~p", [Error]),
    ?abort(Error, State);

event('ClearSession', State) ->
    ?stop(normal, State);

event(timeout, State) ->
    ?next(?OffLine, State);

event('AccountingStopped', State = #{'NextProfile' := NextProfileName}) ->
    send_accounting_stop(State),
    ?next(NextProfileName, maps:remove('NextProfile', State));

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

	Ev = 'AccountingStopped' ->
	    event(Ev, State)

    after TimeOut ->
	    event(timeout, State)
    end.

%%===================================================================
%% localy defined actions
%%===================================================================

consolidate_accounting(State) ->
    lager:warning("TODO: implement something to consolidate accounting"),
    ?queue_event('AccountingStopped'),
    {ok, State}.

send_accounting_stop(State = #{'Reason' := Reason}) ->
    lager:warning("TODO: implement something to send accounting stop with ~p", [Reason]),
    {ok, State}.
