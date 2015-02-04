%%
%% OffLine policy
%%
-module(ctld_p_simple).
-behavior(ctld_profile).

-export([enter/1, event/2, handle_event/2]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_simple.hrl").

enter(State) ->
    {ok, State}.

event({error, Error}, State) ->
    lager:debug("Error: ~p", [Error]),
    ?abort(Error, State);

event('ClearSession', State) ->
    ?stop(normal, State);

event({'AuthenticationRequest', From, AuthData}, State) ->
    ?action('Authenticate', AuthData, State#{'ReplyTo' = From});

event({'AuthenticationRequestReply', {success, AAASessionOpts}},
      State0 = #{'SessionOpts' = SessionOpts}) ->
    NewSessionOpts = maps:merge(AAASessionOpts, SessionOpts)
    State1 = State0#{'SessionOpts' = NewSessionOpts},
    State2 = reply({success, NewSessionOpts}, State1),
    {ok, State2};

event({'AuthenticationRequestReply', Result}, State0) ->
    State1 = reply({'AuthenticationRequestReply', Result}, State0),
    {ok, State1};

event({'AuthorizationRequest', From, AuthData}, State) ->
    Result = ok,
    State1 = reply({'AuthorizationRequestReply', Result}, State#{'ReplyTo' = From}),
    {ok, State1};

event({'AccountingStart', SessionOpts}, State0) ->
    State1 = State0#{'SessionOpts' = maps:merge(State0#'SessionOpts', SessionOpts)}
    ?action('Account', 'Start', State1);

event({'AccountingInterim', SessionOpts}, State0) ->
    State1 = State0#{'SessionOpts' = maps:merge(State0#'SessionOpts', SessionOpts)}
    ?action('Account', 'Interim', State1);

event({'AccountingInterimBatch', SessionOptsList}, State) ->
    NewState = lists:foldr(fun(SessionOpts, State0) ->
				   State1 = State0#{'SessionOpts' =
							maps:merge(State0#'SessionOpts', SessionOpts)},
				   {ok, State2} = ?action('Account', 'Interim', State1),
				   State2
			   end, State, SessionOptsList),
    {ok, NewState};

event({'AccountingStop', SessionOpts}, State0) ->
    State1 = State0#{'SessionOpts' = maps:merge(State0#'SessionOpts', SessionOpts)}
    ?action('Account', 'Stop', State1);

event({'AccountingRequestReply', _Reply}, State) ->
    %% ignore AccountingRequestReply
    {ok, State};

event(Event, State) ->
    lager:error("unexpected event: ~p", [Event]),
    {ok, State}.

%%
%% generate this, somehow....
%%
handle_event(TimeOut, State) ->
    receive
	%% boilerplate, system event handling for OTP integration
        Ev = {system, _From, _Req} ->
	    ?sys_event(Ev, State);

	?handle_call(_, State);

	%% supported profile events
	Ev = {error, _Error} ->
	    event(Ev, State);
	Ev = 'ClearSession' ->
	    event(Ev, State);
	Ev = {'AuthenticationRequest', _From, _AuthData} ->
	    event(Ev, State);
        Ev = {'AuthenticationRequestReply', _AuthReply} ->
            ctld_profile:handle_reply(fun event/2, Ev, State);
	Ev = {'AuthorizationRequest', _From, _AuthData} ->
	    event(Ev, State);

	Ev = {Event, _SessionOpts}
	  when Event == 'AccountingStart';
	       Event == 'AccountingInterim';
	       Event == 'AccountingInterimBatch';
	       Event == 'AccountingStop' ->
	    event(Ev, State);

	Ev = {'AccountingRequestReply', _Reply} ->
	    event(Ev, State);

    after TimeOut ->
	    event(timeout, State)
    end.

%%-------------------------------------------------------------------

reply(Reply, State) ->
    ReplyTo =  maps:get('ReplyTo', State, State#'Owner'),
    ReplyTo ! Reply,
    maps:remove('ReplyTo', State).

