%%
%% AuthPending policy
%%
-module(ctld_p_auth_pending).
-behavior(ctld_profile).

-export([enter/1, event/2, handle_event/2]).

-include("include/ctld_profile.hrl").
-include("include/ctld_p_behavior.hrl").

-on_load(init/0).

init() ->
    ?register(?AuthPending).

enter(State) ->
    {ok, State}.

event({error, Error}, State) ->
    lager:debug("Error: ~p", [Error]),
    ?abort(Error, State);

event('ClearSession', State) ->
    ?stop(normal, State);

event(timeout, State) ->
    ?next(?OffLine, State);

event({'AuthenticationRequestReply', _AuthReply = #{'Profile' := ProfileName}}, State) ->
    %%
    %% apply AuthReply values to current State....
    %%

    %% NOTE: Class to Profile Mapping is performed by the AAA Provider....
    ?next(ProfileName, State);

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

	Ev = {'AuthenticationRequestReply', _AuthReply} ->
	    event(Ev, State)

    after TimeOut ->
	    event(timeout, State)
    end.
