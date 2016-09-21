%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_profile).

-export([action/2, action/3, handle_reply/3]).

-include("include/ergw_aaa_profile.hrl").

-callback enter(State :: #{}) ->
    {ok, NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}} |
    {next_profile, NextProfileName :: atom(), NewState :: #{}}.

-callback event(Event :: term(), State :: #{}) ->
    {ok, NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}} |
    {next_profile, NextProfileName :: atom(), NewState :: #{}}.

action('Authenticate', State) ->
    lager:debug("Event: Authenticate ~p", [State]),

    %% get or / init ctld provider ref
    case init_provider(State) of
	{ok, State1} ->
	    %% send auth request
	    start_authentication(State1);
	{error, Error} ->
	    ?queue_event('AuthenticationRequestReply', {failed, #{'Reason' => Error}}),
	    {ok, State}
    end;

action(Action, State) ->
    lager:warning("TODO: implement action: ~p", [Action]),
    {ok, State}.

action('Authenticate', [AuthData], State)
  when is_map(AuthData) ->
    action('Authenticate', maps:merge(State, AuthData));

action('Account', [AcctType], State) ->
    lager:debug("Event: Account ~p", [AcctType]),

    %% get or / init ctld provider ref
    case init_provider(State) of
	{ok, State1} ->
	    %% send auth request
	    request_accounting(AcctType, State1);
	{error, Error} ->
	    ?queue_event('AccountingRequestReply', {failed, #{'Reason' => Error}}),
	    {ok, State}
    end;

action(Action, Args, State) ->
    lager:warning("~w TODO: implement action: ~p(~p)", [?MODULE, Action, Args]),
    {ok, State}.

handle_reply(Fun, {'AuthenticationRequestReply', {Verdict, SessionOpts, PState}}, State)
  when is_function(Fun, 2) ->
    Fun({'AuthenticationRequestReply', {Verdict, SessionOpts}}, State#{'AuthProviderState' => PState});
handle_reply(Fun, Ev, State)
  when is_function(Fun, 2) ->
    Fun(Ev, State).

%%===================================================================
%% Internal
%%===================================================================
invoke_provider(F, A,
		State = #{'AuthProvider' := Provider, 'AuthProviderState' := PState}) ->
    {Reply, PState1} = apply(Provider, F, A ++ [State, PState]),
    {Reply, State#{'AuthProviderState' := PState1}}.

init_provider(State = #{'AuthProvider' := _Provider}) ->
    {ok, State};
init_provider(State) ->
    lager:debug("Application: ~p", [application:get_application()]),
    lager:debug("Env: ~p", [application:get_all_env()]),
    {ok, {Provider, ProviderOpts}} = application:get_env(ergw_aaa_provider),

    case Provider:init(ProviderOpts) of
        {ok, PState} ->
	    State1 = State#{'AuthProvider'      => Provider,
			    'AuthProviderState' => PState},
            {ok, State1};
        Other ->
            {error, Other}
    end.

start_authentication(State) ->
    invoke_provider(start_authentication, [self()], State).

request_accounting(AcctType, State) ->
    State1 = update_accounting_state(AcctType, State),
    invoke_provider(start_accounting, [self(), AcctType], State1).

update_accounting_state('Start', State) ->
    State#{ 'Accounting-Start' => ergw_aaa_variable:now_ms() };
update_accounting_state('Interim', State) ->
    State#{ 'Last-Interim-Update' => ergw_aaa_variable:now_ms() };
update_accounting_state('Stop', State) ->
    State#{ 'Accounting-Stop' => ergw_aaa_variable:now_ms() };
update_accounting_state(_AcctType, State) ->
    State.
