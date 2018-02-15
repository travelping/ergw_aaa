%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_profile).

-export([initialize_provider/1, action/2, action/3, handle_reply/3]).

-include("include/ergw_aaa_profile.hrl").

-callback enter(State :: #{}) ->
    {ok, NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}} |
    {next_profile, NextProfileName :: atom(), NewState :: #{}}.

-callback event(Event :: term(), State :: #{}) ->
    {ok, NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}} |
    {next_profile, NextProfileName :: atom(), NewState :: #{}}.

initialize_provider(Config) ->
    Apps = proplists:get_value(applications, Config, []),
    ProvidersSupSpec = lists:flatmap(
        fun(App) ->
            {ok, {Handler, HandlerOpts, _AttrMap}} = get_opts(App),
            {ok, ProviderSupSpec} = Handler:initialize_provider(HandlerOpts),
            ProviderSupSpec
        end, Apps),
    {ok, ProvidersSupSpec}.

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
    AppId = maps:get('AAA-Application-Id', State, default),

    {ok, {Provider, ProviderOpts, AttrMap}} = get_application_opts(AppId),

    case Provider:init(ProviderOpts) of
        {ok, PState} ->
	    State1 = State#{'AuthProvider'        => Provider,
			    'AuthProviderState'   => PState},
            State2 = lists:foldl(fun attribute_map/2, State1, AttrMap),
            {ok, State2};
        Other ->
            {error, Other}
    end.

get_application_opts(ApplicationID) ->
    case setup:get_env(ergw_aaa, applications) of
        {ok, Providers} when is_list(Providers) ->
            case lists:keyfind(ApplicationID, 1, Providers) of
                false   -> get_opts(lists:keyfind(default, 1, Providers));
                AppOpts -> get_opts(AppOpts)
            end;
        _ ->
            get_old_application_opts()
    end.

get_old_application_opts() ->
    {ok, {Provider, ProviderOpts}} = setup:get_env(ergw_aaa, ergw_aaa_provider),
    {ok, {Provider, ProviderOpts, []}}.

get_opts({AppId, Provider}) ->
    get_opts({AppId, Provider, {attribute_map, []}});
get_opts({_AppId, {provider, Provider, ProviderOpts}, {attribute_map, AttrMap}}) ->
    {ok, {Provider, ProviderOpts, AttrMap}}.

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

attribute_map({Attr, disabled}, Attributes) ->
    maps:remove(Attr, Attributes);
attribute_map({Attr, Rule}, Attributes) ->
    AttrVal = compute_attribute(Rule, Attributes, <<>>),
    Attributes#{Attr => AttrVal}.

compute_attribute([], _Attributes, Res) -> Res;
compute_attribute([RuleVar | Tail], Attributes, Res)
  when is_binary(RuleVar) ->
    compute_attribute(Tail, Attributes, <<Res/binary, RuleVar/binary>>);
compute_attribute([RuleVar | Tail], Attributes, Res)
  when is_list(RuleVar) ->
    RuleVar1 = erlang:list_to_binary(RuleVar),
    compute_attribute(Tail, Attributes, <<Res/binary, RuleVar1/binary>>);
compute_attribute([RuleVar | Tail], Attributes, Res)
  when is_atom(RuleVar) ->
    Value = case maps:find(RuleVar, Attributes) of
        {ok, Result} -> Result;
        _ -> erlang:atom_to_binary(RuleVar, unicode)
    end,
    compute_attribute(Tail, Attributes, <<Res/binary, Value/binary>>).
