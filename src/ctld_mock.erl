-module(ctld_mock).

-behaviour(ctld_aaa).

%% AAA API
-export([init/1, session_id/2, authorize/3, start_authentication/3, start_accounting/4]).

-import(ctld_session, [to_session/1]).

-include("include/ctld_profile.hrl").

-record(state, {shared_secret,
		auth_state,
		accounting = [],
		acct_app_id = default}).

%%===================================================================
%% API
%%===================================================================
init(Opts) ->
    State = #state{
      shared_secret = proplists:get_value(shared_secret, Opts, <<"secret">>)
     },
    {ok, State}.

session_id(#{'Session-Id' := SessionId}, State) ->
    {SessionId, State};
session_id(#{'AAA-Application-Id' := AcctAppId}, State) ->
    {ctld_session_seq:inc(AcctAppId), State};
session_id(_Session, State = #state{acct_app_id = AcctAppId}) ->
    {ctld_session_seq:inc(AcctAppId), State}.

start_authentication(From, _Session, State = #state{shared_secret = Secret}) ->
    SessionOpts =  to_session([{'TLS-Pre-Shared-Key', Secret}]),
    Verdict = success,
    ?queue_event(From, {'AuthenticationRequestReply', {Verdict, SessionOpts, State}}),
    {ok, State}.

authorize(_From, _Session, State) ->
    Verdict = success,
    {reply, Verdict, to_session([]), State}.

start_accounting(_From, 'Start', _Session, State) ->
    {ok, State};

start_accounting(_From, 'Interim', _Session, State) ->
    {ok, State};

start_accounting(_From, 'Stop', _Session, State) ->
    {ok, State}.
