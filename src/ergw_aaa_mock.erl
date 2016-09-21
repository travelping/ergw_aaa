%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_mock).

-behaviour(ergw_aaa).

%% AAA API
-export([init/1, authorize/3, start_authentication/3, start_accounting/4]).

-import(ergw_aaa_session, [to_session/1]).

-include("include/ergw_aaa_profile.hrl").

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

copy_session_id(#{'Session-Id' := SessionId}, Opts) ->
    Opts#{'Session-Id' => SessionId};
copy_session_id(_, Opts) ->
    Opts.

start_authentication(From, Session, State = #state{shared_secret = Secret}) ->
    SessionOpts = copy_session_id(Session, to_session([{'TLS-Pre-Shared-Key', Secret}])),
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
