-module(ctld_mock).

-behaviour(ctld_aaa).

%% AAA API
-export([init/1, authenticate/3, authorize/3, start/2, interim/2, stop/2]).

-import(ctld_session, [attr_get/2, attr_get/3, attr_set/3, attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

-record(state, {shared_secret,
		auth_state, accounting = []}).

%%===================================================================
%% API
%%===================================================================
init(Opts) ->
    State = #state{
      shared_secret = proplists:get_value(shared_secret, Opts, <<"secret">>)
     },
    {ok, State}.

authenticate(_From, _Session, State = #state{shared_secret = Secret}) ->
    SessionOpts = [{'TLS-Pre-Shared-Key', Secret}],
    Verdict = success,
    {reply, Verdict, to_session(SessionOpts), State}.

authorize(_From, _Session, State) ->
    Verdict = success,
    {reply, Verdict, to_session([]), State}.

start(_Session, State) ->
    Now = now_ticks(),
    SessionOpts = [{'Accounting-Start', Now}],
    {to_session(SessionOpts), State}.

interim(_Session, State) ->
    Now = now_ticks(),
    SessionOpts = [{'Last-Interim-Update', Now}],
    {to_session(SessionOpts), State}.

stop(_Session, State) ->
    Now = now_ticks(),
    SessionOpts = [{'Accounting-Stop', Now}],
    {to_session(SessionOpts), State}.

%%===================================================================
%% Internal Helpers
%%===================================================================

%% get time with 100ms +/50ms presision
now_ticks() ->
    now_ticks(erlang:now()).

now_ticks({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 10000000 + Secs * 10 + round(MicroSecs div 100000).
