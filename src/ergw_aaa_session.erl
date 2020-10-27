%% Copyright 2016-2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_session).

-compile({parse_transform, cut}).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4,
	 terminate/2, code_change/3]).

%% Session Process API
-export([start_link/2, invoke/4,
	 authenticate/2, authorize/2,
	 start/2, start/3,
	 interim/2, interim/3,
	 stop/2, stop/3,
	 terminate/1, get/1, get/2, get/3, set/2, set/3, unset/2,
	 request/4, response/4]).

%% Session Object API
-export([to_session/1, native_to_seconds/1]).

-include_lib("kernel/include/logger.hrl").
-include("include/ergw_aaa_session.hrl").

-record(data, {'$version' = 1,
	       owner,
	       owner_monitor,
	       application,
	       handlers,
	       session
	      }).
-record(state, {
	       }).

-define(AAA_TIMEOUT, (30 * 1000)).      %% 30sec for all AAA gen_statem:call timeouts
-define(DEFAULT_INTERIM_ACCT, 600).     %% 10 minutes for accounting interim
-define(DEFAULT_SERVICE_TYPE, 'Framed-User').
-define(DEFAULT_FRAMED_PROTO, 'PPP').

%%===================================================================
%% Experimental API
%%===================================================================

-export([ev_add/2, ev_del/2, ev_set/2]).
-export([trigger/4, trigger/5, event/4]).

ev_add({K, Ev}, A) ->
    [{add, {K, Ev}}|A].
ev_del({K, Ev}, A) ->
    [{del, {K, Ev}}|A].
ev_set({K, Ev}, A) ->
    [{set, {K, Ev}}|A].


%% SubSys: atom()
%% Level: 'IP-CAN' | {'rule', RuleName}
%% Type: 'time'
%% Value: term()
%% Opts: ['recurring']

trigger(SubSys, Level, Type, Value) ->
    trigger(SubSys, Level, Type, Value, []).

trigger(SubSys, Level, Type, Value, Opts) ->
    {{SubSys, Level, Type}, {Type, Level, Value, Opts}}.

event(Session, Event, EvOpts, SessionOpts) when is_map(SessionOpts) ->
    gen_statem:call(Session, {Event, EvOpts, SessionOpts}, ?AAA_TIMEOUT).

%%===================================================================
%% API
%%===================================================================

start_link(Owner, SessionData) ->
    Opts = [{hibernate_after, 500},
	    {spawn_opt,[{fullsweep_after, 0}]}],
    gen_statem:start_link(?MODULE, [Owner, SessionData], Opts).

invoke_compat_async(Session, SessionOpts, Procedure) ->
    case invoke(Session, SessionOpts, Procedure, #{async => true}) of
	Result when is_tuple(Result) ->
	    element(1, Result);
	Other ->
	    Other
    end.

invoke(Session, SessionOpts, Procedure, Opts)
  when is_list(Opts); is_map(Opts) ->
    gen_statem:call(Session, {invoke, SessionOpts, Procedure, normalize_opts(Opts)}, ?AAA_TIMEOUT).

request(Session, Handler, Procedure, Avps) ->
     gen_statem:call(Session, {request, Handler, Procedure, Avps}).

response(#aaa_request{from = {Pid, _}} = Request, Result, Avps, SessionOpts)
  when is_pid(Pid), is_map(Avps) ->
    gen_statem:cast(Pid, {'$response', Request, Result, Avps, SessionOpts});
response(#aaa_request{from = Fun} = Request, Result, Avps, SessionOpts)
  when is_function(Fun, 4), is_map(Avps) ->
    Fun(Request, Result, Avps, SessionOpts).

authenticate(Session, SessionOpts)
  when is_map(SessionOpts) ->
    case invoke(Session, SessionOpts, authenticate, [inc_session_id]) of
	{ok, _, _} ->
	    success;
	{Other, _, _} ->
	    Other;
	Other ->
	    Other
    end.

authorize(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, authorize, []).

start(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke_compat_async(Session, SessionOpts, start).

start(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, start, Opts).

interim(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke_compat_async(Session, SessionOpts, interim).

interim(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, interim, Opts).

stop(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke_compat_async(Session, SessionOpts, stop).

stop(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, stop, Opts).

terminate(Session) ->
    gen_statem:cast(Session, terminate).

get(Session) ->
    gen_statem:call(Session, get).

get(Session, Option) ->
    gen_statem:call(Session, {get, Option}).

get(Session, Option, Default) ->
    gen_statem:call(Session, {get, Option, Default}).

set(Session, Option, Value) ->
    gen_statem:call(Session, {set, Option, Value}).

set(Session, Values) when is_map(Values) ->
    gen_statem:call(Session, {set, Values}).

unset(Session, Options) when is_list(Options) ->
    gen_statem:call(Session, {unset, Options});
unset(Session, Option) when is_atom(Option) ->
    gen_statem:call(Session, {unset, [Option]}).

%%===================================================================
%% gen_statem callbacks
%%===================================================================

%% callback_mode() -> [handle_event_function, state_enter].
callback_mode() -> handle_event_function.

init([Owner, SessionOpts]) ->
    process_flag(trap_exit, true),

    AppId = maps:get('AAA-Application-Id', SessionOpts, default),
    SessionId = ergw_aaa_session_seq:inc(AppId),
    MonRef = erlang:monitor(process, Owner),

    App = ergw_aaa_config:get_application(AppId),
    OriginHost = maps:get('Origin-Host', App, net_adm:localhost()),
    DiamSessionId =
	iolist_to_binary(ergw_aaa_session_seq:diameter_session_id(OriginHost, SessionId)),

    DefaultSessionOpts =
	#{'Session-Start'       => erlang:monotonic_time(),
	  'Session-Id'          => SessionId,
	  'Multi-Session-Id'    => SessionId,
	  'Diameter-Session-Id' => DiamSessionId
	 },

    ergw_aaa_session_reg:register(SessionId),
    ergw_aaa_session_reg:register(DiamSessionId),

    Data = #data{
	      owner         = Owner,
	      owner_monitor = MonRef,
	      application   = App,
	      handlers      = #{},
	      session       = DefaultSessionOpts
	      },
    State = #state{},
    {Reply, DataOut, _Events} = exec(init, SessionOpts, #{}, Data),
    {Reply, State, DataOut}.

handle_event({call, From}, get, _State, Data) ->
    {keep_state_and_data, [{reply, From, Data#data.session}]};

handle_event({call, From}, {get, Opt}, _State, Data) ->
    {keep_state_and_data, [{reply, From, maps:find(Opt, Data#data.session)}]};

handle_event({call, From}, {get, Opt, Default}, _State, Data) ->
    {keep_state_and_data, [{reply, From, maps:get(Opt, Data#data.session, Default)}]};

handle_event({call, From}, {set, Opt, Value}, _State, Data = #data{session = Session}) ->
    {keep_state, Data#data{session = maps:put(Opt, Value, Session)}, [{reply, From, ok}]};

handle_event({call, From}, {set, Values}, _State, Data) ->
    {keep_state, Data#data{session = maps:merge(Data#data.session, Values)}, [{reply, From, ok}]};

handle_event({call, From}, {unset, Options}, _State, Data = #data{session = Session}) ->
    {keep_state, Data#data{session = maps:without(Options, Session)}, [{reply, From, ok}]};

handle_event(info, {'EXIT', Owner, Reason},
	     _State, #data{owner = Owner} = Data) ->
    ?LOG(error, "Received EXIT signal for ~p with reason ~p", [Owner, Reason]),
    handle_owner_exit(Data),
    {stop, normal};

handle_event(info, {'EXIT', _From, _Reason}, _State, _Data) ->
    %% ignore EXIT from eradius client
    keep_state_and_data;

handle_event(state_timeout, #aaa_request{caller = From}, _State, _Data) ->
    Reply = {error, #{}},
    {keep_state_and_data, [{reply, From, Reply}]};

handle_event(cast, {'$response', #aaa_request{caller = From, handler = Handler},
		    Result, Avps0, SessionOpts},
	     State, #data{session = Session0} = Data) ->
    Session = session_merge(Session0, SessionOpts),
    Avps = Handler:from_session(Session, Avps0),
    Reply = [{reply, From, {Result, Avps}}],
    {next_state, State, Data#data{session = Session}, Reply};

handle_event({call, From}, {request, Handler, Procedure, Avps}, _State,
	     #data{owner = Owner, session = Session0}) ->

    {Session, Events} = Handler:to_session(Procedure, {Session0, []}, Avps),

    Request = #aaa_request{
		 from = {self(), make_ref()},
		 caller = From,
		 handler = Handler,
		 procedure = Procedure,
		 session = Session,
		 events = Events},
    Owner ! Request,
    {keep_state_and_data, [{state_timeout, 10 * 1000, Request}]};

handle_event({call, From}, {{Handler, _Level, time}, EvOpts, SessionOpts}, _State,
	     #data{session = Session0} = Data) ->

    Service = proplists:get_value(service, EvOpts, default),
    Procedure = proplists:get_value(procedure, EvOpts, interim),
    Session = session_merge(Session0, SessionOpts),
    {Result, NewSession, Events} = Handler:invoke(Service, Procedure, Session, [], EvOpts),
    Reply = {Result, NewSession, Events},
    {keep_state, Data#data{session = NewSession}, [{reply, From, Reply}]};

handle_event(cast, {answer, {_, NewSession, Events}}, _State, #data{owner = Owner})
  when Events /= [] ->
    Owner ! {update_session, NewSession, Events},
    keep_state_and_data;
handle_event(cast, {answer, _}, _State, _Data) ->
    keep_state_and_data;

handle_event({call, From}, {invoke, SessionOpts, Procedure, Opts},
	     _State, Data0) ->
    Async = maps:get(async, Opts, false),
    case exec(Procedure, SessionOpts, Opts, Data0) of
	{Result, #data{session = Session} = Data, Events} when Async =:= false ->
	    Reply = {Result, Session, Events},
	    {keep_state, Data, [{reply, From, Reply}]};
	{Result, #data{session = Session} = Data, Events} when Async =:= true ->
	    update_session(Session, Events, Data),
	    Reply = {Result, Session},
	    {keep_state, Data, [{reply, From, Reply}]};
	{Result, #data{session = Session} = Data} ->
	    Reply = {Result, Session},
	    {keep_state, Data, [{reply, From, Reply}]}
    end;

handle_event(cast, terminate, _State, _Data) ->
    ?LOG(info, "Handling terminate request: ~p", [_Data]),
    {stop, normal};

handle_event(info, {'DOWN', _Ref, process, Owner, Info},
	     _State, #data{owner = Owner} = Data) ->
    ?LOG(error, "Received DOWN information for ~p with info ~p", [Data#data.owner, Info]),
    handle_owner_exit(Data),
    {stop, normal};

handle_event(info, {'$reply', Promise, Handler, Msg, Opts} = _Info,
	     _State, #data{handlers = HandlersS, session = Session} = Data0) ->
    State = maps:get(Handler, HandlersS, undefined),
    {_, SessOut, EvsOut, StateOut} =
	Handler:handle_response(Promise, Msg, Session, [], Opts, State),
    aaa_state_stats(Handler, State, StateOut),
    Data = Data0#data{handlers = maps:put(Handler, StateOut, HandlersS), session = SessOut},
    update_session(SessOut, EvsOut, Data),
    {keep_state, Data};

handle_event(Type, Event, _State, _Data) ->
    ?LOG(warning, "unhandled event ~p:~p", [Type, Event]),
    keep_state_and_data.

terminate(Reason, Data) ->
    ?LOG(error, "ctld Session terminating with state ~p with reason ~p", [Data, Reason]),
    ok.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%===================================================================
%% Session Object API and Helpers
%%
%% Purpose: hide the internals of Session Objects from the rest of
%%          the world
%%===================================================================

to_session(Session) when is_list(Session) ->
    maps:from_list(Session);
to_session(Session) when is_map(Session) ->
    Session.

%%===================================================================
%% internal helpers
%%===================================================================
prepare_next_session_id(Session) ->
    AcctAppId = maps:get('AAA-Application-Id', Session, default),
    Session#{'Session-Id' => ergw_aaa_session_seq:inc(AcctAppId)}.

handle_owner_exit(Data) ->
    Opts = #{now => erlang:monotonic_time()},
    action(stop, Opts, Data).

maps_merge_with(K, Fun, V, Map) ->
    maps:update_with(K, maps:fold(Fun, V, _), V, Map).

monitor_merge(K, V, M)
  when is_map(V) ->
    maps_merge_with(K, fun monitor_merge/3, V, M);
monitor_merge(K, V, Values)
  when is_integer(V) ->
    maps:update_with(K, (_ + V), V, Values).

session_merge(monitors = K, V, Session) ->
    monitor_merge(K, V, Session);
session_merge(K, V, Session) ->
    Session#{K => V}.

session_merge(Session, Opts) ->
    maps:fold(fun session_merge/3, Session, Opts).

normalize_opts(Opts) when is_list(Opts) ->
    maps:from_list(proplists:unfold(Opts));
normalize_opts(Opts) when is_map(Opts) ->
    Opts.

%%===================================================================
%% provider helpers
%%===================================================================

update_session(_Session, [], _Data) ->
    ok;
update_session(Session, Events, #data{owner = Owner}) ->
    Owner ! {update_session, Session, Events}.

exec(Procedure, SessionOpts, Opts0, #data{owner = Owner, session = SessionIn} = Data) ->
    Opts = Opts0#{now => maps:get(now, Opts0, erlang:monotonic_time()),
		  owner => Owner},
    Session0 = session_merge(SessionIn, SessionOpts),
    Session1 = maps:fold(fun handle_session_opts/3, Session0, Opts),
    Session2 = update_accounting_state(Procedure, Session1, Opts),
    action(Procedure, Opts, Data#data{session = Session2}).

native_to_seconds(Native) ->
    round(Native / erlang:convert_time_unit(1, second, native)).

handle_session_opts(inc_session_id, true, Session) ->
    prepare_next_session_id(Session);
handle_session_opts(_K, _V, Session) ->
    Session.

accounting_start(#{'Accounting-Start' := Start}) ->
    Start;
accounting_start(#{'Session-Start' := Start}) ->
    Start.

update_accounting_state(start, Session, #{now := Now}) ->
    Session#{'Accounting-Start' => Now};
update_accounting_state(interim, Session, #{now := Now}) ->
    Start = accounting_start(Session),
    Session#{'Acct-Session-Time' => native_to_seconds(Now - Start),
	     'Last-Interim-Update' => Now};
update_accounting_state(stop, Session, #{now := Now}) ->
    Start = accounting_start(Session),
    Session#{'Acct-Session-Time' => native_to_seconds(Now - Start),
	     'Accounting-Stop' => Now};
update_accounting_state(_Procedure, Session, _Opts) ->
    Session.

services(init, App) ->
    Procedures =
	maps:fold(
	  fun(_, Svcs, S0) ->
		  lists:foldl(fun({Svc, _}, S1) -> S1#{Svc => #{}} end, S0, Svcs)
	  end, #{}, maps:get(procedures, App, #{})),
    Session = maps:get(session, App, []),
    {Keys, _} = lists:unzip(Session),
    Session ++ maps:to_list(maps:without(Keys, Procedures));
services(Procedure, App) ->
    Procedures = maps:get(procedures, App, #{}),
    maps:get(Procedure, Procedures, []).

action(Procedure, Opts, #data{application = App} = Data) ->
    Pipeline = services(Procedure, App),
    pipeline(Procedure, Data, [], Opts, Pipeline).

pipeline(_, Data, Events, _Opts, []) ->
    {ok, Data, Events};
pipeline(Procedure, DataIn, EventsIn, Opts, [Head|Tail]) ->
    case step(Head, Procedure, DataIn, EventsIn, Opts) of
	{ok, DataOut, EventsOut} ->
	    pipeline(Procedure, DataOut, EventsOut, Opts, Tail);
	Other ->
	    Other
    end.

step({Service, SvcOpts}, Procedure, #data{handlers = HandlersS,
					  session = Session0} = Data, Events, Opts)
  when is_atom(Service) ->
    Svc = ergw_aaa_config:get_service(Service),
    StepOpts = maps:merge(Opts, SvcOpts),
    Handler = maps:get(handler, Svc),
    Session = termination_cause_mapping(Session0, StepOpts),
    State = maps:get(Handler, HandlersS, undefined),
    {Result, SessOut, EvsOut, StateOut} =
	Handler:invoke(Service, Procedure, Session, Events, StepOpts, State),
    aaa_state_stats(Handler, State, StateOut),
    {Result, Data#data{handlers = maps:put(Handler, StateOut, HandlersS),
		       session = SessOut}, EvsOut}.

aaa_state_stats_dec(_, From)
    when From =:= undefined; From =:= stopped ->
	ok;
aaa_state_stats_dec(Handler, From) ->
    prometheus_gauge:dec(aaa_sessions_total, [Handler, From]).

aaa_state_stats_inc(_, To)
    when To =:= undefined; To =:= stopped ->
	ok;
aaa_state_stats_inc(Handler, To) ->
    prometheus_gauge:inc(aaa_sessions_total, [Handler, To]).

aaa_state_stats(Handler, CurrentState, NewState) ->
    From = get_handler_state(Handler, CurrentState),
    To = get_handler_state(Handler, NewState),
    if From /= To ->
	    aaa_state_stats_dec(Handler, From),
	    aaa_state_stats_inc(Handler, To);
       From == To ->
	       ok
    end.

get_handler_state(_, undefined) ->
    undefined;
get_handler_state(Handler, State) ->
    Handler:get_state_atom(State).

termination_cause_mapping(#{'Termination-Cause' := Cause} = Session, #{termination_cause_mapping := Config})
  when is_atom(Cause), is_map_key(Cause, Config) ->
    Session#{'Termination-Cause' := maps:get(Cause, Config)};
termination_cause_mapping(Session, _) ->
    Session.
