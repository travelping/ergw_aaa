%% Copyright 2016-2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_session).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4,
	 terminate/2, code_change/3]).

%% Session Process API
-export([start_link/2, invoke/4, invoke/5,
	 authenticate/2, authorize/2,
	 start/2, start/3,
	 interim/2, interim/3,
	 stop/2, stop/3,
	 terminate/1, get/1, get/2, set/2, set/3, sync/1]).

%% Session Object API
-export([attr_get/2, attr_get/3, attr_set/3,
	 attr_append/3, attr_fold/3,
	 merge/2, to_session/1,
	 native_to_seconds/1]).

-export([get_svc_opt/2, set_svc_opt/3]).

-record(data, {'$version' = 1,
	       owner,
	       owner_monitor,
	       application,
	       session
	      }).
-record(state, {
		pending :: 'undefined' | pid()
	       }).

-define(AAA_TIMEOUT, (30 * 1000)).      %% 30sec for all AAA gen_statem:call timeouts
-define(DEFAULT_INTERIM_ACCT, 600).     %% 10 minutes for accounting interim
-define(DEFAULT_SERVICE_TYPE, 'Framed-User').
-define(DEFAULT_FRAMED_PROTO, 'PPP').

%%===================================================================
%% Experimental API
%%===================================================================

-export([ev_add/2, ev_del/2, ev_set/2]).
-export([trigger/5, event/4]).

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
trigger(SubSys, Level, time, Value, Opts) ->
    {{SubSys, Level, time}, {time, Level, Value, Opts}}.

event(Session, Event, EvOpts, SessionOpts) when is_map(SessionOpts) ->
    gen_statem:call(Session, {Event, EvOpts, SessionOpts}, ?AAA_TIMEOUT).

%%===================================================================
%% API
%%===================================================================

start_link(Owner, SessionData) ->
    gen_statem:start_link(?MODULE, [Owner, SessionData], []).

invoke(Session, SessionOpts, Procedure, Opts) ->
    invoke(Session, SessionOpts, Procedure, Opts, proplists:get_bool(async, Opts)).

invoke(Session, SessionOpts, Procedure, Opts, true) ->
    gen_statem:cast(Session, {invoke, SessionOpts, Procedure, Opts});
invoke(Session, SessionOpts, Procedure, Opts, false) ->
    gen_statem:call(Session, {invoke, SessionOpts, Procedure, Opts}, ?AAA_TIMEOUT).

authenticate(Session, SessionOpts)
  when is_map(SessionOpts) ->
    case invoke(Session, SessionOpts, authenticate, [inc_session_id], false) of
	{ok, _, _} ->
	    success;
	{Other, _, _} ->
	    Other;
	Other ->
	    Other
    end.

authorize(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, authorize, [], false).

start(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, start, [], true).

start(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, start, Opts).

interim(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, interim, [], true).

interim(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, interim, Opts).

stop(Session, SessionOpts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, stop, [], true).

stop(Session, SessionOpts, Opts) when is_map(SessionOpts) ->
    invoke(Session, SessionOpts, stop, Opts).

terminate(Session) ->
    gen_statem:cast(Session, terminate).

get(Session) ->
    gen_statem:call(Session, get).

get(Session, Option) ->
    gen_statem:call(Session, {get, Option}).

set(Session, Option, Value) ->
    gen_statem:call(Session, {set, Option, Value}).

set(Session, Values) when is_map(Values) ->
    gen_statem:call(Session, {set, Values}).

sync(Session) ->
    gen_statem:call(Session, sync).

get_svc_opt(Service, Session) ->
    maps:get(Service, Session, #{}).

set_svc_opt(Service, Opts, Session) ->
    maps:put(Service, Opts, Session).

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

    DefaultSessionOpts =
	#{'Session-Start'       => erlang:monotonic_time(),
	  'Session-Id'          => SessionId,
	  'Multi-Session-Id'    => SessionId,
	  'Diameter-Session-Id' =>
	      diameter:session_id(OriginHost) ++ [";", integer_to_list(SessionId)]
	 },
    Data = #data{
	       owner         = Owner,
	       owner_monitor = MonRef,
	       application   = App,
	       session       = DefaultSessionOpts
	      },
    State = #state{pending = undefined},
    {Reply, Session, _Events} = exec(init, SessionOpts, [], Data),

    {Reply, State, Data#data{session = Session}}.

handle_event({call, From}, get, _State, Data) ->
    {keep_state_and_data, [{reply, From, Data#data.session}]};

handle_event({call, From}, {get, Opt}, _State, Data) ->
    {keep_state_and_data, [{reply, From, maps:find(Opt, Data#data.session)}]};

handle_event({call, From}, {set, Opt, Value}, _State, Data = #data{session = Session}) ->
    {keep_state, Data#data{session = maps:put(Opt, Value, Session)}, [{reply, From, ok}]};

handle_event({call, From}, {set, Values}, _State, Data) ->
    {keep_state, Data#data{session = maps:merge(Data#data.session, Values)}, [{reply, From, ok}]};

handle_event(info, {'EXIT', Pid, _Reason}, #state{pending = Pid} = State, Data) ->
    {next_state, State#state{pending = undefined}, Data};

handle_event(info, {'EXIT', _From, _Reason}, _State, _Data) ->
    %% ignore EXIT from eradius client
    keep_state_and_data;

handle_event(info, {'$invoke', Pid, Session, Events}, #state{pending = Pid} = State,
	     #data{owner = Owner} = Data) ->
    Owner ! {update_session, Session, Events},
    {next_state, State#state{pending = undefined}, Data#data{session = Session}};

handle_event(_Type, _Event, #state{pending = Pending}, _Data)
  when is_pid(Pending) ->
    {keep_state_and_data, postpone};

handle_event({call, From}, sync, _State, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};

handle_event(cast, terminate, _State, _Data) ->
    lager:info("Handling terminate request: ~p", [_Data]),
    {stop, normal};

handle_event(info, {'DOWN', _Ref, process, Owner, Info},
	     _State, #data{owner = Owner} = Data) ->
    lager:error("Received DOWN information for ~p with info ~p", [Data#data.owner, Info]),
    handle_owner_exit(Data),
    {stop, normal};

handle_event(info, {'EXIT', Owner, Reason},
	     _State, #data{owner = Owner} = Data) ->
    lager:error("Received EXIT signal for ~p with reason ~p", [Owner, Reason]),
    handle_owner_exit(Data),
    {stop, normal};

handle_event({call, From}, {invoke, SessionOpts, Procedure, Opts}, _State, Data) ->
    {Result, NewSession, Events} = exec(Procedure, SessionOpts, Opts, Data),
    Reply = {Result, NewSession, Events},
    {keep_state, Data#data{session = NewSession}, [{reply, From, Reply}]};

handle_event(cast, {invoke, SessionOpts, Procedure, Opts}, State, Data) ->
    Self = self(),
    Pid = proc_lib:spawn_link(
	    fun() ->
		    {_, Session, Events} = exec(Procedure, SessionOpts, Opts, Data),
		    Self ! {'$invoke', self(), Session, Events}
	    end),
    {next_state, State#state{pending = Pid}, Data};

handle_event({call, From}, {{Handler, _Level, time}, EvOpts, SessionOpts}, _State,
	     #data{session = Session0} = Data) ->

    Service = proplists:get_value(service, EvOpts, default),
    Procedure = proplists:get_value(procedure, EvOpts, interim),
    Session = maps:merge(Session0, SessionOpts),
    {Result, NewSession, Events} = Handler:invoke(Service, Procedure, Session, [], EvOpts),
    Reply = {Result, NewSession, Events},
    {keep_state, Data#data{session = NewSession}, [{reply, From, Reply}]};

handle_event(Type, Event, _State, _Data) ->
    lager:warning("unhandled event ~p:~p", [Type, Event]),
    keep_state_and_data.

terminate(Reason, Data) ->
    lager:error("ctld Session terminating with state ~p with reason ~p", [Data, Reason]),
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

-spec attr_get(Key :: term(), Session :: map()) -> {ok, Value :: any()} | error.
attr_get(Key, Session) ->
    maps:find(Key, Session).

attr_get(Key, Session, Default) ->
    maps:get(Key, Session, Default).

attr_set(Key, Value, Session) ->
    maps:put(Key, Value, Session).

attr_append(Key, Value, Session) ->
    maps:put(Key, Value, Session).

attr_fold(Fun, Acc, Session) ->
    maps:fold(Fun, Acc, Session).

merge(S1, S2) ->
    maps:merge(S1, S2).

%%===================================================================
%% internal helpers
%%===================================================================
prepare_next_session_id(Session) ->
    AcctAppId = maps:get('AAA-Application-Id', Session, default),
    Session#{'Session-Id' => ergw_aaa_session_seq:inc(AcctAppId)}.

handle_owner_exit(Data) ->
    action(stop, Data#data.session, Data).

%%===================================================================
%% provider helpers
%%===================================================================

exec(Procedure, SessionOpts, Opts, #data{session = SessionIn} = Data) ->
    Session0 = maps:merge(SessionIn, SessionOpts),
    Session1 = handle_session_opts(Opts, Session0),
    Session2 = update_session_state(Procedure, Session1),
    action(Procedure, Session2, Data).

native_to_seconds(Native) ->
    round(Native / erlang:convert_time_unit(1, second, native)).

handle_session_opts([], Session) ->
    Session;
handle_session_opts([inc_session_id|Tail], Session) ->
    handle_session_opts(Tail, prepare_next_session_id(Session));
handle_session_opts([_|Tail], Session) ->
    handle_session_opts(Tail, Session).

update_session_state(Procedure, Session) ->
    Now = erlang:monotonic_time(),
    update_accounting_state(
      Procedure,
      Session#{'Now' => erlang:monotonic_time(),
	       'Event-Timestamp' => native_to_seconds(Now + erlang:time_offset())}).

accounting_start(#{'Accounting-Start' := Start}) ->
    Start;
accounting_start(#{'Session-Start' := Start}) ->
    Start.

update_accounting_state(start, Session) ->
    Session#{'Accounting-Start' => maps:get('Now', Session)};
update_accounting_state(interim, Session) ->
    Start = accounting_start(Session),
    #{'Now' := Now} = Session,
    Session#{'Acct-Session-Time' => native_to_seconds(Now - Start),
	     'Last-Interim-Update' => Now};
update_accounting_state(stop, Session) ->
    Start = accounting_start(Session),
    #{'Now' := Now} = Session,
    Session#{'Acct-Session-Time' => native_to_seconds(Now - Start),
	     'Accounting-Stop' => Now};
update_accounting_state(_Procedure, Session) ->
    Session.

services(init, App) ->
    maps:get(session, App, []);
services(Procedure, App) ->
    Procedures = maps:get(procedures, App, #{}),
    maps:get(Procedure, Procedures, []).

action(Procedure, Session, #data{application = App} = _Data) ->
    Pipeline = services(Procedure, App),
    pipeline(Procedure, Session, [], Pipeline).

pipeline(_, Session, Events, []) ->
    {ok, Session, Events};
pipeline(Procedure, SessionIn, EventsIn, [Head|Tail]) ->
    case step(Head, Procedure, SessionIn, EventsIn) of
	{ok, SessionOut, EventsOut} ->
	    pipeline(Procedure, SessionOut, EventsOut, Tail);
	Other ->
	    Other
    end.

step({Service, Opts}, Procedure, Session, Events)
  when is_atom(Service) ->
    Svc = ergw_aaa_config:get_service(Service),
    Handler = maps:get(handler, Svc),
    Handler:invoke(Service, Procedure, Session, Events, Opts).
