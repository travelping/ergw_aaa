-module(ctld_session).

-behaviour(gen_server).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% Session Process API
-export([start_link/5, authenticate/2, authorize/2, start/2, stop/2, terminate/1, get/1, get/2, set/2, set/3]).

%% Session Object API
-export([attr_get/2, attr_get/3, attr_set/3,
	 attr_append/3, attr_fold/3,
	 merge/2, to_session/1]).

-record(state, {'$version' = 0,
		id,
		lmod, leader,
		a3handler, a3state,
		session,

		auth_state = stopped,
		session_timeout,
		interim_accounting
}).

-define(AAA_TIMEOUT, (30 * 1000)).      %% 30sec for all AAA gen_server:call timeouts
-define(SESSION_TIMERS, [{#state.session_timeout,    'Session-Timeout',    session_timeout},
			 {#state.interim_accounting, 'Interim-Accounting', interim_accounting}]).

%%===================================================================
%% API
%%===================================================================

start_link(LMod, Leader, A3Handler, A3Opts, SessionData) ->
    gen_server:start_link(?MODULE, [LMod, Leader, A3Handler, A3Opts, SessionData], []).

authenticate(Session, SessionOpts) when is_list(SessionOpts) ->
    gen_server:call(Session, {authenticate, SessionOpts}, ?AAA_TIMEOUT).

authorize(Session, SessionOpts) when is_list(SessionOpts) ->
    gen_server:call(Session, {authorize, SessionOpts}, ?AAA_TIMEOUT).

start(Session, SessionOpts) when is_list(SessionOpts) ->
    gen_server:cast(Session, {start, SessionOpts}).

stop(Session, SessionOpts) when is_list(SessionOpts) ->
    gen_server:cast(Session, {stop, SessionOpts}).

terminate(Session) ->
    gen_server:cast(Session, terminate).

get(Session) ->
    gen_server:call(Session, get).

get(Session, Option) ->
    gen_server:call(Session, {get, Option}).

set(Session, Option, Value) ->
    gen_server:call(Session, {set, Option, Value}).

set(Session, Values) when is_list(Values) ->
    gen_server:call(Session, {set, Values}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([LMod, Leader, A3Handler, A3Opts, SessionData]) ->
    process_flag(trap_exit, true),
    case A3Handler:init(A3Opts) of
	{ok, A3State} ->

%	    SessionId = session_id:new(),
	    SessionId = 1,
	    State = #state{
	      lmod = LMod,
	      leader = Leader,
	      id = SessionId,
	      a3handler = A3Handler,
	      a3state = A3State,
	      session = to_session([{'Session-Id', SessionId}|SessionData])
	     },
	    {ok, State};
	Other ->
	    {stop, Other}
    end.

handle_call({authenticate, SessionOpts}, From,
	    State = #state{session = Session}) ->

    a3call(authenticate, From, State#state{session = merge(Session, SessionOpts)});

handle_call({authorize, SessionOpts}, From,
	    State0 = #state{session = Session}) ->

    a3call(authorize, From, State0#state{session = merge(Session, SessionOpts)});

handle_call(get, _From, State = #state{session = Session}) ->
    {reply, Session, State};

handle_call({get, Opt}, _From, State = #state{session = Session}) ->
    {reply, attr_get(Opt, Session), State};

handle_call({set, Opt, Value}, _From, State0 = #state{session = Session}) ->
    State = State0#state{session = attr_set(Opt, Value, Session)},
    {reply, ok, State};

handle_call({set, Values}, _From, State0 = #state{session = Session}) ->
    State = State0#state{session = merge(Session, Values)},
    {reply, ok, State};

handle_call(Event, _From, State) ->
    io:format("~p(~p): got ~p~n", [?MODULE, ?LINE, Event]),
    {reply, ok, State}.

handle_cast({start, SessionOpts}, State0 = #state{session = Session}) ->
    State1 = a3cast(start, State0#state{session = merge(Session, SessionOpts)}),
    State = start_session_timers(State1),
    {noreply, State#state{auth_state = running}};

handle_cast({stop, SessionOpts}, State0 = #state{session = Session}) ->
    State1 = a3cast(stop, State0#state{session = merge(Session, SessionOpts)}),
    State = stop_session_timers(State1),
    %{stop, normal, State#state{auth_state = stopped}};
    {noreply, State#state{auth_state = stopped}};

handle_cast(terminate, State) ->
    lager:info("Handling terminate request: ~p", [State]),
    {stop, normal, State};

handle_cast(Event, State) ->
    io:format("~p(~p): got ~p~n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info({timeout, TimerRef, interim_accounting},
	    State0 = #state{lmod = LMod,
			    leader = Leader,
			    id = SessionId,
			    session = Session,
			    interim_accounting = TimerRef}) ->
    case LMod:interim_update(Leader, SessionId, Session) of
	{ok, UpdateSessionOpts} ->
	    State1 = a3cast(interim, State0#state{session = merge(Session, UpdateSessionOpts)}),
	    State = start_session_timer(#state.interim_accounting, attr_get('Interim-Accounting', Session),
					interim_accounting, State1),
	    {noreply, State};
	_ ->
	    {noreply, State0}
    end;

handle_info({timeout, TimerRef, session_timeout},
	    State = #state{lmod = LMod,
			   leader = Leader,
			   id = SessionId,
			   session = Session,
			   session_timeout = TimerRef}) ->
    LMod:session_timeout(Leader, SessionId, Session),
    {noreply, State#state{session_timeout = undefined}};

handle_info({'EXIT', From, Reason}, State = #state{leader = From, auth_state = AuthState}) ->
    lager:error("Received EXIT signal from ~p with reason ~p", [From, Reason]),
    if AuthState == running ->
           a3cast(stop, State),
           stop_session_timers(State);
       true ->
           ok
    end,
    {stop, normal, State};

handle_info(Info, State) ->
    lager:warning("Received unhandled message ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg("ctld Session terminating with state ~p with reason ~p~n", [State, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Session Object API and Helpers
%%
%% Purpose: hide the internals of Session Objects from the rest of
%%          the world
%%===================================================================

to_session(Session) when is_list(Session) ->
    orddict:from_list(Session).

-spec attr_get(Key :: term(), Value :: term()) -> {ok, Value :: any()} | error.
attr_get(Key, Session) ->
    orddict:find(Key, Session).

attr_get(Key, Session, Default) ->
    case orddict:find(Key, Session) of
	{ok, Value} ->
	    Value;
	error ->
	    Default
    end.

attr_set(Key, Value, Session) ->
    orddict:store(Key, Value, Session).

attr_append(Key, Value, Session) ->
    orddict:append(Key, Value, Session).

attr_fold(Fun, Acc, Session) ->
    orddict:fold(Fun, Acc, Session).

merge(S1, S2) ->
    orddict:merge(fun(_Key, _V1, V2) -> V2 end, S1, S2).

%%===================================================================
%% AAA Callback Module Helpers
%%===================================================================

a3call(F, From, State = #state{a3handler = A3Handler,
			       a3state = A3State0,
			       session = Session}) ->
    case A3Handler:F(From, Session, A3State0) of
	{noreply, A3State} ->
	    {noreply, State#state{a3state = A3State}};
	{reply, Value, A3State} ->
	    {Value, State#state{a3state = A3State}};
	{reply, Value, SessionOpts, A3State} ->
	    {reply, Value, State#state{
			     a3state = A3State,
			     session = merge(Session, SessionOpts)}}
    end.

a3cast(F, State = #state{a3handler = A3Handler,
			 a3state = A3State0,
			 session = Session}) ->
    case A3Handler:F(Session, A3State0) of
	{SessionOpts, A3State} ->
	    State#state{a3state = A3State,
			session = merge(Session, SessionOpts)};
	A3State ->
	    State#state{a3state = A3State}
    end.

%%===================================================================
%% internal helpers
%%===================================================================

start_timer(Time, Msg) ->
    erlang:start_timer(Time, self(), Msg).

cancel_timer(undefined) ->
    false;
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            receive {timeout, Ref, _} -> 0
            after 0 -> false
            end;
        RemainingTime ->
            RemainingTime
    end.

start_session_timer(Timer, {ok, Value}, Msg, State)
  when is_integer(Value) ->
    cancel_timer(element(Timer, State)),
    setelement(Timer, State, start_timer(Value, Msg));
start_session_timer(Timer, _, _, State) ->
    setelement(Timer, State, undefined).

start_session_timers([], State) ->
    State;
start_session_timers([{Timer, Key, Msg}|R], State0 = #state{session = Session}) ->
    Value = attr_get(Key, Session),
    State = start_session_timer(Timer, Value, Msg, State0),
    start_session_timers(R, State).

stop_session_timer(Timer, State) ->
    cancel_timer(element(Timer, State)),
    setelement(Timer, State, undefined).

stop_session_timers([], State) ->
    State;
stop_session_timers([{Timer, _, _}|R], State0) ->
    State = stop_session_timer(Timer, State0),
    stop_session_timers(R, State).

start_session_timers(State) ->
    start_session_timers(?SESSION_TIMERS, State).

stop_session_timers(State) ->
    stop_session_timers(?SESSION_TIMERS, State).
