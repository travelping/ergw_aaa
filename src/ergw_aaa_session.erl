%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_session).

-behaviour(gen_server).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% Session Process API
-export([start_link/2, authenticate/2, authorize/2,
	 start/2, interim/2, interim_batch/2, stop/2,
	 terminate/1, get/1, get/2, set/2, set/3]).

%% Session Object API
-export([attr_get/2, attr_get/3, attr_set/3,
	 attr_append/3, attr_fold/3,
	 merge/2, to_session/1]).

-record(state, {'$version' = 0,
		owner,
		owner_monitor,
		authenticated = false,
		started = false,
		reply,
		interim_timer,
		session}).

-define(AAA_TIMEOUT, (30 * 1000)).      %% 30sec for all AAA gen_server:call timeouts

-include("include/ergw_aaa_profile.hrl").

%%===================================================================
%% API
%%===================================================================

start_link(Owner, SessionData) ->
    gen_server:start_link(?MODULE, [Owner, SessionData], []).

authenticate(Session, SessionOpts) when is_map(SessionOpts) ->
    gen_server:call(Session, {authenticate, SessionOpts}, ?AAA_TIMEOUT).

authorize(Session, SessionOpts) when is_map(SessionOpts) ->
    gen_server:call(Session, {authorize, SessionOpts}, ?AAA_TIMEOUT).

start(Session, SessionOpts) when is_map(SessionOpts) ->
    gen_server:cast(Session, {start, SessionOpts}).

interim(Session, SessionOpts) when is_map(SessionOpts) ->
    gen_server:cast(Session, {interim, SessionOpts}).

interim_batch(Session, SessionOptsList) when is_list(SessionOptsList) ->
    gen_server:cast(Session, {interim_batch, SessionOptsList}).

stop(Session, SessionOpts) when is_map(SessionOpts) ->
    gen_server:cast(Session, {stop, SessionOpts}).

terminate(Session) ->
    gen_server:cast(Session, terminate).

get(Session) ->
    gen_server:call(Session, get).

get(Session, Option) ->
    gen_server:call(Session, {get, Option}).

set(Session, Option, Value) ->
    gen_server:call(Session, {set, Option, Value}).

set(Session, Values) when is_map(Values) ->
    gen_server:call(Session, {set, Values}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([Owner, SessionOpts]) ->
    process_flag(trap_exit, true),

    AcctAppId = maps:get('AAA-Application-Id', SessionOpts, default),
    SessionId = ergw_aaa_session_seq:inc(AcctAppId),
    MonRef = erlang:monitor(process, Owner),

    DefaultSessionOpts = #{
      'Session-Id'         => SessionId,
      'Multi-Session-Id'   => SessionId,
      'Service-Type'       => 'Framed-User',
      'Framed-Protocol'    => 'PPP',
      'Interim-Accounting' => 10 * 1000
     },
    Session = maps:merge(DefaultSessionOpts, SessionOpts),
    State = #state{
	       owner         = Owner,
	       owner_monitor = MonRef,
	       session       = Session},
    {ok, State}.

handle_call({authenticate, SessionOpts}, From, State0) ->
    NewSessionOpts = prepare_next_session_id(maps:merge(State0#state.session, SessionOpts)),
    {Reply, NewSession} = ?action('Authenticate', NewSessionOpts),
    State1 = State0#state{session = NewSession},
    case Reply of
	ok    -> {noreply, State1#state{reply = From}};
	Other -> {reply, Other, State1}
    end;

handle_call({authorize, SessionOpts}, _From, State0) ->
    State1 = State0#state{session = maps:merge(State0#state.session, SessionOpts)},
    Reply =
	if State1#state.authenticated -> ok;
	    true                      -> not_authorized
	end,
    {reply, Reply, State1};

handle_call(get, _From, State) ->
    {reply, State#state.session, State};

handle_call({get, Opt}, _From, State) ->
    {reply, maps:find(Opt, State#state.session), State};

handle_call({set, Opt, Value}, _From, State = #state{session = Session}) ->
    {reply, ok, State#state{session = maps:put(Opt, Value, Session)}};

handle_call({set, Values}, _From, State) ->
    {reply, ok, State#state{session = maps:merge(State#state.session, Values)}};

handle_call(Event, _From, State) ->
    lager:warning("unhandled call ~p", [Event]),
    {reply, ok, State}.

handle_cast({start, SessionOpts}, State) ->
    NewSessionOpts = maps:merge(State#state.session, SessionOpts),
    {_, NewSession} = ?action('Account', 'Start', NewSessionOpts),
    State0 = State#state{session = NewSession, started = true},
    State1 = start_interim_accounting(State0),
    {noreply, State1};

handle_cast({interim, SessionOpts}, State) ->
    NewSessionOpts = maps:merge(State#state.session, SessionOpts),
    {_, NewSession} = ?action('Account', 'Interim', NewSessionOpts),
    {noreply, State#state{session = NewSession}};

handle_cast({interim_batch, SessionOptsList}, State) ->
    NewSession =
	lists:foldr(fun(SOOptsEntry, S0) ->
			    NewS = maps:merge(S0, SOOptsEntry),
			    {_, S1} = ?action('Account', 'Interim', NewS),
			    S1
		    end, State#state.session, SessionOptsList),
    {noreply, State#state{session = NewSession}};

handle_cast({stop, SessionOpts}, State) ->
    State0 = stop_interim_accounting(State),
    NewSessionOpts = maps:merge(State#state.session, SessionOpts),
    {_, NewSession} = ?action('Account', 'Stop', NewSessionOpts),
    {noreply, State0#state{session = NewSession, started = false}};

handle_cast(terminate, State) ->
    lager:info("Handling terminate request: ~p", [State]),
    {stop, normal, State};

handle_cast(Event, State) ->
    io:format("~p(~p): got ~p~n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Owner, Info},
	    State = #state{owner = Owner}) ->
    lager:error("Received DOWN information for ~p with info ~p", [Owner, Info]),
    handle_owner_exit(State),
    {stop, normal, State};

handle_info({'EXIT', Owner, Reason}, State = #state{owner = Owner}) ->
    lager:error("Received EXIT signal for ~p with reason ~p", [Owner, Reason]),
    handle_owner_exit(State),
    {stop, normal, State};

handle_info({'EXIT', _From, _Reason}, State) ->
    %% ignore EXIT from eradius client
    {noreply, State};

handle_info({'AuthenticationRequestReply', _AuthReply}, State = #state{authenticated = true}) ->
    %% already authenticated, ignore
    {noreply, State};

handle_info(Ev = {'AuthenticationRequestReply', _AuthReply}, State0) ->
    {Reply, Authenticated, NewSession} = ergw_aaa_profile:handle_reply(fun event/2, Ev, State0#state.session),
    State1 = State0#state{session = NewSession, authenticated = Authenticated},
    State2 = reply(Reply, State1),
    {noreply, State2};

handle_info({timeout, TimerRef, interim},
	    State = #state{
		       owner = Owner,
		       interim_timer = TimerRef,
		       session = Session}) ->
    case Session of
	#{'Accouting-Update-Fun' := UpdateFun}
	  when is_function(UpdateFun, 2) ->
	    NewSessionOpts = UpdateFun(Owner, Session),
	    lager:debug("InternalInterimUpdate: ~p -> ~p", [Session, NewSessionOpts]),
	    {_, NewSession} = ?action('Account', 'Interim', NewSessionOpts),
	    State0 = State#state{session = NewSession},
	    State1 = restart_interim_accounting(State0),
	    {noreply, State1};
	_ ->
	    State0 = stop_interim_accounting(State),
	    {noreply, State0}
    end;

handle_info({timeout, TimerRef, interim}, State = #state{interim_timer = TimerRef}) ->
    State0 = stop_interim_accounting(State),
    {noreply, State0};

handle_info(Info, State) ->
    lager:warning("Received unhandled message ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:error("ctld Session terminating with state ~p with reason ~p", [State, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------

reply(Reply, State = #state{reply = undefined}) ->
    State#state.owner ! Reply,
    State;
reply(Reply, State = #state{reply = ReplyTo}) ->
    gen_server:reply(ReplyTo, Reply),
    State#state{reply = undefined}.

event({'AuthenticationRequestReply', {Verdict, SessionOpts}}, Session0) ->
    Session1 = maps:without(['EAP-Data'], Session0),
    Session = maps:merge(Session1, SessionOpts),
    {Verdict, Verdict == success, Session}.

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
prepare_next_session_id(State) ->
    AcctAppId = maps:get('AAA-Application-Id', State, default),
    State#{'Session-Id' => ergw_aaa_session_seq:inc(AcctAppId)}.

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

start_interim_accounting(State) ->
    cancel_timer(State#state.interim_timer),
    case State#state.session of
	#{'Interim-Accounting'   := InterimAccounting,
	  'Accouting-Update-Fun' := UpdateFun}
	  when is_integer(InterimAccounting), is_function(UpdateFun, 2) ->
	    State#state{interim_timer = start_timer(InterimAccounting, interim)};
	_ ->
	    State
    end.

stop_interim_accounting(State) ->
    cancel_timer(State#state.interim_timer),
    State#state{interim_timer = undefined}.

restart_interim_accounting(State) ->
    case State#state.session of
	#{'Interim-Accounting' := InterimAccounting}
	  when is_integer(InterimAccounting) ->
	    State#state{interim_timer = start_timer(InterimAccounting, interim)};
	_ ->
	    State#state{interim_timer = undefined}
    end.

handle_owner_exit(State)
  when State#state.authenticated orelse State#state.started ->
    ?action('Account', 'Stop', State#state.session);
handle_owner_exit(_State) ->
    ok.
