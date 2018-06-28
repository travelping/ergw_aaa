%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_variable).

-compile({parse_transform, cut}).

-export([now_ms/0, now_ms/1]).
-export([new/4, set/2, get/1, update/2, stop_timers/1, rearm_timer/1, clear_triggers/1]).

-include("include/ergw_aaa_variable.hrl").

%%===================================================================
%% API
%%===================================================================

%% now in milliseconds
now_ms() ->
    erlang:system_time(milli_seconds).

now_ms({MegaSecs,Secs,MicroSecs}) ->
	MegaSecs * 1000000000 + Secs * 1000 + round(MicroSecs / 1000.0).

new(Name, Type, Value, TriggerDefs) ->
    Now = now_ms(),
    Triggers = lists:map(init_trigger(Now, Name, Type, Value, _), TriggerDefs),
    #var{name = Name, type = Type, value = Value, triggers = Triggers}.

set(Variable = #var{type = Type, name = Name, value = OldValue, triggers = Triggers}, NewValue) ->
    Now = now_ms(),
    NewTriggers = lists:map(process_trigger(Now, Name, Type, OldValue, NewValue, _), Triggers),
    Variable#var{type = Type, value = NewValue, triggers = NewTriggers}.

get(#var{value = Value}) ->
    Value.

update(Variable = #var{type = Type, value = Value}, Fun)
  when is_function(Fun, 2) ->
    set(Variable, Fun(Type, Value)).

stop_timers(Var0 = #var{triggers = Triggers0}) ->
    Now = now_ms(),
    {Triggers, Var} = lists:mapfoldl(stop_timer(Now, _, _), Var0, Triggers0),
    Var#var{triggers = Triggers}.

rearm_timer(Var0 = #var{type = Type, name = Name, triggers = Triggers0}) ->
    Now = now_ms(),
    {TriggerDefs, Var} = lists:mapfoldl(stop_timer(Now, _, _), Var0, Triggers0),
    Triggers1 = lists:map(init_trigger(Now, Name, Type, 0, _), TriggerDefs),
    Var#var{triggers = Triggers1}.

clear_triggers(Var = #var{triggers = Triggers}) ->
    lists:foreach(clear_trigger(_), Triggers),
    Var#var{triggers = []}.

%%===================================================================
%% Internal Helpers
%%===================================================================

process_trigger(_Now, _Name, _Type, OldValue, NewValue, Trigger = {Event, limit, Limit}) ->
    if OldValue < Limit andalso NewValue >= Limit ->
	    self() ! Event;
       true ->
	    ok
    end,
    Trigger;

process_trigger(_Now, _Name, Type, _OldValue, _NewValue, Trigger) ->
    lager:error("invalid trigger ~p for type ~p", [Trigger, Type]),
    Trigger.

start_timer(Time, Name, Event) ->
    erlang:start_timer(Time, self(), {'$trigger', Name, Event}).

%% Returns the remaing time for the timer if Ref referred to
%% an active timer/send_event_after, false otherwise.
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timeout, Ref, _} -> 0
	    after 0 -> false
	    end;
	RemainingTime ->
	    RemainingTime
    end.

init_trigger(Now, Name, timer, _Value, {Event, limit, TimeOut}) ->
    TimerRef = start_timer(TimeOut, Name, Event),
    {Event, limit, TimeOut, Now, TimerRef};
init_trigger(_Now, _Name, _Type, _Value, Trigger) ->
    Trigger.

stop_timer(Now, _Trigger = {Event, limit, TimeOut, StartTime, TimerRef}, Var = #var{type = timer}) ->
    cancel_timer(TimerRef),
    {{Event, limit, TimeOut}, Var#var{value = Now - StartTime}};

stop_timer(_Now, Trigger, Var) ->
    {Trigger, Var}.

clear_trigger({_Event, limit, _TimeOut, _StartTime, TimerRef}) ->
    cancel_timer(TimerRef);
clear_trigger({Event, limit, _Limit}) ->
    receive Event -> ok
    after 0 -> ok
    end.
