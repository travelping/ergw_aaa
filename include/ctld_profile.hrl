-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-define(register(ProfileName), ctld_behavior:register(?MODULE, ProfileName)).
-define(queue_event(Event), self() ! Event).
-define(queue_event(To, Event), To ! Event).

-define(stop(Reason, State), {stop, Reason, State}).
-define(abort(Reason, State), {stop, {error, Reason}, State}).

-define(next(NextProfileName, State), {next_profile, NextProfileName, State}).

-define(action(Action, State), ctld_profile:action(Action, State)).
-define(action(Action, ArgN1, State), ctld_profile:action(Action, [ArgN1], State)).
-define(action(Action, ArgN1, ArgN2, State), ctld_profile:action(Action, [ArgN1, ArgN2], State)).

-define(sys_event(Event, State), Ev).

-define(def_timer_event(Variable, Event, TimerRef), {timeout, TimerRef, {'$trigger', Variable, Event}}).

-define(call(What), {'$gen_call', _, What}).
-define(handle_call(What, State), Ev = {'$gen_call', _, What} -> {Ev, State}).

