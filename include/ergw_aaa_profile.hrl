-compile({parse_transform, cut}).

-define(queue_event(Event), self() ! Event).
-define(queue_event(To, Event), To ! Event).

-define(action(Action, State), ergw_aaa_profile:action(Action, State)).
-define(action(Action, ArgN1, State), ergw_aaa_profile:action(Action, [ArgN1], State)).
-define(action(Action, ArgN1, ArgN2, State), ergw_aaa_profile:action(Action, [ArgN1, ArgN2], State)).

