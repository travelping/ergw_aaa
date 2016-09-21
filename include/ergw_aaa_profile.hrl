%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-compile({parse_transform, cut}).

-define(queue_event(Event), self() ! Event).
-define(queue_event(To, Event), To ! Event).

-define(action(Action, State), ergw_aaa_profile:action(Action, State)).
-define(action(Action, ArgN1, State), ergw_aaa_profile:action(Action, [ArgN1], State)).
-define(action(Action, ArgN1, ArgN2, State), ergw_aaa_profile:action(Action, [ArgN1, ArgN2], State)).

