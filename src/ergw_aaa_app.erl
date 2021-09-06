%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ergw_aaa_sup:start_link() of
        {ok, _} = Ret ->
            prometheus_declare(),
            maybe_load_radius_callback(),
            Ret;
        Other ->
            Other
    end.

stop(_State) ->
    ok.

%%===================================================================
%% Internal
%%===================================================================

prometheus_declare() ->
    prometheus_gauge:declare([{name, aaa_sessions_total},
			      {labels, [handler, state]},
			      {help, "AAA sessions"}]).

maybe_load_radius_callback() ->
    case application:get_env(eradius, radius_callback, false) of
        false ->
            ok;
        Callback ->
            ok = eradius:modules_ready([Callback])
    end.
