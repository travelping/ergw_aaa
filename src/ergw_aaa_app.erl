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
    Config = ergw_aaa_config:load_config(setup:get_all_env(ergw_aaa)),
    {ok, ProviderSupSpecs} = ergw_aaa_profile:initialize_provider(Config),
    ergw_aaa_sup:start_link(ProviderSupSpecs).

stop(_State) ->
    ok.
