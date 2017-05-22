%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ProviderSupSpecs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, ProviderSupSpecs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ProviderSupSpecs) ->
    {ok, {{one_for_one, 30, 60}, [?CHILD(ergw_aaa_session_seq, worker),
				  ?CHILD(ergw_aaa_session_sup, supervisor)
				  | ProviderSupSpecs]}}.

