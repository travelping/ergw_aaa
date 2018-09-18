%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_childs/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 30, 60}, [?CHILD(ergw_aaa_diameter_srv, worker),
				  ?CHILD(ergw_aaa_session_seq, worker),
				  ?CHILD(ergw_aaa_session_reg, worker),
				  ?CHILD(ergw_aaa_session_sup, supervisor)]}}.

start_childs(ProviderSupSpecs) ->
    [{ok, _} = supervisor:start_child(?SERVER, X) || X <- remove_dups(ProviderSupSpecs)].

%% ===================================================================

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), element(1, X) /= element(1, H)]].
