%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa).

-type session() :: list({atom(), term()}) | map().
-type a3state() :: term().
-export_type([session/0, a3state/0]).

-callback validate_options(Options :: term()) -> term().
-callback initialize_provider(Options :: term()) -> {ok, [supervisor:child_spec()]} | {error, term()}.

-callback init(proplists:proplist()) -> {ok, a3state()} | {error, term()}.
-callback start_authentication(From :: term(), Session :: session(), State :: a3state()) ->
    {reply, a3state()}.
-callback authorize(From :: term(), Session :: session(), State :: a3state()) ->
    {noreply, term()} | {reply, term(), a3state()} | {reply, term(), session(), a3state()}.
-callback start_accounting(From :: term(), Type :: term(), Session :: session(), State :: a3state()) ->
    {ok, a3state()}.
