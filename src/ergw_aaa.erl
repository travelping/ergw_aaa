%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa).

-type session() :: map().
-export_type([session/0]).

-callback validate_handler(Options :: list() | map()) -> map().
-callback validate_service(Service :: atom(), HandlerOpts :: map(),
			   Options :: list() | map()) -> map().
-callback validate_procedure(App :: atom(), Procedure :: atom(),
			     Service :: atom(), ServiceOpts :: map(),
			     SessionOptions :: list() | map()) -> map().

-callback initialize_handler(Options :: map()) ->
    {ok, [supervisor:child_spec()]} | {error, term()}.
-callback initialize_service(ServiceId :: atom(), Options :: map()) ->
    {ok, [supervisor:child_spec()]} | {error, term()}.

-callback invoke(ServiceId :: atom(), Procedure :: atom(),
		 Session :: session(), Events :: list(), Opts :: map()) ->
    {ok | atom(), session()}.
