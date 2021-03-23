%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa).

-compile({parse_transform, cut}).

%% API
-export([setopts/1, setopt/2,
	 add_function/2, add_handler/2, add_service/2, add_application/2,
	 get_function/1, get_handler/1, get_service/1, get_application/1]).

-ifdef(TEST).
-export([add_config/3]).
-endif.

-type session() :: map().
-export_type([session/0]).

%%====================================================================
%% behavior spec
%%====================================================================

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
		 Session :: session(), Events :: list(),
		 Opts :: map(), State :: term()) ->
    {ok | atom(), Result :: term(), session(), StateOut :: term()}.

-callback handle_response(Promise :: reference(), Result :: term(),
			  Session :: session(), Events :: list(),
			  Opts :: map(), StateIn :: term()) ->
    {ok | atom(), Result :: term(), session(), StateOut :: term()}.

%%====================================================================
%% API
%%====================================================================

-define(FUNCTION_KEY, functions).
-define(HANDLER_KEY,  handlers).
-define(SERVICE_KEY,  services).
-define(APP_KEY,      apps).

setopts(Opts0) when is_map(Opts0)->
    Opts = maps:map(fun ergw_aaa_config:validate_option/2, Opts0),
    maps:map(application:set_env(ergw_aaa, _, _), Opts),
    ok;
setopts(Opts) when is_list(Opts) ->
    setopts(ergw_aaa_config:to_map(Opts)).

setopt(rate_limits = Opt, Value0) ->
    Value = ergw_aaa_config:validate_options(
	      fun ergw_aaa_config:validate_rate_limit/2, Value0, []),
    application:set_env(ergw_aaa, Opt, Value);
setopt(Opt, Value0) ->
    Value = ergw_aaa_config:validate_option(Opt, Value0),
    application:set_env(ergw_aaa, Opt, Value).

add_function(Name, Opts0) ->
    #{handler := Handler} =
	Opts = ergw_aaa_config:validate_function(Name, Opts0),
    add_config(?FUNCTION_KEY, Name, Opts),
    {ok, SupSpec} = Handler:initialize_function(Name, Opts),
    ergw_aaa_sup:start_childs(SupSpec),
    ok.

add_handler(Handler, Opts0) ->
    Opts = ergw_aaa_config:validate_handler(Handler, Opts0),
    add_config(?HANDLER_KEY, Handler, Opts),
    {ok, SupSpec} = Handler:initialize_handler(Opts),
    ergw_aaa_sup:start_childs(SupSpec),
    ok.

add_service(Name, Opts0) ->
    #{handler := Handler} =
	Opts = ergw_aaa_config:validate_service(Name, Opts0),
    add_config(?SERVICE_KEY, Name, Opts),
    {ok, SupSpec} = Handler:initialize_service(Name, Opts),
    ergw_aaa_sup:start_childs(SupSpec),
    ok.

add_application(Name, Opts0) ->
    Opts = ergw_aaa_config:validate_app(Name, Opts0),
    add_config(?APP_KEY, Name, Opts).

get_function(Name) ->
    Functions = application:get_env(ergw_aaa, ?FUNCTION_KEY, undefined),
    maps:get(Name, Functions, undefined).

get_handler(Name) ->
    Handlers = application:get_env(ergw_aaa, ?HANDLER_KEY, undefined),
    maps:get(Name, Handlers, undefined).

get_service(Name) ->
    Services = application:get_env(ergw_aaa, ?SERVICE_KEY, #{}),
    maps:get(Name, Services, #{}).

get_application(Name) ->
    Apps = application:get_env(ergw_aaa, ?APP_KEY, #{}),
    maps:get(Name, Apps, #{}).

%%===================================================================
%% Internal
%%===================================================================

add_config(Key, Name, Opts) ->
    M = application:get_env(ergw_aaa, Key, #{}),
    if is_map_key(Name, M) ->
	    erlang:error(badarg, [Key, Name]);
       true ->
	    application:set_env(ergw_aaa, Key, maps:put(Name, Opts, M))
    end.
