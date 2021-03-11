%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_config).

-compile({parse_transform, cut}).

%% API
-export([load_config/0, validate_options/4,
	 get_service/1, get_application/1]).

-ifdef(TEST).
-export([validate_config/1]).
-endif.

-define(DefaultRateLimit, #{outstanding_requests => 50, rate => 50}).
-define(DefaultOptions, #{product_name => "erGW-AAA", functions => #{}}).

-define(is_opts(X), (is_list(X) orelse is_map(X))).

%%%===================================================================
%%% API
%%%===================================================================

load_config() ->
    Config0 = setup:get_all_env(ergw_aaa),
    Config = validate_config(maps:from_list(Config0)),
    maps:map(fun(K,V) -> application:set_env(ergw_aaa, K, V) end, Config),
    Config.

%% opts_fold(Fun, AccIn, Opts) when is_list(Opts) ->
%%     lists:foldl(fun({K,V}, Acc) -> Fun(K, V, Acc) end, AccIn, Opts);
%% opts_fold(Fun, AccIn, Opts) when is_map(Opts) ->
%%     maps:fold(Fun, AccIn, Opts).

validate_options(Fun, Options, Defaults, list)
  when is_list(Options), ?is_opts(Defaults) ->
    Opts0 = proplists:unfold(Options),
    Opts = lists:ukeymerge(1, lists:keysort(1, Opts0), lists:keysort(1, to_list(Defaults))),
    return_type(validate_options(Fun, Opts), list);
validate_options(Fun, Options, Defaults, map)
  when is_map(Options) andalso ?is_opts(Defaults) ->
    Opts = maps:to_list(maps:merge(to_map(Defaults), Options)),
    return_type(validate_options(Fun, Opts), map).

get_service(Service) ->
    Services = application:get_env(ergw_aaa, services, #{}),
    maps:get(Service, Services, #{}).

get_application(AppId) ->
    Apps = application:get_env(ergw_aaa, apps, #{}),
    maps:get(AppId, Apps, #{}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

return_type(List, list) when is_list(List) ->
    List;
return_type(List, map) when is_list(List) ->
    maps:from_list(List);
return_type(Map, map) when is_map(Map) ->
    Map;
return_type(Map, list) when is_map(Map) ->
    maps:to_list(Map).

to_map(List) when is_list(List) ->
    maps:from_list(List);
to_map(Map) when is_map(Map) ->
    Map.

to_list(Map) when is_map(Map) ->
    maps:to_list(Map);
to_list(List) when is_list(List) ->
    List.

set_opt(Key, Value, List) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Value});
set_opt(Key, Value, Map) when is_map(Map) ->
    Map#{Key => Value}.

without_opts(Keys, List) when is_list(List) ->
    [X || X <- List, not lists:member(element(1, X), Keys)];
without_opts(Keys, Map) when is_map(Map) ->
    maps:without(Keys, Map).

check_unique_keys(_Key, Map) when is_map(Map) ->
    ok;
check_unique_keys(Key, List) when is_list(List) ->
    UList = lists:ukeysort(1, List),
    if length(UList) == length(List) ->
	    ok;
       true ->
	    Duplicate = proplists:get_keys(List) -- proplists:get_keys(UList),
	    erlang:error(badarg, [Key, Duplicate])
    end.

validate_option(Fun, Opt, Value) when is_function(Fun, 2) ->
    {Opt, Fun(Opt, Value)};
validate_option(Fun, Opt, Value) when is_function(Fun, 1) ->
    Fun({Opt, Value}).

validate_options(_Fun, []) ->
    [];
%% validate_options(Fun, [Opt | Tail]) when is_atom(Opt) ->
%%     [validate_option(Fun, Opt, true) | validate_options(Fun, Tail)];
validate_options(Fun, [{Opt, Value} | Tail]) ->
    [validate_option(Fun, Opt, Value) | validate_options(Fun, Tail)].

validate_config(Config0) ->
    Config1 = validate_keyed_opt(functions, fun validate_function/2, Config0, #{}),
    Config2 = validate_keyed_opt(handlers, fun validate_handler/2, Config1, #{}),
    Config3 = validate_keyed_opt(services, validate_service(_, _, Config2), Config2, #{}),
    Config4 = validate_keyed_opt(apps, validate_app(_, _, Config3), Config3, #{}),
    Config = validate_keyed_opt(rate_limits, fun validate_rate_limit/2, Config4,
				#{<<"default">> => ?DefaultRateLimit}),
    validate_options(fun validate_option/2, Config, ?DefaultOptions, map).

validate_option(product_name, Value)
  when is_list(Value); is_binary(Value) ->
    Value;
validate_option(Opt, Value)
  when Opt == product_name ->
    erlang:error(badarg, [Opt, Value]);
validate_option(_Opt, Value) ->
    Value.

validate_keyed_opt(Key, Fun, Config, Default) when is_map(Config) ->
    case maps:get(Key, Config, Default) of
	Values when is_map(Values) ->
	    check_unique_keys(Key, Values),
	    V = validate_options(Fun, Values, [], map),
	    set_opt(Key, V, Config);
	Values ->
	    erlang:error(badarg, [Key, Values])
    end;
validate_keyed_opt(Key, _, Config, _) ->
    erlang:error(badarg, [Key, Config]).

validate_function(Function, Opts)
  when is_binary(Function), is_map(Opts) ->
    Handler = maps:get(handler, Opts, undefined),
    case code:ensure_loaded(Handler) of
	{module, _} ->
	    ok;
	_ ->
	    erlang:error(badarg, [Handler, Opts])
    end,
    OOut = Handler:validate_function(without_opts([handler], Opts)),
    set_opt(handler, Handler, OOut).

validate_handler(Handler, Opts)
  when is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
	{module, _} ->
	    ok;
	_ ->
	    erlang:error(badarg, [Handler, Opts])
    end,
    Handler:validate_handler(Opts).

validate_service(Service, Opts, Config)
  when is_binary(Service), is_map(Opts), is_map(Config) ->
    Handlers = maps:get(handlers, Config, #{}),
    Handler = maps:get(handler, Opts, undefined),
    case maps:get(Handler, Handlers, undefined) of
	HandlerOpts when is_map(HandlerOpts) ->
	    OOut = Handler:validate_service(Service, HandlerOpts, without_opts([handler], Opts)),
	    set_opt(handler, Handler, OOut);
	_ ->
	    erlang:error(badarg, [Service, {handler, Handler}])
    end;
validate_service(Service, Opts, _) ->
    erlang:error(badarg, [Service, Opts]).

validate_app(App, Procedures, Config)
  when is_binary(App), is_map(Procedures) ->
    validate_options(validate_app_procs_option(App, _, _, Config), Procedures, [], map);
validate_app(App, Procedures, _) ->
    erlang:error(badarg, [App, Procedures]).

validate_app_procs_option(App, Procedure, Services, Config)
  when is_list(Services) ->
    lists:map(validate_app_procs_svc(App, Procedure, _, Config), Services);
validate_app_procs_option(App, Procedure, Services, _Config) ->
    erlang:error(badarg, [App, Procedure, Services]).

validate_app_procs_svc(App, Procedure, Service, Config)
  when is_binary(Service) ->
    validate_app_procs_svc(App, Procedure, {Service, #{}}, Config);
validate_app_procs_svc(App, Procedure, {Service, Opts}, Config)
  when is_binary(Service), is_map(Opts) ->
    case maps:get(services, Config, undefined) of
	#{Service := SvcOpts} ->
	    Handler = maps:get(handler, SvcOpts),
	    {Service, Handler:validate_procedure(App, Procedure, Service,
						 without_opts([handler], SvcOpts), Opts)};
	_ ->
	    erlang:error(badarg, [App, Procedure, Service])
    end;
validate_app_procs_svc(App, Procedure, Service, _Config) ->
    erlang:error(badarg, [App, Procedure, Service]).

validate_rate_limit(RateLimit, Opts)
  when is_binary(RateLimit) ->
    validate_options(validate_rate_limit_option(RateLimit, _, _),
		     Opts, ?DefaultRateLimit, map);
validate_rate_limit(RateLimit, Opts) ->
    erlang:error(badarg, [RateLimit, Opts]).

validate_rate_limit_option(_RateLimit, outstanding_requests, Reqs)
  when is_integer(Reqs) andalso Reqs > 0 ->
    Reqs;
validate_rate_limit_option(_RateLimit, rate, Rate)
  when is_integer(Rate) andalso Rate > 0 andalso Rate < 100000 ->
    Rate;
validate_rate_limit_option(RateLimit, Opt, Value) ->
    erlang:error(badarg, [RateLimit, Opt, Value]).

%%%===================================================================
