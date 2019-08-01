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

-define(DefaultRateLimit, [{outstanding_requests, 50},
			   {rate, 50}
			  ]).
-define(DefaultOptions, [{product_name, "erGW-AAA"},
			 {functions, []}
			]).

-define(is_opts(X), (is_list(X) orelse is_map(X))).
-define(non_empty_opts(X), ((is_list(X) andalso length(X) /= 0) orelse
			    (is_map(X) andalso map_size(X) /= 0))).

%%%===================================================================
%%% API
%%%===================================================================

load_config() ->
    Config0 = setup:get_all_env(ergw_aaa),
    Config = validate_config(Config0),
    maps:map(fun(K,V) -> application:set_env(ergw_aaa, K, V) end, Config),
    Config.

%% opts_fold(Fun, AccIn, Opts) when is_list(Opts) ->
%%     lists:foldl(fun({K,V}, Acc) -> Fun(K, V, Acc) end, AccIn, Opts);
%% opts_fold(Fun, AccIn, Opts) when is_map(Opts) ->
%%     maps:fold(Fun, AccIn, Opts).

validate_options(Fun, Options, Defaults, ReturnType)
  when is_list(Options), ?is_opts(Defaults) ->
    Opts0 = proplists:unfold(Options),
    Opts = lists:ukeymerge(1, lists:keysort(1, Opts0), lists:keysort(1, to_list(Defaults))),
    return_type(validate_options(Fun, Opts), ReturnType);
validate_options(Fun, Options, Defaults, ReturnType)
  when is_map(Options) andalso ?is_opts(Defaults) ->
    Opts = maps:to_list(maps:merge(to_map(Defaults), Options)),
    return_type(validate_options(Fun, Opts), ReturnType).

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

%% get_opt(Key, List) when is_list(List) ->
%%     proplists:get_value(Key, List);
%% get_opt(Key, Map) when is_map(Map) ->
%%     maps:get(Key, Map).

get_opt(Key, List, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default);
get_opt(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default).

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
	    throw({error, {options, {Key, Duplicate}}})
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
    Config1 = validate_keyed_opt(functions, fun validate_function/2, Config0, []),
    Config2 = validate_keyed_opt(handlers, fun validate_handler/2, Config1, []),
    Config3 = validate_keyed_opt(services, validate_service(_, _, Config2), Config2, []),
    Config4 = validate_keyed_opt(apps, validate_app(_, _, Config3), Config3, []),
    Config = validate_keyed_opt(rate_limits, fun validate_rate_limit/2, Config4,
				[{default, ?DefaultRateLimit}]),
    validate_options(fun validate_option/2, Config, ?DefaultOptions, map).

validate_option(product_name, Value)
  when is_list(Value); is_binary(Value) ->
    Value;
validate_option(Opt, Value)
  when Opt == product_name ->
    throw({error, {options, {Opt, Value}}});
validate_option(_Opt, Value) ->
    Value.

validate_keyed_opt(Key, Fun, Config, Default) ->
    case get_opt(Key, Config, Default) of
	Values when ?is_opts(Values) ->
	    check_unique_keys(Key, Values),
	    V = validate_options(Fun, Values, [], map),
	    set_opt(Key, V, Config);
	Values ->
	    throw({error, {options, {Key, Values}}})
    end.

validate_function(Function, Opts)
  when is_atom(Function) ->
    Handler = get_opt(handler, Opts, undefined),
    case code:ensure_loaded(Handler) of
	{module, _} ->
	    ok;
	_ ->
	    throw({error, {options, {Handler, Opts}}})
    end,
    OOut = Handler:validate_function(without_opts([handler], Opts)),
    set_opt(handler, Handler, OOut).

validate_handler(Handler, Opts)
  when is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
	{module, _} ->
	    ok;
	_ ->
	    throw({error, {options, {Handler, Opts}}})
    end,
    Handler:validate_handler(Opts).

validate_service(Service, Opts, Config) ->
    Handlers = get_opt(handlers, Config, #{}),
    Handler = get_opt(handler, Opts, undefined),
    case maps:get(Handler, Handlers, undefined) of
	HandlerOpts when is_map(HandlerOpts) ->
	    OOut = Handler:validate_service(Service, HandlerOpts, without_opts([handler], Opts)),
	    set_opt(handler, Handler, OOut);
	_ ->
	    throw({error, {options, {Service, {handler, Handler}}}})
    end.

validate_app(App, Opts, Config) ->
    validate_options(validate_app_option(App, _, _, Config), Opts, [], map).

validate_app_option(App, session, Services, Config)
  when is_list(Services) ->
    validate_options(validate_app_procs_svc(App, init, _, _, Config), Services, [], list);
validate_app_option(App, procedures, Procedures, Config)
  when ?is_opts(Procedures) ->
    validate_options(validate_app_procs_option(App, _, _, Config), Procedures, [], map);
validate_app_option(App, Opt, Value, _Config) ->
    throw({error, {options, {App, Opt, Value}}}).

validate_app_procs_option(App, Procedure, Services, Config)
  when is_list(Services) ->
    validate_options(validate_app_procs_svc(App, Procedure, _, _, Config), Services, [], list);
validate_app_procs_option(App, Procedure, Services, _Config) ->
    throw({error, {options, {App, Procedure, Services}}}).

validate_app_procs_svc(App, Procedure, Service, true, Config) ->
    validate_app_procs_svc(App, Procedure, Service, [], Config);
validate_app_procs_svc(App, Procedure, Service, Opts, Config)
  when ?is_opts(Opts) ->
    case get_opt(services, Config, undefined) of
	#{Service := SvcOpts} ->
	    Handler = maps:get(handler, SvcOpts),
	    Handler:validate_procedure(App, Procedure, Service,
				       without_opts([handler], SvcOpts), Opts);
	_ ->
	    throw({error, {options, {App, Procedure, Service}}})
    end;
validate_app_procs_svc(App, Procedure, Service, Opts, _Config) ->
    throw({error, {options, {App, Procedure, Service, Opts}}}).

validate_rate_limit(RateLimit, Opts) ->
    validate_options(validate_rate_limit_option(RateLimit, _, _),
		     Opts, ?DefaultRateLimit, map).

validate_rate_limit_option(_RateLimit, outstanding_requests, Reqs)
  when is_integer(Reqs) ->
    Reqs;
validate_rate_limit_option(_RateLimit, rate, Rate)
  when is_integer(Rate) ->
    Rate;
validate_rate_limit_option(RateLimit, Opt, Value) ->
    throw({error, {options, {RateLimit, Opt, Value}}}).
