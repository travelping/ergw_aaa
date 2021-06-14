%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_config).

-compile({parse_transform, cut}).

%% API
-export([validate_options/3,
	 validate_option/2, validate_function/2,
	 validate_handler/2, validate_service/2,
	 validate_app/2, validate_rate_limit/2,
	 validate_answers/1]).
-export([to_map/1]).

-define(is_opts(X), (is_list(X) orelse is_map(X))).

-define(DefaultRateLimit, #{outstanding_requests => 50, rate => 50}).

%%%===================================================================
%%% API
%%%===================================================================

to_map(M) when is_map(M) ->
    M;
to_map(L) when is_list(L) ->
    lists:foldr(
      fun({K, V}, M) when not is_map_key(K, M) ->
	      M#{K => V};
	 (K, M) when is_atom(K) ->
	      M#{K => true};
	 (Opt, _) ->
	      erlang:error(badarg, [Opt])
      end, #{}, normalize_proplists(L)).

%% validate_options/3
validate_options(Fun, Options, Defaults)
  when ?is_opts(Options), ?is_opts(Defaults) ->
    Opts = maps:merge(to_map(Defaults), to_map(Options)),
    validate_options(Fun, Opts).

%%%===================================================================
%%% Options Validation
%%%===================================================================

normalize_proplists(L0) ->
    L = proplists:unfold(L0),
    proplists:substitute_negations([{disable, enable}], L).

validate_apply(M, F, A) ->
    case code:ensure_loaded(M) of
	{module, _} ->
	    ok;
	_ ->
	    erlang:error(badarg, [M, A])
    end,
    case erlang:function_exported(M, F, length(A)) of
	true ->
	    apply(M, F, A);
	false ->
	    erlang:error(badarg, [M, F, A])
    end.

validate_option(Fun, Opt, Value) when is_function(Fun, 2) ->
    {Opt, Fun(Opt, Value)};
validate_option(Fun, Opt, Value) when is_function(Fun, 1) ->
    Fun({Opt, Value}).

validate_options(Fun, M) when is_map(M) ->
    maps:fold(
      fun(K0, V0, A) ->
	      {K, V} = validate_option(Fun, K0, V0),
	      A#{K => V}
      end, #{}, M);
validate_options(_Fun, []) ->
    [];
%% validate_options(Fun, [Opt | Tail]) when is_atom(Opt) ->
%%     [validate_option(Fun, Opt, true) | validate_options(Fun, Tail)];
validate_options(Fun, [{Opt, Value} | Tail]) ->
    [validate_option(Fun, Opt, Value) | validate_options(Fun, Tail)].

validate_option(product_name, Value)
  when is_list(Value); is_binary(Value) ->
    Value;
validate_option(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

validate_function(_Function, Opts)
  when is_map(Opts) ->
    Handler = maps:get(handler, Opts, undefined),
    validate_apply(Handler, ?FUNCTION_NAME, [Opts]);
validate_function(Function, Opts)
  when is_list(Opts) ->
    validate_function(Function, to_map(Opts));
validate_function(Function, Opts) ->
    erlang:error(badarg, [Function, Opts]).

validate_handler(Handler, Opts)
  when is_atom(Handler) ->
    validate_apply(Handler, ?FUNCTION_NAME, [Opts]);
validate_handler(Handler, Opts) ->
    erlang:error(badarg, [Handler, Opts]).

validate_service(Service, Opts)
  when is_map(Opts) ->
    Handler = maps:get(handler, Opts, undefined),
    case ergw_aaa:get_handler(Handler) of
	HandlerOpts when is_map(HandlerOpts) ->
	    validate_apply(Handler, ?FUNCTION_NAME, [Service, HandlerOpts, Opts]);
	_ ->
	    erlang:error(badarg, [Service, {handler, Handler}])
    end;
validate_service(Service, Opts)
  when is_list(Opts) ->
    validate_service(Service, to_map(Opts));
validate_service(Service, Opts) ->
    erlang:error(badarg, [Service, Opts]).

validate_app(App, Procedures)
  when is_map(Procedures) ->
    validate_options(validate_app_procs_option(App, _, _), Procedures, []);
validate_app(App, Procedures)
  when is_list(Procedures) ->
    validate_app(App, to_map(Procedures));
validate_app(App, Procedures) ->
    erlang:error(badarg, [App, Procedures]).

validate_app_procs_option(App, Procedure, Services)
  when is_list(Services) ->
    lists:map(validate_app_procs_svc(App, Procedure, _), Services);
validate_app_procs_option(App, Procedure, Services) ->
    erlang:error(badarg, [App, Procedure, Services]).

validate_app_procs_svc(App, Procedure, #{service := Service} = Opts) ->
    case ergw_aaa:get_service(Service) of
	#{handler := Handler} = SvcOpts ->
	    validate_apply(Handler, validate_procedure,
			   [App, Procedure, Service, SvcOpts, Opts]);
	_ ->
	    erlang:error(badarg, [App, Procedure, Service])
    end;
validate_app_procs_svc(App, Procedure, Opts)
  when is_list(Opts) ->
    validate_app_procs_svc(App, Procedure, to_map(Opts));
validate_app_procs_svc(App, Procedure, Service) ->
    erlang:error(badarg, [App, Procedure, Service]).

validate_rate_limit(K = default, Opts) when ?is_opts(Opts) ->
    validate_options(validate_rate_limit_option(K, _, _), Opts, ?DefaultRateLimit);
validate_rate_limit(Peer, Opts) when is_binary(Peer), ?is_opts(Opts) ->
    validate_options(validate_rate_limit_option(Peer, _, _), Opts, ?DefaultRateLimit);
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

validate_answers(Answers) when is_map(Answers) ->
    maps:map(fun validate_answer/2, Answers).

validate_answer(_, V) when is_map(V) ->
    validate_options(fun validate_answer_opt/2, V, []);
validate_answer(K, V) ->
    erlang:error(badarg, [K, V]).

validate_answer_opt(avps, AVPs) when is_map(AVPs) ->
    AVPs;
validate_answer_opt(state, ocs_hold = V) ->
    V;
validate_answer_opt(K, V) ->
    erlang:error(badarg, [K, V]).
