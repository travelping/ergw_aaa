%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_config).

-compile({parse_transform, cut}).

%% API
-export([load_config/1, validate_options/2, validate_options/3]).

-define(DefaultOptions, [{product_name, "erGW-AAA"},
                         {applications, [
                            {default, {provider, ergw_aaa_mock, []}}
                         ]}
                        ]).

%%%===================================================================
%%% API
%%%===================================================================

load_config(Config) ->
    validate_options(fun validate_option/2, Config, ?DefaultOptions).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_options(_Fun, []) ->
        [];
%% validate_options(Fun, [Opt | Tail]) when is_atom(Opt) ->
%%         [Fun(Opt, true) | validate_options(Fun, Tail)];
validate_options(Fun, [{Opt, Value} | Tail]) ->
        [{Opt, Fun(Opt, Value)} | validate_options(Fun, Tail)].

validate_options(Fun, Opts0, Defaults) ->
    Opts = lists:ukeymerge(1, lists:keysort(1, Opts0), lists:keysort(1, Defaults)),
    validate_options(Fun, Opts).

validate_option(product_name, Value) when not is_list(Value), not is_binary(Value) ->
    throw({error, {options, {product_name, Value}}});
validate_option(applications, Apps) when is_list(Apps) ->
    lists:map(fun validate_application/1, Apps);
validate_option(ergw_aaa_provider, {Handler, Opts} = Value)
  when is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
	{module, _} ->
	    ok;
	_ ->
	    throw({error, {options, Value}})
    end,
    {Handler, Handler:validate_options(Opts)};
validate_option(Opt, Value)
  when Opt == ergw_aaa_provider ->
    throw({error, {options, {Opt, Value}}});
validate_option(_Opt, Value) ->
    Value.

validate_application({AppId, {provider, Handler, Opts}} = Value)
  when is_atom(AppId), is_atom(Handler) ->
    case code:ensure_loaded(Handler) of
        {module, _} ->
            ok;
        _ ->
            throw({error, {options, Value}})
    end,
    {AppId, {provider, Handler, Handler:validate_options(Opts)}};
validate_application(Value) ->
    throw({error, {options, Value}}).
