%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa).

-compile({parse_transform, cut}).

%% API
-export([
	peer_info/0,
	peer_info_adjust/1,
	setopts/1, setopt/2,
	 add_function/2, add_handler/2, add_service/2, add_application/2,
	 get_function/1, get_handler/1, get_service/1, get_application/1]).

-ifdef(TEST).
-export([add_config/3]).
-endif.

-type peer_info() :: map().
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

-include_lib("diameter/include/diameter.hrl").
-include_lib("kernel/include/logger.hrl").
-include( "ergw_aaa_internal.hrl" ).

%%====================================================================
%% API
%%====================================================================

-define(FUNCTION_KEY, functions).
-define(HANDLER_KEY,  handlers).
-define(SERVICE_KEY,  services).
-define(APP_KEY,      apps).

%% Only returns capacity and outstanding.
-spec(peer_info() -> peer_info()).
peer_info() -> maps:fold(fun peer_record_map/3, #{}, ergw_aaa_diameter_srv:get_peers_info()).

%% Only knows how to adjust down outstanding.
-spec(peer_info_adjust(peer_info()) -> ok).
peer_info_adjust(PeerInfo) ->
	Peers = ergw_aaa_diameter_srv:get_peers_info(),
	Existing = maps:with(maps:keys(Peers), PeerInfo),
	peer_info_adjust_warning(PeerInfo, Existing, maps:size(PeerInfo), maps:size(Existing)),
	Outstanding = maps:filter(fun peer_info_adjust_outstanding_filter/2, Existing),
	peer_info_adjust_outstanding(Peers, Outstanding, maps:size(Outstanding)),
	Capacity = maps:filter(fun peer_info_adjust_capacity_filter/2, Existing),
	peer_info_adjust_capacity(Capacity, maps:size(Capacity)).

setopts(Opts0) when is_map(Opts0)->
    Opts = maps:map(fun ergw_aaa_config:validate_option/2, Opts0),
    maps:map(application:set_env(ergw_aaa, _, _), Opts),
    ok;
setopts(Opts) when is_list(Opts) ->
    setopts(ergw_aaa_config:to_map(Opts)).

setopt(rate_limit = Opt, {Key, Value0}) ->
    Value = ergw_aaa_config:validate_rate_limit(Key, Value0),
    Limits = application:get_env(ergw_aaa, rate_limits, #{}),
    application:set_env(ergw_aaa, rate_limits, maps:put(Key, Value, Limits));
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


peer_info_adjust_capacity(Capacity, N) when N > 0 ->
	F = fun(State) -> peer_info_adjust_capacity_replace_state(Capacity, State) end,
	sys:replace_state(ergw_aaa_diameter_srv, F);
peer_info_adjust_capacity(_Capacity, _N) ->
	ok.

peer_info_adjust_capacity_filter(_Key, #{capacity := _}) -> true;
peer_info_adjust_capacity_filter(_Key, _Value) -> false.

peer_info_adjust_capacity_replace_state(Capacity, State) ->
	Peers = ergw_aaa_diameter_srv:peers_from_state(State),
	F =  fun(Host, Peer, Acc) ->
		peer_info_adjust_capacity_replace_state(Host, Peer, maps:find(Host, Acc), Acc)
	end,
	ergw_aaa_diameter_srv:peers_into_state(State, maps:fold(F, Peers, Capacity)).

peer_info_adjust_capacity_replace_state(Host, #{capacity := C}, {ok, Peer}, Acc) ->
	Acc#{Host => Peer#peer{capacity = C}};
%% Should not happen, but do not crash sys:replace_state
peer_info_adjust_capacity_replace_state(Host, Peer, error, Acc) ->
	?LOG(error, "~p:peer_info_adjust capacity not adjusted, ~p ~p", [?MODULE, Host, Peer]),
	Acc.


peer_info_adjust_outstanding(Peers, Outstanding, N) when N > 0 ->
	F = fun(Host, Peer, _Acc) -> peer_info_adjust_outstanding_fold(Host, Peer, maps:find(Host, Peers)) end,
	maps:fold(F, ignore, Outstanding);
peer_info_adjust_outstanding(_Peers, _Outstanding, _N) ->
	ok.

peer_info_adjust_outstanding_filter(_Key, #{outstanding := _}) -> true;
peer_info_adjust_outstanding_filter(_Key, _Value) -> false.

peer_info_adjust_outstanding_fold(Host, #{outstanding := Expected}, {ok, #peer{outstanding = Current}})
		when Expected < Current
		->
	Peer = {ignore, #diameter_caps{origin_host = {ignore, Host}}},
	[ergw_aaa_diameter_srv:finish_request(undefined, Peer) || _ <- lists:seq(1, Current - Expected)],
	ok;
peer_info_adjust_outstanding_fold(Host, Peer, {ok, _}) ->
	?LOG(warning, "~p:peer_info_adjust outstanding not adjusted, ~p", [?MODULE, Host, Peer]).


peer_info_adjust_warning(_Expected, _Remaining, Size, Size) ->
	ok;
peer_info_adjust_warning(Expected, Remaining, _, _)  ->
	?LOG(warning, "~p:peer_info_adjust unknown peers ignored, ~p", [?MODULE, maps:keys(Expected) -- maps:keys(Remaining)]).


peer_record_map(Key, #peer{capacity = C, outstanding = O}, Acc) -> Acc#{Key => #{capacity => C, outstanding => O}};
peer_record_map(_Key, _Value, Acc) -> Acc.
