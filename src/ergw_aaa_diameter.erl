%% Copyright 2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_diameter).

-compile({parse_transform, cut}).

%% API
-export([validate_function/1,
	 initialize_function/2]).
-export(['3gpp_from_session'/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/diameter_3gpp_ts29_061_sgi.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(DefaultFunctionOptions, [{transports, undefined},
				 {'Origin-Host', undefined},
				 {'Origin-Realm', undefined}
				 ]).
-define(DefaultTransportOptions, [{connect_to, undefined}
				 ]).

-define(IS_IPv4(X), (is_tuple(X) andalso tuple_size(X) == 4)).
-define(IS_IPv6(X), (is_tuple(X) andalso tuple_size(X) == 8)).
-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).
-define(non_empty_opts(X), ((is_list(X) andalso length(X) /= 0) orelse
			    (is_map(X) andalso map_size(X) /= 0))).

%%===================================================================
%% API
%%===================================================================

initialize_function(Id, #{'Origin-Host' := {OriginHost, Addr},
			  'Origin-Realm' := OriginRealm,
			  transports := Transports}) ->
    ProductName = application:get_env(ergw_aaa, product_name, "erGW-AAA"),

    SvcOpts0 = #{'Origin-Host' => OriginHost,
		 'Origin-Realm' => OriginRealm,
		 'Origin-State-Id' => diameter:origin_state_id(),
		 'Host-IP-Address' => [Addr],
		 'Vendor-Id' => ?VENDOR_ID_TP,
		 'Product-Name' => ProductName,
		 'Supported-Vendor-Id' => [?VENDOR_ID_3GPP,
					   ?VENDOR_ID_ETSI,
					   ?VENDOR_ID_TP],
		 string_decode => false,
		 decode_format => map},
    SvcOpts = merge_svc(SvcOpts0, ergw_aaa_diameter_srv:get_service_opts(Id)),
    ok = diameter:start_service(Id, svc_to_opts(SvcOpts)),
    [ok = initialize_transport(Id, X) || X <- Transports],
    {ok, []}.

initialize_transport(Id, #{connect_to :=
			       #diameter_uri{type = _AAA, % aaa | aaas
					     fqdn = Host,
					     port = Port,
					     transport = Transport,
					     protocol = _Diameter}} = Opts) ->
    Caps = maps:fold(fun build_transport_caps/3, [], Opts),
    {ok, {Raddr, Type}} = resolve_hostname(Host),
    TransportOpts = [{capabilities, Caps},
		     {transport_module, transport_module(Transport)},
		     {transport_config, [{reuseaddr, true},
					 {raddr, Raddr},
					 {rport, Port},
					 Type
					]}],
    {ok, _} = diameter:add_transport(Id, {connect, TransportOpts}),
    ok.

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_capability('Origin-Host', {Host, Addr} = Value)
  when is_binary(Host), ?IS_IP(Addr) ->
    Value;
validate_capability('Origin-Host' = Opt, Value) when is_binary(Value) ->
    try
	{ok, {Addr, _Type}} = resolve_hostname(Value),
	{Value, Addr}
    catch _:_ -> validate_capability_error(Opt, Value)
    end;
validate_capability('Origin-Realm', Value) when is_binary(Value) ->
    Value;
validate_capability(Opt, Value) ->
    validate_capability_error(Opt, Value).

validate_capability_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_function(Opts) ->
    ergw_aaa_config:validate_options(fun validate_function/2, Opts,
				     ?DefaultFunctionOptions, map).

validate_function(transports, Opts) when ?non_empty_opts(Opts) ->
    lists:map(
      ergw_aaa_config:validate_options(fun validate_transport/2, _,
				       ?DefaultTransportOptions, map), Opts);
validate_function(K, V)
  when K =:= 'Origin-Host'; K =:= 'Origin-Realm' ->
    validate_capability(K, V);
validate_function(Opt, Value) ->
    validate_function_error(Opt, Value).

validate_function_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_transport(connect_to, Value) when is_record(Value, diameter_uri) ->
    Value;
validate_transport(connect_to = Opt, Value) when is_binary(Value) ->
    try
	#diameter_uri{} =
	    diameter_types:'DiameterURI'(decode, Value, #{rfc => 6733})
    catch _:_ -> validate_transport_error(Opt, Value)
    end;
validate_transport(K, V)
  when K =:= 'Origin-Host'; K =:= 'Origin-Realm' ->
    validate_capability(K, V);
validate_transport(Opt, Value) ->
    validate_transport_error(Opt, Value).

validate_transport_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

resolve_hostname(Name) when is_binary(Name) -> resolve_hostname(binary_to_list(Name));
resolve_hostname(Name) ->
    Name1 = case inet:gethostbyname(Name, inet6) of
	{error, nxdomain} -> inet:gethostbyname(Name, inet);
	Other -> Other
    end,
    case Name1 of
	{ok, #hostent{h_addr_list = [LocalIP | _], h_addrtype = Type}} ->
	    {ok, {LocalIP, Type}};
	_ -> erlang:error(badarg, Name)
    end.

transport_module(tcp) -> diameter_tcp;
transport_module(sctp) -> diameter_sctp;
transport_module(_) -> unknown.

svc_set(Key, Value, Opts)
  when is_atom(Key), is_list(Value) ->
    Set = sets:from_list(Value),
    maps:update_with(Key, fun(V) -> sets:union(Set, V) end, Set, Opts);
svc_set(Key, Value, Opts)
  when is_atom(Key) ->
    maps:update_with(Key, fun(V) -> sets:add_element(Value, V) end,
		     sets:from_list([Value]), Opts).

merge_svc(Opts, Services) ->
    lists:foldl(fun(Service, OptsIn) -> maps:fold(fun merge_svc/3, OptsIn, Service) end,
		Opts, Services).

merge_svc(K, V, Opts)
  when K =:= 'Auth-Application-Id';
       K =:= 'Acct-Application-Id';
       K =:= 'Vendor-Specific-Application-Id' ->
    svc_set(K, V, Opts);
merge_svc(K, V, Opts)
  when K =:= application, is_list(V) ->
    svc_set(K, [V], Opts).

svc_to_opts(Opts) ->
    maps:fold(fun svc_to_opts/3, [], Opts).

svc_to_opts(K, V, Opts)
  when K =:= 'Auth-Application-Id';
       K =:= 'Acct-Application-Id';
       K =:= 'Vendor-Specific-Application-Id' ->
    [{K, sets:to_list(V)} | Opts];
svc_to_opts(K, V, Opts)
  when K =:= application ->
    Opts ++ [{K, X} || X <- sets:to_list(V)];
svc_to_opts(K, V, Opts) ->
    [{K, V}|Opts].

build_transport_caps('Origin-Host', {Host, Addr}, Caps) ->
    [{'Origin-Host', Host}, {'Host-IP-Address', [Addr]} | Caps];
build_transport_caps('Origin-Realm', Realm, Caps) ->
    [{'Origin-Realm', Realm} | Caps];
build_transport_caps(_, _, Caps) ->
    Caps.

%%===================================================================
%% 3GPP IE
%%===================================================================

format_address({A, B, C, D}) -> <<A, B, C, D>>;
format_address({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
format_address(Addr) -> Addr.

pdp_type('IPv4')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4';
pdp_type('IPv6')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV6';
pdp_type('IPv4v6')                  -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4V6';
pdp_type('PPP')                     -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP';
pdp_type('Non-IP')                  -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_NON-IP';
pdp_type(_)                         -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP'.

'3gpp_from_session'(Key, Value)
  when (Key =:= '3GPP-Charging-Gateway-Address' orelse
	Key =:= '3GPP-SGSN-Address' orelse
	Key =:= '3GPP-GGSN-Address') andalso
       ?IS_IPv4(Value) ->
    format_address(Value);

'3gpp_from_session'(Key, Value)
  when (Key =:= '3GPP-Charging-Gateway-IPv6-Address' orelse
	Key =:= '3GPP-SGSN-IPv6-Address' orelse
	Key =:= '3GPP-GGSN-IPv6-Address') andalso
       ?IS_IPv6(Value) ->
    format_address(Value);

'3gpp_from_session'('3GPP-IPv6-DNS-Servers', Value)
  when is_list(Value) ->
    << <<(format_address(IP))/binary>> || IP <- Value >>;
'3gpp_from_session'('3GPP-IPv6-DNS-Servers', Value)
  when is_binary(Value) ->
    Value;

'3gpp_from_session'('3GPP-Teardown-Indicator', true) ->
    <<1>>;
'3gpp_from_session'('3GPP-Teardown-Indicator', <<_:7, 1:1>>) ->
    <<1>>;
'3gpp_from_session'('3GPP-Teardown-Indicator', Value)
  when is_integer(Value) ->
    << (Value rem 2) >>;
'3gpp_from_session'('3GPP-Teardown-Indicator', _Value) ->
    <<0>>;

'3gpp_from_session'('3GPP-Session-Stop-Indicator', true) ->
    <<255>>;
'3gpp_from_session'('3GPP-Session-Stop-Indicator', Value)
  when is_integer(Value), Value /= 0 ->
    <<255>>;
'3gpp_from_session'('3GPP-Session-Stop-Indicator', Value)
  when is_binary(Value) ->
    Value;
'3gpp_from_session'('3GPP-Session-Stop-Indicator', _Value) ->
    <<0>>;

'3gpp_from_session'(Key, Value)
  when Key =:= '3GPP-RAT-Type'
       andalso is_integer(Value) ->
    <<Value>>;

'3gpp_from_session'(Key, Value)
  when Key =:= '3GPP-Charging-Id'
       andalso is_integer(Value) ->
    <<Value:32>>;

'3gpp_from_session'(Key, Value)
  when Key =:= '3GPP-Camel-Charging' orelse
       Key =:= '3GPP-IMSI' orelse
       Key =:= '3GPP-GPRS-Negotiated-QoS-Profile' orelse
       Key =:= '3GPP-IMSI-MCC-MNC' orelse
       Key =:= '3GPP-GGSN-MCC-MNC' orelse
       Key =:= '3GPP-SGSN-MCC-MNC' orelse
       Key =:= '3GPP-IMEISV' orelse
       Key =:= '3GPP-User-Location-Info' orelse
       Key =:= '3GPP-Packet-Filter' orelse
       Key =:= '3GPP-Negotiated-DSCP' ->
    Value;

'3gpp_from_session'('3GPP-PDP-Type', Value) ->
    pdp_type(Value);

'3gpp_from_session'('3GPP-MS-TimeZone', {A, B}) ->
    <<A, B>>;

'3gpp_from_session'('3GPP-Charging-Characteristics', Value)
  when is_binary(Value) ->
    erlang:iolist_to_binary([io_lib:format("~2.16.0B", [X]) || <<X>> <= Value]);

'3gpp_from_session'(Key, Value)
  when (Key =:= '3GPP-NSAPI' orelse
	Key =:= '3GPP-Selection-Mode') andalso
       is_integer(Value) ->
    erlang:integer_to_binary(Value, 16).

