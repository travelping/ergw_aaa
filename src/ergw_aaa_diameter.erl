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
-export(['3gpp_from_session'/2, qos_from_session/1]).
-export([encode_ipv6prefix/1, decode_ipv6prefix/1]).
-export([validate_termination_cause_mapping/1]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/diameter_3gpp_ts29_061_sgi.hrl").

%% RFC:
%% * https://tools.ietf.org/html/rfc3588#section-8.15
%% * https://www.iana.org/assignments/aaa-parameters/aaa-parameters.xhtml#aaa-parameters-16
-define(DEFAULT_TERMINATION_CAUSE_MAPPING, [
    {normal, 1},                 % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {administrative, 4},         % ?'DIAMETER_BASE_TERMINATION-CAUSE_ADMINISTRATIVE'
    {link_broken, 5},            % ?'DIAMETER_BASE_TERMINATION-CAUSE_LINK_BROKEN'
    {upf_failure, 5},            % ?'DIAMETER_BASE_TERMINATION-CAUSE_LINK_BROKEN'
    {remote_failure, 1},         % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {cp_inactivity_timeout, 1},  % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {up_inactivity_timeout, 1},  % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {peer_restart, 1},           % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {'ASR', 1},                  % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {error, 1},                  % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {req_timeout, 1},            % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {conn_error, 1},             % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {rate_limit, 1},             % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {ocs_hold_end, 1},           % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {peer_reject, 1},            % ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'
    {deleted_by_upf, 19}         % 'NAS_ERROR'
]).

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(DefaultFunctionOptions, [{transports, undefined},
				 {'Origin-Host', undefined},
				 {'Origin-Realm', undefined}
				 ]).
-define(DefaultTransportOptions, [{connect_to, undefined}, 
				  {unordered, true},
				  {reuseaddr, true},
				  {nodelay, true}
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

    %% adding the DIAMETER RFC base dictionary controls some options in
    %% the Erlang diameter app
    Base6733 = [{alias, base6733},
		{dictionary, diameter_gen_base_rfc6733},
		{module, [?MODULE, base6733]}],
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
		 decode_format => map,
		 application => sets:from_list([Base6733])
		},
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
		     {transport_config, transport_config(Transport, Type, Raddr, Port, Opts)}],
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
    erlang:error(badarg, [Opt, Value]).

validate_function(Opts) ->
    ergw_aaa_config:validate_options(fun validate_function/2, Opts, ?DefaultFunctionOptions).

validate_function(transports, Opts) when ?non_empty_opts(Opts) ->
    lists:map(ergw_aaa_config:validate_options(
		fun validate_transport/2, _, ?DefaultTransportOptions), Opts);
validate_function(K, V)
  when K =:= 'Origin-Host'; K =:= 'Origin-Realm' ->
    validate_capability(K, V);
validate_function(handler, Value) ->
    Value;
validate_function(Opt, Value) ->
    validate_function_error(Opt, Value).

validate_function_error(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

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
validate_transport(fragment_timer, Value) when Value =:= infinity ->
    Value;
validate_transport(fragment_timer, Value)
  when is_integer(Value), Value >= 0, Value =< 16#FFFFFFFF ->
    Value;
validate_transport(recbuf, Value) when is_integer(Value), Value >= 16*1024 ->
    Value;
validate_transport(sndbuf, Value) when is_integer(Value), Value >= 16*1024 ->
    Value;
validate_transport(reuseaddr, Value) when is_boolean(Value) ->
    Value;
validate_transport(unordered, Value) when is_boolean(Value) ->
    Value;
validate_transport(nodelay, Value) when is_boolean(Value) ->
    Value;
validate_transport(Opt, Value) ->
    validate_transport_error(Opt, Value).

validate_transport_error(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

validate_termination_cause_mapping(Opts) when is_list(Opts); is_map(Opts) ->
    ergw_aaa_config:validate_options(fun validate_termination_cause_mapping/2, Opts, ?DEFAULT_TERMINATION_CAUSE_MAPPING);
validate_termination_cause_mapping(Opts) ->
    erlang:error(badarg, [termination_cause_mapping, Opts]).

validate_termination_cause_mapping(Opt, Value) when is_atom(Opt), is_integer(Value) ->
    Value;
validate_termination_cause_mapping(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

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

transport_config(tcp, Type, Raddr, Port, Opts) ->
    [Type, {raddr, Raddr}, {rport, Port}
     | maps:to_list(maps:with([fragment_timer, reuseaddr, recbuf, sndbuf, nodelay], Opts))];
transport_config(sctp, Type, Raddr, Port, Opts0) ->
    Opts =
	[Type, {raddr, Raddr}, {rport, Port}
	| maps:to_list(maps:with([reuseaddr, recbuf, sndbuf, nodelay, unordered], Opts0))],
    [case I of
        {nodelay, V} -> {sctp_nodelay, V};
        _ -> I
    end || I <- Opts].

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
merge_svc(K, [V1|_] = V, Opts)
  when K =:= application, is_list(V1) ->
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

'3gpp_from_session'(Key, Value)
  when (Key =:= '3GPP-Charging-Gateway-Address' orelse
	Key =:= '3GPP-SGSN-Address' orelse
	Key =:= '3GPP-GGSN-Address') andalso
       ?IS_IPv4(Value) ->
    ergw_aaa_3gpp_dict:ip2bin(Value);

'3gpp_from_session'(Key, Value)
  when (Key =:= '3GPP-Charging-Gateway-IPv6-Address' orelse
	Key =:= '3GPP-SGSN-IPv6-Address' orelse
	Key =:= '3GPP-GGSN-IPv6-Address') andalso
       ?IS_IPv6(Value) ->
    ergw_aaa_3gpp_dict:ip2bin(Value);

'3gpp_from_session'(Key, Value)
  when Key =:= '3GPP-PDP-Type';
       Key =:= '3GPP-GPRS-Negotiated-QoS-Profile';
       Key =:= '3GPP-NSAPI';
       Key =:= '3GPP-Session-Stop-Indicator';
       Key =:= '3GPP-Selection-Mode';
       Key =:= '3GPP-Charging-Characteristics';
       Key =:= '3GPP-IPv6-DNS-Servers';
       Key =:= '3GPP-Teardown-Indicator';
       Key =:= '3GPP-RAT-Type';
       Key =:= '3GPP-MS-TimeZone';
       Key =:= '3GPP-Allocate-IP-Type';
       Key =:= '3GPP-Secondary-RAT-Usage' ->
    ergw_aaa_3gpp_dict:encode(Key, Value);

'3gpp_from_session'(Key, Value)
  when Key =:= '3GPP-Charging-Id';
       Key =:= '3GPP-Camel-Charging';
       Key =:= '3GPP-IMSI';
       Key =:= '3GPP-IMSI-MCC-MNC';
       Key =:= '3GPP-GGSN-MCC-MNC';
       Key =:= '3GPP-SGSN-MCC-MNC';
       Key =:= '3GPP-IMEISV';
       Key =:= '3GPP-User-Location-Info';
       Key =:= '3GPP-Packet-Filter';
       Key =:= '3GPP-Negotiated-DSCP' ->
    Value.

arp_from_session('Priority-Level' = Key, PL, ARP) ->
    ARP#{Key => PL};
arp_from_session(Key, Value, ARP)
  when Key == 'Pre-emption-Capability';
       Key == 'Pre-emption-Vulnerability' ->
    ARP#{Key => [Value]};
arp_from_session(_K, _V, ARP) ->
    ARP.

-define(UINT32MAX, 16#ffffffff).

%% 3GPP TS 29.214 version 15.4.0, Section 4.4.10:
%%
%%   When the Rx session is being established, if the AF supports the corresponding
%%   feature [...] and needs to indicate bandwidth values higher than 2^32-1 bps,
%%   AVPs representing bitrate in bps shall be provided with value set to 2^32-1 bps
%%   and bandwidth AVPs representing bitrate in kbps shall be provided with the actual
%%   required bandwidth.

qos_from_session('Allocation-Retention-Priority' = Key, ARP, Info) ->
    Info#{Key => [maps:fold(fun arp_from_session/3, #{}, ARP)]};

qos_from_session('Max-Requested-Bandwidth-UL' = Key, MBR, Info)
  when MBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-Max-Requested-BW-UL' => [MBR div 1000]};
qos_from_session('Max-Requested-Bandwidth-DL' = Key, MBR, Info)
  when MBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-Max-Requested-BW-DL' => [MBR div 1000]};
qos_from_session('Guaranteed-Bitrate-UL' = Key, GBR, Info)
  when GBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-GBR-UL' => [GBR div 1000]};
qos_from_session('Guaranteed-Bitrate-DL' = Key, GBR, Info)
  when GBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-GBR-DL' => [GBR div 1000]};
qos_from_session('APN-Aggregate-Max-Bitrate-UL' = Key, AMBR, Info)
  when AMBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-APN-AMBR-UL' => [AMBR div 1000]};
qos_from_session('APN-Aggregate-Max-Bitrate-DL' = Key, AMBR, Info)
  when AMBR > ?UINT32MAX ->
    Info#{Key => ?UINT32MAX, 'Extended-APN-AMBR-DL' => [AMBR div 1000]};

qos_from_session(Key, Value, Info)
  when Key == 'QoS-Class-Identifier';
       Key == 'Max-Requested-Bandwidth-UL';
       Key == 'Max-Requested-Bandwidth-DL';
       Key == 'Guaranteed-Bitrate-UL';
       Key == 'Guaranteed-Bitrate-DL';
       Key == 'APN-Aggregate-Max-Bitrate-UL';
       Key == 'APN-Aggregate-Max-Bitrate-DL' ->
    Info#{Key => [Value]};

%% TBD:
%%   [ Bearer-Identifier ]
%%  *[ Conditional-APN-Aggregate-Max-Bitrate ]

qos_from_session(_K, _V, Info) ->
    Info.

qos_from_session(Info) ->
    maps:fold(fun qos_from_session/3, #{}, Info).

encode_ipv6prefix({{A,B,C,D,E,F,G,H}, PLen}) ->
    L = (PLen + 7) div 8,
    <<IP:L/bytes, _R/binary>> = <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>,
    <<0, PLen:8, IP/binary>>.

decode_ipv6prefix(<<_:8, PLen:8, Bin/binary>>) ->
    BinLen = (PLen + 7) div 8,
    <<P:BinLen/bytes, Rest/binary>> = Bin,
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>> = eradius_lib:pad_to(16, P),
    {{{A,B,C,D,E,F,G,H}, PLen}, Rest}.
