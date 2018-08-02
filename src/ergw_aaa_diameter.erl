%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_diameter).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_options/1, initialize_provider/1,
	 init/1, authorize/3, start_authentication/3, start_accounting/4]).
-export(['3gpp_from_session'/2]).
%%
%% diameter callbacks
-export([peer_up/3,
	 peer_down/3,
	 pick_peer/4, pick_peer/5,
	 prepare_request/3, prepare_request/4,
	 prepare_retransmit/3, prepare_retransmit/4,
	 handle_answer/4, handle_answer/5,
	 handle_error/4,
	 handle_request/3]).

-export([stop/0, call/1, cast/1, cast/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/ergw_aaa_profile.hrl").
-include("include/ergw_aaa_variable.hrl").
-include("include/diameter_3gpp_ts29_061_sgi.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(Start, 2).
-define(Interim, 3).
-define(Stop, 4).

-record(state, {accounting_record_number = 0,
		sid    :: binary(), % diameter Session-Id
		nas_id :: binary()}).

-define(DefaultOptions, [{nas_identifier, undefined},
			 {host, undefined},
			 {realm, undefined},
			 {connect_to, undefined}
			]).

-define(IS_IPv4(X), (is_tuple(X) andalso tuple_size(X) == 4)).
-define(IS_IPv6(X), (is_tuple(X) andalso tuple_size(X) == 8)).

%%===================================================================
%% API
%%===================================================================
initialize_provider(Opts) ->
    {OriginHost, Addr} = proplists:get_value(host, Opts),
    OriginRealm = proplists:get_value(realm, Opts),
    ProductName = setup:get_env(ergw_aaa, product_name, "erGW-AAA"),
    SvcOpts = [{'Origin-Host', OriginHost},
	       {'Origin-Realm', OriginRealm},
	       {'Origin-State-Id', diameter:origin_state_id()},
	       {'Host-IP-Address', [Addr]},
	       {'Vendor-Id', ?VENDOR_ID_TP},
	       {'Product-Name', ProductName},
	       {'Supported-Vendor-Id', [?VENDOR_ID_3GPP,
					?VENDOR_ID_ETSI,
					?VENDOR_ID_TP]},
	       {'Auth-Application-Id', [diameter_3gpp_ts29_061_sgi:id()]},
	       {'Acct-Application-Id', [diameter_3gpp_ts29_061_sgi:id()]},
	       {string_decode, false},
	       {application, [{alias, nasreq},
			      {dictionary, diameter_3gpp_ts29_061_sgi},
			      {module, ?MODULE}]}],
    ok = diameter:start_service(?MODULE, SvcOpts),

    #diameter_uri{type = _AAA, % aaa | aaas
		  fqdn = Host,
		  port = Port,
		  transport = Transport,
		  protocol = _Diameter} = proplists:get_value(connect_to, Opts),
    {ok, {Raddr, Type}} = resolve_hostname(Host),
    TransportOpts = [{transport_module, transport_module(Transport)},
		     {transport_config, [{reuseaddr, true},
					 {raddr, Raddr},
					 {rport, Port},
					 Type
					]}],
    {ok, _} = diameter:add_transport(?MODULE, {connect, TransportOpts}),

    {ok, []}.

validate_options(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions).

init(Opts) ->
    State = #state{
      sid = list_to_binary(string:join(diameter:session_id("erGW-AAA"), "")),
      nas_id = proplists:get_value(nas_identifier, Opts)
    },
    {ok, State}.

start_authentication(From, Session, State) ->
    Verdict = success,
    ?queue_event(From, {'AuthenticationRequestReply', {Verdict, Session, State}}),
    {ok, State}.

authorize(_From, _Session, State) ->
    Verdict = success,
    {reply, Verdict, ergw_aaa_session:to_session([]), State}.

start_accounting(From, 'Start', Session0, State) ->
    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets'],
    Session = maps:without(Keys, Session0),
    Request = create_ACR(Session, ?Start, State),
    cast(Request, From),
    {ok, inc_number(State)};

start_accounting(From, 'Interim', Session0, State) ->
    Now = ergw_aaa_variable:now_ms(),
    Start = ergw_aaa_session:attr_get('Accounting-Start', Session0, Now),
    Session = Session0#{'Acct-Session-Time' => round((Now - Start) / 1000)},
    Request = create_ACR(Session, ?Interim, State),
    cast(Request, From),
    {ok, inc_number(State)};

start_accounting(From, 'Stop', Session0, State) ->
    Now = ergw_aaa_variable:now_ms(),
    Start = ergw_aaa_session:attr_get('Accounting-Start', Session0, Now),
    Session = Session0#{'Acct-Session-Time' => round((Now - Start) / 1000)},
    Request = create_ACR(Session, ?Stop, State),
    cast(Request, From),
    {ok, inc_number(State)}.

% will be used in tests
stop() ->
    diameter:stop_service(?MODULE),
    diameter:remove_transport(?MODULE, true).

call(Request) ->
    diameter:call(?MODULE, nasreq, Request, []).

cast(Request) -> cast(Request, self()).

cast(Request, From) ->
    diameter:call(?MODULE, nasreq, Request, [detach, {extra, [From]}]).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State) ->
    lager:debug("peer_up: ~p~n", [_Peer]),
    State.

peer_down(_SvcName, _Peer, State) ->
    lager:debug("peer_down: ~p~n", [_Peer]),
    State.

pick_peer([], RemoteCandidates, _SvcName, _State) ->
    N = rand:uniform(length(RemoteCandidates)),
    {ok, lists:nth(N, RemoteCandidates)};
pick_peer(LocalCandidates, _, _SvcName, _State) ->
    N = rand:uniform(length(LocalCandidates)),
    {ok, lists:nth(N, LocalCandidates)}.

pick_peer(LocalCandidates, RemoteCandidates, SvcName, State, _From) ->
    pick_peer(LocalCandidates, RemoteCandidates, SvcName, State).

prepare_request(#diameter_packet{msg = ['ACR' = T | Avps]}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
		   origin_realm = {OR, DR},
		   acct_application_id = {[Ids], _}}
	= Caps,

    {send, [T, {'Origin-Host', OH},
	       {'Origin-Realm', OR},
	       {'Destination-Host', [DH]},
	       {'Destination-Realm', DR},
	       {'Acct-Application-Id', Ids}
	    | Avps]};

prepare_request(_Packet, _, _Peer) ->
    lager:debug("unexpected request: ~p~n", [_Packet]),
    erlang:error({unexpected, ?MODULE, ?LINE}).

prepare_request(Packet, SvcName, Peer, _From) ->
    prepare_request(Packet, SvcName, Peer).

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

prepare_retransmit(Packet, SvcName, Peer, _From) ->
    prepare_request(Packet, SvcName, Peer).

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer) ->
    {ok, Msg}.

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer, From)
  when is_record(Msg, diameter_sgi_ACA) and is_pid(From) ->
    handle_aca(From, Msg),
    {ok, Msg};

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer, _From) ->
    {ok, Msg}.

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).


%%===================================================================
%% internal helpers
%%===================================================================
handle_aca(From, Answer) ->
    #diameter_sgi_ACA{'Result-Code' = Code,
		      'Acct-Interim-Interval' = Interval} = Answer,
    if Code == ?'DIAMETER_BASE_RESULT-CODE_SUCCESS' andalso length(Interval) > 0 ->
	   ?queue_event(From, {'ChangeInterimAccouting', hd(Interval)});
       true -> ok
    end.

validate_option(nas_identifier, Value) when is_binary(Value) ->
    Value;
validate_option(connect_to = Opt, Value) when is_binary(Value) ->
    try
	#diameter_uri{} = decode_diameter_uri(Value)
    catch _:_ -> validate_option_error(Opt, Value)
    end;
validate_option(host = Opt, Value) when is_binary(Value) ->
    try
	{ok, {Addr, _Type}} = resolve_hostname(Value),
	{Value, Addr}
    catch _:_ -> validate_option_error(Opt, Value)
    end;
validate_option(realm, Value) when is_binary(Value) ->
    Value;
validate_option(acct_interim_interval, Value) when is_integer(Value) ->
    Value;
validate_option(service_type, Value) when is_atom(Value) ->
    Value;
validate_option(framed_protocol, Value) when is_atom(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

decode_diameter_uri(Value) ->
    Module = diameter_types, % trick to stop xref complains about undef function
    try
	apply(Module, 'DiameterURI', [decode, Value, #{rfc => 6733}]) % OTP 20
    catch _:_ -> apply(Module, 'DiameterURI', [decode, Value])
    end.

inc_number(#state{accounting_record_number = Number} = State) ->
    State#state{accounting_record_number = Number + 1}.

format_address({A, B, C, D}) -> <<A, B, C, D>>;
format_address({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
format_address(Addr) -> Addr.

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

service_type('Login-User')              -> ?'DIAMETER_SGI_SERVICE-TYPE_LOGIN';
service_type('Framed-User')             -> ?'DIAMETER_SGI_SERVICE-TYPE_FRAMED';
service_type('Callback-Login-User')     -> ?'DIAMETER_SGI_SERVICE-TYPE_CALLBACK_LOGIN';
service_type('Callback-Framed-User')    -> ?'DIAMETER_SGI_SERVICE-TYPE_CALLBACK_FRAMED';
service_type('Outbound-User')           -> ?'DIAMETER_SGI_SERVICE-TYPE_OUTBOUND';
service_type('Administrative-User')     -> ?'DIAMETER_SGI_SERVICE-TYPE_ADMINISTRATIVE';
service_type('NAS-Prompt-User')         -> ?'DIAMETER_SGI_SERVICE-TYPE_NAS_PROMPT';
service_type('Authenticate-Only')       -> ?'DIAMETER_SGI_SERVICE-TYPE_AUTHENTICATE_ONLY';
service_type('Callback-NAS-Prompt')     -> ?'DIAMETER_SGI_SERVICE-TYPE_CALLBACK_NAS_PROMPT';
service_type('Call-Check')              -> ?'DIAMETER_SGI_SERVICE-TYPE_CALL_CHECK';
service_type('Callback-Administrative') -> ?'DIAMETER_SGI_SERVICE-TYPE_CALLBACK_ADMINISTRATIVE';
service_type('Voice')                   -> ?'DIAMETER_SGI_SERVICE-TYPE_VOICE';
service_type('Fax')                     -> ?'DIAMETER_SGI_SERVICE-TYPE_FAX';
service_type('Modem-Relay')             -> ?'DIAMETER_SGI_SERVICE-TYPE_MODEM_RELAY';
service_type('IAPP-Register')           -> ?'DIAMETER_SGI_SERVICE-TYPE_IAPP-REGISTER';
service_type('IAPP-AP-Check')           -> ?'DIAMETER_SGI_SERVICE-TYPE_IAPP-AP-CHECK';
service_type('Authorze-Only')           -> ?'DIAMETER_SGI_SERVICE-TYPE_AUTHORIZE_ONLY';
%service_type('Framed-Management')       -> ?'DIAMETER_SGI_SERVICE-TYPE_FRAMED-MANAGEMENT';
% these types are not described in dia file
%service_type('TP-CAPWAP-WTP')           -> ?'DIAMETER_SGI_SERVICE-TYPE_UNKNOWN'.
%service_type('TP-CAPWAP-STA')           -> ?'DIAMETER_SGI_SERVICE-TYPE_UNKNOWN'.
service_type(_)                         -> ?'DIAMETER_SGI_SERVICE-TYPE_FRAMED'.

%acct_authentic('None')                    -> ?'DIAMETER_SGI_ACCT-AUTHENTIC-NONE';
acct_authentic('RADIUS')                  -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_RADIUS';
acct_authentic('Local')                   -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_LOCAL';
acct_authentic('Remote')                  -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_REMOTE';
acct_authentic('Diameter')                -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_DIAMETER';
acct_authentic(_)                         -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_RADIUS'.

framed_protocol('PPP')                     -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_PPP';
framed_protocol('SLIP')                    -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_SLIP';
framed_protocol('ARAP')                    -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_ARAP';
framed_protocol('Gandalf-SLML')            -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_GANDALF';
framed_protocol('Xylogics-IPX-SLIP')       -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_XYLOGICS';
framed_protocol('X.75-Synchronous')        -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_X75';
framed_protocol('GPRS-PDP-Context')        -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_GPRS_PDP_CONTEXT';
% note that mapping for 255,256,257,258,259,260,261 missed for now
% these types are not described in dia file
%framed_protocol('TP-CAPWAP')               -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_PPP'.
framed_protocol(_)                         -> ?'DIAMETER_SGI_FRAMED-PROTOCOL_PPP'.

pdp_type('IPv4')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4';
pdp_type('IPv6')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV6';
pdp_type('IPv4v6')                  -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4V6';
pdp_type('PPP')                     -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP';
pdp_type('Non-IP')                  -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_NON-IP';
pdp_type(_)                         -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP'.

to_list({Key, [A | _] = Avps}) when is_map(A) ->
    {Key, lists:map(fun to_list/1, Avps)};
to_list({Key, Avps}) when is_map(Avps) ->
    {Key, lists:map(fun to_list/1, maps:to_list(Avps))};
to_list(Avps) when is_map(Avps) ->
    lists:map(fun to_list/1, maps:to_list(Avps));
to_list(Avp) ->
    Avp.

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

from_session(Key, Value, M)
  when Key =:= '3GPP-Charging-Gateway-Address';
       Key =:= '3GPP-SGSN-Address';
       Key =:= '3GPP-GGSN-Address';
       Key =:= '3GPP-Charging-Gateway-IPv6-Address';
       Key =:= '3GPP-SGSN-IPv6-Address';
       Key =:= '3GPP-GGSN-IPv6-Address';
       Key =:= '3GPP-Charging-Id';
       Key =:= '3GPP-Camel-Charging';
       Key =:= '3GPP-PDP-Type';
       Key =:= '3GPP-RAT-Type';
       Key =:= '3GPP-MS-TimeZone';
       Key =:= '3GPP-Charging-Characteristics';
       Key =:= '3GPP-NSAPI';
       Key =:= '3GPP-Selection-Mode';
       Key =:= '3GPP-IMSI';
       Key =:= '3GPP-GPRS-Negotiated-QoS-Profile';
       Key =:= '3GPP-IMSI-MCC-MNC';
       Key =:= '3GPP-GGSN-MCC-MNC';
       Key =:= '3GPP-SGSN-MCC-MNC';
       Key =:= '3GPP-IMEISV';
       Key =:= '3GPP-User-Location-Info';
       Key =:= '3GPP-Packet-Filter';
       Key =:= '3GPP-Negotiated-DSCP' ->
    M#{Key => ['3gpp_from_session'(Key, Value)]};

from_session('Service-Type' = Key, Value, M) ->
    M#{Key => [service_type(Value)]};
from_session('Acct-Authentic' = Key, Value, M) ->
    M#{Key => [acct_authentic(Value)]};
from_session('Framed-Protocol' = Key, Value, M) ->
    M#{Key => [framed_protocol(Value)]};

from_session('InPackets', Value, M) ->
    M#{'Accounting-Input-Packets' => [Value]};
from_session('OutPackets', Value, M) ->
    M#{'Accounting-Output-Packets' => [Value]};
from_session('InOctets', Value, M) ->
    M#{'Accounting-Input-Octets' => [Value]};
from_session('OutOctets', Value, M) ->
    M#{'Accounting-Output-Octets' => [Value]};

from_session('IP', Value, M) ->
    M#{'Framed-IP-Address' => [format_address(Value)]};
from_session('Framed-IP-Address' = Key, Value, M) ->
    M#{Key => [format_address(Value)]};

from_session(Key, Value, M)
  when Key =:= 'Acct-Session-Time';
       Key =:= 'User-Name';
       Key =:= 'NAS-Port-Id';
       Key =:= 'NAS-Port-Type';
       Key =:= 'Class';
       Key =:= 'Called-Station-Id';
       Key =:= 'Calling-Station-Id';
       Key =:= 'Framed-Interface-Id' ->
    M#{Key => [Value]};

from_session(_Key, _Value, M) -> M.

from_session(Session, Avps) ->
    maps:fold(fun from_session/3, Avps, Session).

create_ACR(Session, Type, State) ->
    Avps0 = #{'Session-Id'               => State#state.sid,
	      'Accounting-Record-Type'   => Type,
	      'Accounting-Record-Number' => State#state.accounting_record_number,
	      'NAS-Identifier'           => [State#state.nas_id]},
    Avps = from_session(Session, Avps0),
    ['ACR' | to_list(Avps)].
