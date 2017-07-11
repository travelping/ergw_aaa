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

%%===================================================================
%% API
%%===================================================================
initialize_provider(Opts) ->
    {OriginHost, Addr} = proplists:get_value(host, Opts),
    OriginRealm = proplists:get_value(realm, Opts),
    SvcOpts = [{'Origin-Host', OriginHost},
               {'Origin-Realm', OriginRealm},
               {'Origin-State-Id', diameter:origin_state_id()},
               {'Host-IP-Address', [Addr]},
               {'Vendor-Id', ?VENDOR_ID_TP},
               {'Product-Name', "erGW-AAA"},
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

start_accounting(From, 'Start', Session, State) ->
    Request0 = create_ACR(Session, ?Start, State),
    Request = Request0#diameter_sgi_ACR{
                'Accounting-Input-Octets' = [],
                'Accounting-Input-Packets' = [],
                'Accounting-Output-Octets' = [],
                'Accounting-Output-Packets' = []
              },
    cast(Request, From),
    {ok, inc_number(State)};

start_accounting(From, 'Interim', Session, State) ->
    Request0 = create_ACR(Session, ?Interim, State),
    Now = ergw_aaa_variable:now_ms(),
    Start = ergw_aaa_session:attr_get('Accounting-Start', Session, Now),
    Request = Request0#diameter_sgi_ACR{
                'Acct-Session-Time' = [round((Now - Start) / 1000)]
              },
    cast(Request, From),
    {ok, inc_number(State)};

start_accounting(From, 'Stop', Session, State) ->
    Request0 = create_ACR(Session, ?Stop, State),
    Now = ergw_aaa_variable:now_ms(),
    Start = ergw_aaa_session:attr_get('Accounting-Start', Session, Now),
    Request = Request0#diameter_sgi_ACR{
                'Acct-Session-Time' = [round((Now - Start) / 1000)]
              },
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

pick_peer([Peer | _], _, _SvcName, _State) ->
    lager:debug("pick_peer: ~p~n", [Peer]),
    {ok, Peer}.

pick_peer(LocalCandidates, RemoteCandidates, SvcName, State, _From) ->
    pick_peer(LocalCandidates, RemoteCandidates, SvcName, State).

prepare_request(#diameter_packet{msg = #diameter_sgi_ACR{} = Rec}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR},
                   acct_application_id = {[Ids], _}}
        = Caps,

    {send, Rec#diameter_sgi_ACR{'Origin-Host' = OH,
                                'Origin-Realm' = OR,
                                'Destination-Host' = [DH],
                                'Destination-Realm' = DR,
                                'Acct-Application-Id' = Ids}};

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

attr_get(Key, Session) ->
    case ergw_aaa_session:attr_get(Key, Session, undefined) of
       undefined -> [];
       Value -> [Value]
    end.

attr_get_addr(Key, Session) ->
    format_address(attr_get(Key, Session)).

format_address([{A, B, C, D}]) -> [<<A, B, C, D>>];
format_address([{A, B, C, D, E, F, G, H}]) ->
    [<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>];
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

service_type([Atom]) when is_atom(Atom) -> service_type(Atom);
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

acct_authentic([Atom]) when is_atom(Atom) -> acct_authentic(Atom);
%acct_authentic('None')                    -> ?'DIAMETER_SGI_ACCT-AUTHENTIC-NONE';
acct_authentic('RADIUS')                  -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_RADIUS';
acct_authentic('Local')                   -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_LOCAL';
acct_authentic('Remote')                  -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_REMOTE';
acct_authentic('Diameter')                -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_DIAMETER';
acct_authentic(_)                         -> ?'DIAMETER_SGI_ACCT-AUTHENTIC_RADIUS'.

framed_protocol([Atom]) when is_atom(Atom) -> framed_protocol(Atom);
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

pdp_type([Atom]) when is_atom(Atom) -> pdp_type(Atom);
pdp_type('IPv4')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4';
pdp_type('IPv6')                    -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV6';
pdp_type('IPv4v6')                  -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_IPV4V6';
pdp_type('PPP')                     -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP';
pdp_type(_)                         -> ?'DIAMETER_SGI_3GPP-PDP-TYPE_PPP'.

format_cc(Value) when is_binary(Value) ->
    erlang:iolist_to_binary([io_lib:format("~2.16.0B", [X]) 
                             || <<X>> <= Value]);
format_cc(_) -> [].

create_ACR(Session, Type, State) ->
    AcctSessionId = hd(attr_get('Session-Id', Session)),
    MultiSessionId = hd(attr_get('Multi-Session-Id', Session)),
    #diameter_sgi_ACR{
       'Session-Id' = State#state.sid,
       'Accounting-Record-Type' = Type,
       'Accounting-Record-Number' = State#state.accounting_record_number,
       'User-Name' = attr_get('Username', Session),
       'Acct-Session-Id' = io_lib:format("~40.16.0B", [AcctSessionId]),
       'Acct-Multi-Session-Id' = io_lib:format("~40.16.0B", [MultiSessionId]),
       'NAS-Identifier' = [State#state.nas_id],
       'NAS-Port-Id' = attr_get('Port-Id', Session),
       'NAS-Port-Type' = attr_get('Port-Type', Session),
       'Class' = attr_get('Class', Session),
       'Service-Type' = [service_type(attr_get('Service-Type', Session))],
       'Accounting-Input-Octets' = attr_get('InOctets', Session),
       'Accounting-Input-Packets' = attr_get('InPackets', Session),
       'Accounting-Output-Octets' = attr_get('OutOctets', Session),
       'Accounting-Output-Packets' = attr_get('OutPackets', Session),
       'Acct-Authentic' = [acct_authentic(attr_get('Acct-Authentic', Session))],
       'Called-Station-Id' = attr_get('Called-Station-Id', Session),
       'Calling-Station-Id' = attr_get('Calling-Station-Id', Session),
       'Framed-Interface-Id' = attr_get('Framed-Interface-Id', Session),
       'Framed-IP-Address' = attr_get_addr('Framed-IP-Address', Session),
       'Framed-Protocol' = [framed_protocol(attr_get('Framed-Protocol', Session))],
       'Tunneling' = tunneling(Session, State),
       '3GPP-IMSI' = attr_get('3GPP-IMSI', Session),
       '3GPP-Charging-Id' = attr_get('3GPP-Charging-ID', Session),
       '3GPP-PDP-Type' = [pdp_type(attr_get('3GPP-Charging-ID', Session))],
       '3GPP-CG-Address' = attr_get_addr('3GPP-Charging-Gateway-Address', Session),
       '3GPP-GPRS-Negotiated-QoS-Profile' = attr_get('3GPP-GPRS-Negotiated-QoS-Profile', Session),
       '3GPP-SGSN-Address' = attr_get_addr('3GPP-SGSN-Address', Session),
       '3GPP-GGSN-Address' = attr_get_addr('3GPP-GGSN-Address', Session),
       '3GPP-IMSI-MCC-MNC' = attr_get('3GPP-IMSI-MCC-MNC', Session),
       '3GPP-GGSN-MCC-MNC' = attr_get('3GPP-GGSN-MCC-MNC', Session),
       '3GPP-NSAPI' = attr_get('3GPP-NSAPI', Session),
       '3GPP-Selection-Mode' = attr_get('3GPP-Selection-Mode', Session),
       '3GPP-Charging-Characteristics' = format_cc(attr_get('3GPP-Charging-Characteristics', Session)),
       '3GPP-CG-IPv6-Address' = attr_get_addr('3GPP-Charging-Gateway-IPv6-Address', Session),
       '3GPP-SGSN-IPv6-Address' = attr_get_addr('3GPP-SGSN-IPv6-Address', Session),
       '3GPP-GGSN-IPv6-Address' = attr_get_addr('3GPP-GGSN-IPv6-Address', Session),
       '3GPP-SGSN-MCC-MNC' = attr_get('3GPP-SGSN-MCC-MNC', Session),
       '3GPP-IMEISV' = attr_get('3GPP-IMEISV', Session),
       '3GPP-RAT-Type' = attr_get('3GPP-RAT-Type', Session),
       '3GPP-User-Location-Info' = attr_get('3GPP-User-Location-Info', Session),
       '3GPP-MS-TimeZone' = attr_get('3GPP-MS-TimeZone', Session),
       '3GPP-CAMEL-Charging-Info' = attr_get('3GPP-Camel-Charging', Session),
       '3GPP-Packet-Filter' = attr_get('3GPP-Packet-Filter', Session),
       '3GPP-Negotiated-DSCP' = attr_get('3GPP-Negotiated-DSCP', Session),
       'AVP' = []}.

% TODO: build #'Tunneling'{}
tunneling(_Session, _State) -> [].
