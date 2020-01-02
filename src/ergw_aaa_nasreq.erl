%% Copyright 2017-2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

%% Notes :
%% This module supports the split and the coupled accounting model,
%% as described in https://tools.ietf.org/html/rfc6733#section-9.3
%%
%% The dictionaries are based on 3GPP TS29.061. One particularity
%% is that while RFC 7155 (obsoletes RFC 4005) defines the
%% Acct-Application-Id as mandatory in the ACR message, the 3GPP
%% spec defines it as optional.
%% According to RFC 7155, Sect. 1.1 the Acct-Application-Id was
%% already mandatory (see the RFC for an explanation). In order
%% to be more compatible this application defines the Id as
%% optional and alays fills it in requests and answers

-module(ergw_aaa_nasreq).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/6, handle_response/6]).

%%
%% diameter callbacks
-export([peer_up/3,
	 peer_down/3,
	 pick_peer/5,
	 prepare_request/4,
	 prepare_retransmit/4,
	 handle_answer/5,
	 handle_error/5,
	 handle_request/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/ergw_aaa_session.hrl").
-include("include/diameter_rfc4005_nasreq.hrl").
-include("include/diameter_3gpp_ts29_061_sgi.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, nasreq).
-define(BASE_ACC_APP, nasreq_base_acc).
-define(DIAMETER_DICT_NASREQ, diameter_3gpp_ts29_061_sgi).
-define(DIAMETER_DICT_NASREQ_BASE_ACC, diameter_3gpp_ts29_061_sgi_base_acc).
-define(DIAMETER_APP_ID_BASE_ACC, ?DIAMETER_DICT_NASREQ_BASE_ACC:id()).
-define(DIAMETER_APP_ID_NASREQ, ?DIAMETER_DICT_NASREQ:id()).
-define(DEFAULT_TERMINATION_CAUSE, ?'TERMINATION-CAUSE_PORT_ERROR').

-define(DefaultOptions, [{function, "undefined"},
			 {accounting, coupled},
			 {'Destination-Realm', undefined}]).

-define(IS_IPv4(X), (is_tuple(X) andalso tuple_size(X) == 4)).
-define(IS_IPv6(X), (is_tuple(X) andalso tuple_size(X) == 8)).
-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

-record(state, {pending = #{}      :: #{atom() => reference()},
		state = init       :: atom(),
		authorized = false :: boolean(),
		record_number      :: 'undefined' | integer()
	       }).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{function := Function, accounting := AcctModel}) ->
    {AcctId, Appl} =
	case AcctModel of
	    split   ->
		AcctAppl = [[{alias, ?BASE_ACC_APP},
			     {dictionary, ?DIAMETER_DICT_NASREQ_BASE_ACC},
			     {module, ?MODULE}]],
		{?DIAMETER_APP_ID_BASE_ACC, AcctAppl};
	    coupled ->
		{?DIAMETER_APP_ID_NASREQ, []}
	end,
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_NASREQ,
	  'Acct-Application-Id' => AcctId,
	  application => [[{alias, ?APP},
			   {dictionary, ?DIAMETER_DICT_NASREQ},
			   {module, ?MODULE}]
			  | Appl]},
    ergw_aaa_diameter_srv:register_service(Function, SvcOpts),
    {ok, []}.

validate_handler(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions, map).

validate_service(_Service, HandlerOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, HandlerOpts, map).

validate_procedure(_Application, _Procedure, _Service, ServiceOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ServiceOpts, map).

invoke(_Service, init, Session, Events, _Opts, _State) ->
    {ok, Session, Events, #state{state = stopped}};

invoke(_Service, authenticate, Session, Events, Opts, State0) ->
    %% Combined authentication and authorisation, setting
    State = State0#state{authorized = true},
    RecType = ?'DIAMETER_SGI_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
    Request = create_AAR(RecType, Session, Opts),
    await_response(send_request(?APP, Request, Opts), 'AAR', Session, Events, State, Opts);

invoke(_Service, authorize, Session, Events, _Opts, State) ->
    {ok, Session, Events, State};

invoke(_Service, start, Session0, Events, Opts, #state{state = stopped} = State0) ->
    State = inc_record_number(State0#state{state = started}),
    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
    Session = maps:without(Keys, Session0),
    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_START_RECORD',
    App = acct_app_alias(Opts),
    Request = create_ACR(RecType, Session, Opts, State),
    await_response(send_request(App, Request, Opts), 'ACR', Session, Events, State, Opts);

invoke(_Service, interim, Session, Events, Opts, #state{state = started} = State0) ->
    State = inc_record_number(State0),
    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD',
    App = acct_app_alias(Opts),
    Request = create_ACR(RecType, Session, Opts, State),
    await_response(send_request(App, Request, Opts), 'ACR', Session, Events, State, Opts);

invoke(_Service, stop, Session, Events, Opts,
       #state{state = started, authorized = Authorized} = State0) ->
    ?LOG(debug, "Session Stop: ~p", [Session]),
    State1 = inc_record_number(State0#state{state = stopped}),
    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_STOP_RECORD',
    App = acct_app_alias(Opts),
    ACRRequest = create_ACR(RecType, Session, Opts, State1),
    ACRRes = await_response(send_request(App, ACRRequest, Opts), 'ACR', Session, Events, State1, Opts),
    case Authorized of
	true ->
	    STRRequest = create_STR(Session, Opts),
	    State2 = element(4, ACRRes),
	    STRRes =
		await_response(send_request(?APP, STRRequest, Opts), 'STR', Session, Events, State2, Opts),
	    setelement(4, ACRRes, element(4, STRRes));
	_ ->
	    ACRRes
    end;

invoke(_Service, Procedure, Session, Events, _Opts, State)
  when Procedure =:= start; Procedure =:= interim; Procedure =:= stop ->
    {ok, Session, Events, State};

invoke(Service, Procedure, Session, Events, _Opts, State) ->
    {{error, {Service, Procedure}}, Session, Events, State}.

%%%===================================================================
%%% ergw_aaa_diameter_srv wrapper
%%%===================================================================

send_request(App, Request, Config) ->
    ergw_aaa_diameter_srv:send_request(?MODULE, App, Request, Config).

await_response(Promise, Type, Session, Events, #state{pending = Pending} = State, #{async := true}) ->
    {ok, Session, Events, State#state{pending = Pending#{Promise => Type}}};
await_response(Promise, Type, Session, Events, State, Opts) ->
    Msg = ergw_aaa_diameter_srv:await_response(Promise),
    handle_response(Type, Msg, Session, Events, Opts, State).


%% handle_response/6
handle_response(Promise, Msg, Session, Events, Opts, #state{pending = Pending0} = State)
  when is_reference(Promise) ->
    {Type, Pending} = maps:take(Promise, Pending0),
    handle_response(Type, Msg, Session, Events, Opts, State#state{pending = Pending});

handle_response('ACR', Msg, Session, Events, Opts, State) ->
    handle_aca(Msg, Session, Events, Opts, State);
handle_response('AAR', Msg, Session, Events, Opts, State) ->
    handle_aaa(Msg, Session, Events, Opts, State);
handle_response('STR', Msg, Session, Events, Opts, State) ->
    handle_sta(Msg, Session, Events, Opts, State).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

%% peer_up/3
peer_up(_SvcName, _Peer, State) ->
    ?LOG(debug, "peer_up: ~p~n", [_Peer]),
    State.

%% peer_down/3
peer_down(SvcName, Peer, State) ->
    ergw_aaa_diameter_srv:peer_down(?MODULE, SvcName, Peer),
    ?LOG(debug, "peer_down: ~p~n", [Peer]),
    State.

%% pick_peer/5
pick_peer([], RemoteCandidates, SvcName, _State, CallOpts) ->
    ergw_aaa_diameter_srv:pick_peer(RemoteCandidates, SvcName, CallOpts);
pick_peer(LocalCandidates, _, SvcName, _State, CallOpts) ->
    ergw_aaa_diameter_srv:pick_peer(LocalCandidates, SvcName, CallOpts).

%% prepare_request/4
prepare_request(#diameter_packet{msg = ['ACR' = T | Avps]} = Pkt0, SvcName,
		{_, Caps} = Peer, CallOpts)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _},
		   acct_application_id = {[AcctAppId|_], _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Acct-Application-Id' => [AcctAppId]}],
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	    Pkt0#diameter_packet{msg = Msg}, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer);

prepare_request(#diameter_packet{msg = ['AAR' = T | Avps]} = Pkt0, SvcName,
		{_, Caps} = Peer, CallOpts)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR}],
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	    Pkt0#diameter_packet{msg = Msg}, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer);

prepare_request(#diameter_packet{msg = ['STR' = T | Avps]} = Pkt0, SvcName,
		{_, Caps} = Peer, CallOpts)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR}],
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	    Pkt0#diameter_packet{msg = Msg}, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer);

prepare_request(Pkt0, SvcName, {_PeerRef, _} = Peer, CallOpts) ->
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	       Pkt0, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer).

%% prepare_retransmit/4
prepare_retransmit(Pkt, SvcName, Peer, CallOpts) ->
    prepare_request(Pkt, SvcName, Peer, CallOpts).

%% handle_answer/5
handle_answer(#diameter_packet{msg = Msg}, _Request, SvcName, Peer, _CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    Msg.

%% handle_error/5
handle_error(Reason, _Request, _SvcName, undefined, _CallOpts) ->
    Reason;
handle_error(Reason, _Request, SvcName, Peer, CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:retry_request(Reason, SvcName, Peer, CallOpts).

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(function, Value) when is_atom(Value) ->
    Value;
validate_option(accounting, coupled = Value) ->
    Value;
validate_option(accounting, split = Value) ->
    Value;
validate_option(send_pool_in_AAR, true) ->
    true;
validate_option(send_pool_in_AAR, false) ->
    false;
validate_option('Destination-Host', Value) when is_binary(Value) ->
    [Value];
validate_option('Destination-Host', [Value]) when is_binary(Value) ->
    [Value];
validate_option('Destination-Realm', Value) when is_binary(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

acct_app_alias(#{accounting := coupled}) -> ?APP;
acct_app_alias(#{accounting := split}) -> ?BASE_ACC_APP.

handle_aaa(['AAA' | #{'Result-Code' := RC} = Avps], Session0, Events0, _Opts, State)
  when RC < 3000 ->
    {Session, Events} = to_session({nasreq, 'AAA'}, {Session0, Events0}, Avps),
    {ok, Session, Events, State};
handle_aaa([Answer | #{'Result-Code' := Code}], Session, Events, _Opts, State)
  when Answer =:= 'AAA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, Events, State};
handle_aaa({error, _} = Result, Session, Events, _Opts, State) ->
    {Result, Session, Events, State}.

handle_aca(['ACA' | #{'Result-Code' := RC} = Avps], Session0, Events0, _Opts, State)
  when RC < 3000 ->
    {Session, Events} = to_session({nasreq, 'ACA'}, {Session0, Events0}, Avps),
    {ok, Session, Events, State};
handle_aca([Answer | #{'Result-Code' := RC}], Session, Events, _Opts, State)
  when Answer =:= 'ACA'; Answer =:= 'answer-message' ->
    {{fail, RC}, Session, [stop | Events], State};
handle_aca({error, _} = Result, Session, Events, _Opts, State) ->
    {Result, Session, Events, State}.

handle_sta(['STA' | _Avps], Session, Events, _Opts, State) ->
    {ok, Session, Events, State}.

inc_record_number(#state{record_number = Number} = State) when is_integer(Number) ->
    State#state{record_number = Number + 1};
inc_record_number(State) ->
    State#state{record_number = 0}.

%% to_session/3
to_session(Procedure, {Session0, Events0}, Avps) ->
    Session =
	case Session0 of
	    #{'Acct-Interim-Interval' := Interim}
	      when not is_map_key('Acct-Interim-Interval', Avps),
		   is_integer(Interim), Interim > 0 ->
		Session0#{'Acct-Interim-Interval' => [Interim]};
	    _ ->
		Session0
	end,
    maps:fold(to_session(Procedure, _, _, _), {Session, Events0}, Avps).

%% to_session/4
to_session(_, 'Acct-Interim-Interval', [Interim], {Session, Events})
  when is_integer(Interim), Interim > 0 ->
    Trigger = ergw_aaa_session:trigger(?MODULE, 'IP-CAN', periodic, Interim),
    {Session#{'Acct-Interim-Interval' => Interim}, ergw_aaa_session:ev_set(Trigger, Events)};

to_session(_, 'Framed-IP-Address', [<<A,B,C,D>>], {Session, Events}) ->
    {Session#{'Framed-IP-Address' => {A,B,C,D}}, Events};
to_session(_, 'Framed-IPv6-Prefix', [Prefix], {Session, Events}) ->
    {Session#{'Framed-IPv6-Prefix' => ergw_aaa_diameter:decode_ipv6prefix(Prefix)}, Events};

to_session(_, _, _, SessEv) ->
    SessEv.

format_address({A, B, C, D}) -> <<A, B, C, D>>;
format_address({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
format_address(Addr) -> Addr.

%% from Erlang R21:

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).

system_time_to_universal_time(Time, TimeUnit) ->
    Secs = erlang:convert_time_unit(Time, TimeUnit, second),
    calendar:gregorian_seconds_to_datetime(Secs + ?SECONDS_FROM_0_TO_1970).

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


from_session(Key, Value, M)
  when Key =:= '3GPP-Charging-Gateway-Address';
       Key =:= '3GPP-SGSN-Address';
       Key =:= '3GPP-GGSN-Address';
       Key =:= '3GPP-Charging-Gateway-IPv6-Address';
       Key =:= '3GPP-SGSN-IPv6-Address';
       Key =:= '3GPP-GGSN-IPv6-Address';
       Key =:= '3GPP-IPv6-DNS-Servers';
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
    M#{Key => [ergw_aaa_diameter:'3gpp_from_session'(Key, Value)]};

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
from_session('Framed-IPv6-Prefix' = Key, Value, M) ->
    M#{Key => [ergw_aaa_diameter:encode_ipv6prefix(Value)]};

from_session('Diameter-Session-Id', SId, M) ->
    M#{'Session-Id' => SId};

from_session(Key, Value, M)
  when Key =:= 'Acct-Session-Time';
       Key =:= 'User-Name';
       Key =:= 'NAS-Identifier';
       Key =:= 'NAS-Port-Id';
       Key =:= 'NAS-Port-Type';
       Key =:= 'Class';
       Key =:= 'Called-Station-Id';
       Key =:= 'Calling-Station-Id';
       Key =:= 'Framed-Interface-Id';
       Key =:= 'Framed-Pool';
       Key =:= 'Framed-IPv6-Pool' ->
    M#{Key => [Value]};

from_session(_Key, _Value, M) -> M.

from_session(Session, Avps) ->
    maps:fold(fun from_session/3, Avps, Session).

create_AAR(Type, Session0, Opts) ->
    Username = maps:get('Username', Session0, <<>>),
    Password = maps:get('Password', Session0, <<>>),
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Auth-Request-Type' => Type,
		   'User-Name' => Username,
		   'User-Password' => Password,
		   'Auth-Application-Id' => ?'DIAMETER_APP_ID_NASREQ'},
    Session = case maps:get(send_pool_in_AAR, Opts, false) of
	    false -> maps:without(['Framed-Pool', 'Framed-IPv6-Pool'], Session0);
	    _ -> Session0
	end,
    Avps = from_session(Session, Avps1),
    ['AAR' | Avps].

create_ACR(Type, Session, #{now := Now} = Opts, #state{record_number = RecNumber}) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Accounting-Record-Type'   => Type,
		   'Accounting-Record-Number' => RecNumber,
		   'Event-Timestamp' =>
		       [system_time_to_universal_time(Now + erlang:time_offset(), native)]},
    Avps = from_session(Session, Avps1),
    ['ACR' | Avps].

create_STR(Session, Opts) ->
    Cause = maps:get('Termination-Cause', Session, ?'DEFAULT_TERMINATION_CAUSE'),
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Termination-Cause' => Cause,
		   'Auth-Application-Id' => ?'DIAMETER_APP_ID_NASREQ'},
    Avps = from_session(Session, Avps1),
    ['STR' | Avps].
