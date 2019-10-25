%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

%% Notes : 
%% This module implements split accounting, as described in 
%% https://tools.ietf.org/html/rfc6733#section-9.3
%% - TODO : Implement configuration option for coupled accounting
%% - TODO : Find a "prettier" way to deal with 2 diameter applications
%% The dictionaries are based on 3GPP TS29.061. One particularity
%% is that while RFC 7155 (obsoletes RFC 4005) defines the 
%% Acct-Application-Id as mandatory in the ACR message, the 3GPP
%% spec defines it as optional. This implementation does NOT 
%% include this AVP in the ACR message. 

-module(ergw_aaa_nasreq).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/5]).

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
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/ergw_aaa_session.hrl").
-include("include/diameter_rfc4005_nasreq.hrl").
-include("include/diameter_3gpp_ts29_061_sgi.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(NASREQ_AUTH_APP, nasreq_auth).
-define(NASREQ_ACC_APP, nasreq_acc).
-define(DIAMETER_DICT_NASREQ_AUTH, diameter_3gpp_ts29_061_sgi_auth).
-define(DIAMETER_DICT_NASREQ_ACC, diameter_3gpp_ts29_061_sgi_acc).
-define(DIAMETER_APP_ID_NASREQ_AUTH, ?DIAMETER_DICT_NASREQ_AUTH:id()).
-define(DIAMETER_APP_ID_NASREQ_ACC, ?DIAMETER_DICT_NASREQ_ACC:id()).
-define(DEFAULT_TERMINATION_CAUSE, ?'TERMINATION-CAUSE_PORT_ERROR').

-define(DefaultOptions, [{function, "undefined"},
			 {'Destination-Realm', undefined}]).

-define(IS_IPv4(X), (is_tuple(X) andalso tuple_size(X) == 4)).
-define(IS_IPv6(X), (is_tuple(X) andalso tuple_size(X) == 8)).
-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{function := Function}) ->
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_NASREQ_AUTH,
	  'Acct-Application-Id' => ?DIAMETER_APP_ID_NASREQ_ACC,
	  application => [[{alias, ?NASREQ_AUTH_APP},
			  {dictionary, ?DIAMETER_DICT_NASREQ_AUTH},
			  {module, ?MODULE}],
              [{alias, ?NASREQ_ACC_APP},
			  {dictionary, ?DIAMETER_DICT_NASREQ_ACC},
			  {module, ?MODULE}]]},
    ergw_aaa_diameter_srv:register_service(Function, SvcOpts),
    {ok, []}.

validate_handler(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions, map).

validate_service(_Service, HandlerOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, HandlerOpts, map).

validate_procedure(_Application, _Procedure, _Service, ServiceOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ServiceOpts, map).

invoke(_Service, init, Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, authenticate, Session0, Events, Opts) ->
        Session = ?set_svc_opt('Authorisation', true, Session0),
        RecType = ?'DIAMETER_SGI_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE',
	    Request = create_AAR(RecType, Session, Opts),
	    ergw_aaa_diameter_srv:call(?NASREQ_AUTH_APP, Request, Session, Events, Opts);

invoke(_Service, authorize, Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, start, Session0, Events, Opts) ->
    case ?get_svc_opt('State', Session0, stopped) of
	stopped ->
	    Session1 = ?set_svc_opt('State', started, Session0),
	    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
	    Session = maps:without(Keys, inc_number(Session1)),
	    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_START_RECORD',
	    Request = create_ACR(RecType, Session, Opts),
	    ergw_aaa_diameter_srv:call(?NASREQ_ACC_APP, Request, Session, Events, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, interim, Session0, Events, Opts) ->
    case ?get_svc_opt('State', Session0, stopped) of
	started ->
	    Session = inc_number(Session0),
	    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD',
	    Request = create_ACR(RecType, Session, Opts),
	    ergw_aaa_diameter_srv:call(?NASREQ_ACC_APP, Request, Session, Events, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, stop, Session0, Events0, Opts) ->
    lager:debug("Session Stop: ~p", [Session0]),
    case ?get_svc_opt('State', Session0, stopped) of
	started ->
	    Session1 = ?set_svc_opt('State', stopped, Session0),
	    Session2 = inc_number(Session1),
	    RecType = ?'DIAMETER_SGI_ACCOUNTING-RECORD-TYPE_STOP_RECORD',
	    ACRRequest = create_ACR(RecType, Session2, Opts),
		ACRRes = ergw_aaa_diameter_srv:call(?NASREQ_ACC_APP, ACRRequest, Session2, Events0, Opts),
        case ?get_svc_opt('Authorisation', Session0, false) of
            true ->
        	    STRRequest = create_STR(Session2, Opts),
	            ergw_aaa_diameter_srv:call(?NASREQ_AUTH_APP, STRRequest, Session2, Events0, Opts);
            _ -> ok
        end,
        ACRRes;
	_ ->
	    {ok, Session0, Events0}
    end;

invoke(Service, Procedure, Session, Events, _Opts) ->
    {{error, {Service, Procedure}}, Session, Events}.

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

%% peer_up/3
peer_up(_SvcName, _Peer, State) ->
    lager:debug("peer_up: ~p~n", [_Peer]),
    State.

%% peer_down/3
peer_down(SvcName, Peer, State) ->
    ergw_aaa_diameter_srv:peer_down(?MODULE, SvcName, Peer),
    lager:debug("peer_down: ~p~n", [Peer]),
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
		   origin_realm = {OR, _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR}],
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
handle_answer(#diameter_packet{msg = [_ | Avps] = Msg}, ['ACR' | _], SvcName, Peer, CallOpts)
  when is_map(Avps) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:handle_answer(fun handle_aca/4, Msg, CallOpts);

handle_answer(#diameter_packet{msg = [_ | Avps] = Msg}, ['AAR' | _], SvcName, Peer, CallOpts)
  when is_map(Avps) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:handle_answer(fun handle_aaa/4, Msg, CallOpts);

handle_answer(#diameter_packet{msg = [_ | Avps] = Msg}, ['STR' | _], SvcName, Peer, CallOpts)
  when is_map(Avps) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:handle_answer(fun handle_sta/4, Msg, CallOpts);

handle_answer(#diameter_packet{msg = Msg}, _Request, SvcName, Peer, _CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    Msg.

%% handle_error/5
handle_error(Reason, _Request, _SvcName, undefined, CallOpts) ->
    ergw_aaa_diameter_srv:handle_answer(fun handle_aca/4, Reason, CallOpts);
handle_error(Reason, _Request, SvcName, Peer, CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:retry(fun handle_aca/4, Reason, SvcName, Peer, CallOpts).

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(function, Value) when is_atom(Value) ->
    Value;
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

handle_aaa(['AAA' | #{'Result-Code' := RC} = Avps], Session0, Events0, _Opts) 
    when RC < 3000 ->
    {Session, Events} = to_session({nasreq, 'AAA'}, {Session0, Events0}, Avps),
    {ok, Session, Events};
handle_aaa([Answer | #{'Result-Code' := Code}], Session, Events, _Opts)
  when Answer =:= 'AAA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, Events};
handle_aaa({error, _} = Result, Session, Events, _Opts) ->
    {Result, Session, Events}.

handle_aca(['ACA' | #{'Result-Code' := RC} = Avps], Session0, Events0, _Opts) 
    when RC < 3000 ->
    {Session, Events} = to_session({nasreq, 'ACA'}, {Session0, Events0}, Avps),
    {ok, Session, Events};
handle_aca([Answer | #{'Result-Code' := RC}], Session, Events, _Opts)
  when Answer =:= 'ACA'; Answer =:= 'answer-message' ->
    {{fail, RC}, Session, [stop | Events]};
handle_aca({error, _} = Result, Session, Events, _Opts) ->
    {Result, Session, Events}.

handle_sta(['STA' | _Avps], Session, Events, _Opts) ->
    {ok, Session, Events}.

inc_number(Session) ->
    Number = ?get_svc_opt('Accounting-Record-Number', Session, -1) + 1,
    ?set_svc_opt('Accounting-Record-Number', Number, Session).

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

from_service('Accounting-Record-Number' = Key, Value, M) ->
    M#{Key => Value};
from_service(_, _, M) ->
    M.

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
       Key =:= 'Framed-Interface-Id' ->
    M#{Key => [Value]};

from_session(?MODULE, Value, M) ->
    maps:fold(fun from_service/3, M, Value);

from_session(_Key, _Value, M) -> M.

from_session(Session, Avps) ->
    maps:fold(fun from_session/3, Avps, Session).

create_AAR(Type, Session, Opts) ->
    Username = maps:get('Username', Session, <<>>),
    Password = maps:get('Password', Session, <<>>),
    FramedIP = maps:get('Framed-IP-Address', Session, [<<0,0,0,0>>]),
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Auth-Request-Type' => Type, 'User-Name' => Username, 
		   'User-Password' => Password, 'Framed-IP-Address' => FramedIP,
           'Auth-Application-Id' => ?'DIAMETER_APP_ID_NASREQ_AUTH'},
    % remove 3GPP-GPRS-Negotiated-QoS-Profile until we fix the encoding according to 
    % https://www.etsi.org/deliver/etsi_ts/129000_129099/129061/15.03.00_60/ts_129061v150300p.pdf
    % page 90
    Session1 = maps:without(['3GPP-GPRS-Negotiated-QoS-Profile'], Session),
    Avps = from_session(Session1, Avps1),
    ['AAR' | Avps].

create_ACR(Type, Session, #{now := Now} = Opts) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Accounting-Record-Type' => Type,
		   'Event-Timestamp' =>
		       [system_time_to_universal_time(Now + erlang:time_offset(), native)]},
    % remove 3GPP-GPRS-Negotiated-QoS-Profile until we fix the encoding according to 
    % https://www.etsi.org/deliver/etsi_ts/129000_129099/129061/15.03.00_60/ts_129061v150300p.pdf
    % page 90
    Session1 = maps:without(['3GPP-GPRS-Negotiated-QoS-Profile'], Session),
    Avps = from_session(Session1, Avps1),
    ['ACR' | Avps].

create_STR(Session, Opts) ->
    Cause = maps:get('Termination-Cause', Session, ?'DEFAULT_TERMINATION_CAUSE'),
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Termination-Cause' => Cause},
    Avps = from_session(Session, Avps1),
    ['STR' | Avps].

