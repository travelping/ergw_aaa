%% Copyright 2017-2019 Travelping GmbH <info@travelping.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(ergw_aaa_gx).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/6, handle_response/6]).
-export([to_session/3, from_session/2]).

-export([get_state_atom/1]).

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
-include("include/diameter_3gpp_ts29_212.hrl").
-include("include/ergw_aaa_gx.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, 'Gx').
-define(DIAMETER_DICT_GX, diameter_3gpp_ts29_212).
-define(DIAMETER_APP_ID_GX, ?DIAMETER_DICT_GX:id()).
-define(API, gx).

-define(DefaultOptions, [{function, undefined},
			 {'Destination-Realm', undefined},
			 {termination_cause_mapping, #{}}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

-record(state, {pending = undefined :: 'undefined' | reference(),
		state = init       :: atom(),
		request_number     :: 'undefined' | integer()
	       }).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{function := Function}) ->
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
	  'Vendor-Specific-Application-Id' =>
	      [#'diameter_base_Vendor-Specific-Application-Id'{
		  'Vendor-Id'           = ?VENDOR_ID_3GPP,
		  'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}],
	  application => [{alias, ?APP},
			  {answer_errors, callback},
			  {dictionary, ?DIAMETER_DICT_GX},
			  {module, ?MODULE}]},
    ergw_aaa_diameter_srv:register_service(Function, SvcOpts),
    {ok, []}.

validate_handler(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions).

validate_service(_Service, HandlerOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, HandlerOpts).

validate_procedure(_Application, _Procedure, _Service, ServiceOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ServiceOpts).

invoke(_Service, init, Session, Events, _Opts, _State) ->
    {ok, Session, Events, #state{state = stopped}};

invoke(_Service, {_, 'CCR-Initial'}, Session0, Events, Opts,
       #state{state = stopped} = State0) ->
    State = inc_request_number(State0#state{state = started}),
    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
    Session = maps:without(Keys, Session0),
    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_INITIAL_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, 'CCR-Update'}, Session, Events, Opts,
       #state{state = started} = State0) ->
    State = inc_request_number(State0),
    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_UPDATE_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, 'CCR-Terminate'}, Session, Events, Opts,
       #state{state = started} = State0) ->
    ?LOG(debug, "Session Stop: ~p", [Session]),
    State = inc_request_number(State0#state{state = stopped}),
    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, terminate, Session, Events, Opts,
       #state{state = started} = State0) ->
    ?LOG(debug, "Session Termination: ~p", [Session]),
    State = inc_request_number(State0#state{state = stopped}),
    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, Procedure}, Session, Events, _Opts, State)
  when Procedure =:= 'CCR-Initial';
       Procedure =:= 'CCR-Update';
       Procedure =:= 'CCR-Terminate' ->
    {ok, Session, Events, State};
invoke(_Service, terminate, Session, Events, _Opts, State) ->
    {ok, Session, Events, State};

invoke(Service, Procedure, Session, Events, _Opts, State) ->
    {{error, {Service, Procedure}}, Session, Events, State}.

%%%===================================================================
%%% ergw_aaa_diameter_srv wrapper
%%%===================================================================

send_request(Request, Config) ->
    ergw_aaa_diameter_srv:send_request(?MODULE, ?APP, Request, Config).

await_response(Promise, Session, Events, State, #{async := true}) ->
    {ok, Session, Events, State#state{pending = Promise}};
await_response(Promise, Session, Events, State, Opts) ->
    Msg = ergw_aaa_diameter_srv:await_response(Promise),
    handle_cca(Msg, Session, Events, Opts, State).

%% handle_response/6
handle_response(Promise, Msg, Session, Events, Opts, #state{pending = Promise} = State) ->
    handle_cca(Msg, Session, Events, Opts, State#state{pending = undefined});
handle_response(_Promise, _Msg, Session, Events, _Opts, State) ->
    {ok, Session, Events, State}.

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
prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]} = Pkt0, SvcName,
		{_, Caps} = Peer, CallOpts)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _},
		   origin_state_id = {OSid, _}} = Caps,

    SF = case catch(ergw_aaa_diameter_srv:is_first_request(?MODULE, SvcName, Peer)) of
	     false ->
		 [];
	     _ ->
		 [#{'Vendor-Id'       => diameter_3gpp_base:vendor_id(),
		    'Feature-List-ID' => 1,
		    'Feature-List' =>
			?'DIAMETER_GX_FEATURE-ID-1_Rel8' bor
			?'DIAMETER_GX_FEATURE-ID-1_Rel9' bor
			?'DIAMETER_GX_FEATURE-ID-1_Rel10'}]
	 end,
    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Origin-State-Id' => OSid,
		     'Supported-Features' => SF}],
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	    Pkt0#diameter_packet{msg = Msg}, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer);

prepare_request(Pkt0, SvcName, {_PeerRef, _} = Peer, CallOpts) ->
    Pkt = ergw_aaa_diameter_srv:prepare_request(
	       Pkt0, SvcName, Peer, CallOpts),
    ergw_aaa_diameter_srv:start_request(Pkt, SvcName, Peer).

%% prepare_retransmit/4
prepare_retransmit(_Pkt, _SvcName, _Peer, _CallOpts) ->
    false.

%% handle_answer/5
handle_answer(#diameter_packet{msg = Msg, errors = Errors},
	      _Request, SvcName, Peer, _CallOpts)
  when length(Errors) /= 0 ->
    ?LOG(error, "~p: decode of answer from ~p failed, errors ~p", [SvcName, Peer, Errors]),
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    case Msg of
	[_ | #{'Result-Code' := RC}] -> {error, RC};	%% try to handle gracefully
	_                            -> {error, failed}
    end;

handle_answer(#diameter_packet{msg = Msg}, _Request, SvcName, Peer, _CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    Msg.

%% handle_error/5
handle_error(Reason, _Request, _SvcName, undefined, _CallOpts) ->
    Reason;
handle_error(Reason, _Request, SvcName, Peer, CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    ergw_aaa_diameter_srv:retry_request(Reason, SvcName, Peer, CallOpts).

%% handle_request/3
handle_request(#diameter_packet{msg = [Command | Avps]}, _SvcName, Peer)
  when Command =:= 'ASR'; Command =:= 'RAR' ->
    handle_common_request(Command, Avps, Peer);
handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(handler, ?MODULE) ->
    ?MODULE;
validate_option(function, Value) ->
    Value;
validate_option(service, Value) ->
    Value;
validate_option('Destination-Host', Value) when is_binary(Value) ->
    [Value];
validate_option('Destination-Host', [Value]) when is_binary(Value) ->
    [Value];
validate_option('Destination-Realm', Value) when is_binary(Value) ->
    Value;
validate_option(answers, Value) when is_map(Value) ->
    ergw_aaa_config:validate_answers(Value);
validate_option(answer_if_down, Value) ->
    Value;
validate_option(answer_if_timeout, Value) ->
    Value;
validate_option(avp_filter, Value) when is_list(Value) ->
    Value;
validate_option(termination_cause_mapping, Value) ->
    ergw_aaa_diameter:validate_termination_cause_mapping(Value);
validate_option(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

%%===================================================================
%% internal helpers
%%===================================================================

%% handle_cca/5
handle_cca(['answer-message' = Answer | #{'Result-Code' := RC} = AVPs],
	   Session, Events, Opts, State) ->
    handle_cca(Answer, RC, AVPs, Session, Events, Opts, State);
handle_cca([Answer | #{'Result-Code' := [RC]} = AVPs],
	   Session, Events, Opts, State) ->
    handle_cca(Answer, RC, AVPs, Session, Events, Opts, State);
handle_cca([Answer |
	    #{'Experimental-Result' :=
		  [#{'Vendor-Id' := ?VENDOR_ID_3GPP,
		     'Experimental-Result-Code' := RC}]} = AVPs],
	   Session, Events, Opts, State) ->
    handle_cca(Answer, RC, AVPs, Session, Events, Opts, State);
handle_cca({error, no_connection}, Session, Events,
	   #{answer_if_down := Answer, answers := Answers} = Opts, State) ->
    Avps = apply_answer_config(Answer, Answers),
    handle_cca(['CCA' | Avps], Session, Events, Opts, State#state{state = peer_down});
handle_cca({error, no_connection}, Session, Events,
	   #{answer_if_timeout := Answer, answers := Answers} = Opts, State) ->
    Avps = apply_answer_config(Answer, Answers),
    handle_cca(['CCA' | Avps], Session, Events, Opts, State);
handle_cca({error, Reason} = Result, Session, Events, _Opts, State) ->
    ?LOG(error, "CCA Result: ~p", [Result]),
    {Result, Session, [{stop, {?API, Reason}} | Events], State#state{state = stopped}}.

%% handle_cca/7
handle_cca('CCA', RC, AVPs, Session0, Events0, _Opts, State)
  when RC < 3000 ->
    {Session, Events} = to_session({?API, 'CCA'}, {Session0, Events0}, AVPs),
    {ok, Session, Events, State};
handle_cca(Answer, RC, _AVPs, Session, Events, _Opts, State)
  when Answer =:= 'CCA'; Answer =:= 'answer-message' ->
    {{fail, RC}, Session, [{stop, {?API, peer_reject}} | Events], State#state{state = stopped}}.

handle_common_request(Command, #{'Session-Id' := SessionId} = Avps, {_PeerRef, Caps}) ->
    {Result, ReplyAvps0} =
	case ergw_aaa_session_reg:lookup(SessionId) of
	    Session when is_pid(Session) ->
		ergw_aaa_session:request(Session, ?MODULE, {?API, Command}, Avps);
	    _ ->
		{{error, unknown_session}, #{}}
	end,

    #diameter_caps{origin_host = {OH,_},
		   origin_realm = {OR,_},
		   origin_state_id = {OSid, _}} = Caps,

    ReplyAvps1 = filter_reply_avps(Command, ReplyAvps0),
    ReplyAvps2 =
	ReplyAvps1#{'Origin-Host' => OH,
		    'Origin-Realm' => OR,
		    'Origin-State-Id' => OSid,
		    'Session-Id' => SessionId},
    ReplyCode = diameter_reply_code(Command),
    ReplyAvps = diameter_reply_avps(Result, ReplyAvps2),
    ?LOG(debug, "~p reply Avps: ~p", [Command, ReplyAvps]),
    {reply, [ReplyCode | ReplyAvps]}.

inc_request_number(#state{request_number = Number} = State) when is_integer(Number) ->
    State#state{request_number = Number + 1};
inc_request_number(State) ->
    State#state{request_number = 0}.

diameter_reply_code('ASR') -> 'ASA';
diameter_reply_code('RAR') -> 'RAA'.

diameter_reply_avps({ok, Reply}, _) ->
    Reply#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'};

diameter_reply_avps(ok, Reply) ->
    Reply#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'};

diameter_reply_avps({error, unknown_session}, Reply) ->
    Reply#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_UNKNOWN_SESSION_ID'};

diameter_reply_avps(_, Reply) ->
    Reply#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_COMPLY'}.

filter_reply_avps('RAR', Avps) ->
    Permited =
	['OC-OLR', 'IP-CAN-Type', 'RAT-Type',
	 'AN-Trusted', 'AN-GW-Address',
	 '3GPP-SGSN-MCC-MNC', '3GPP-SGSN-Address', '3GPP-SGSN-IPv6-Address',
	 'RAI', '3GPP-User-Location-Info', 'User-Location-Info-Time',
	 'NetLoc-Access-Support', 'User-CSG-Information',
	 '3GPP-MS-TimeZone', 'Default-QoS-Information',
	 'Charging-Rule-Report'],
    maps:with(Permited, Avps);
filter_reply_avps('ASR', Avps) ->
    Permited = ['User-Name'],
    maps:with(Permited, Avps);
filter_reply_avps(_, Avps) ->
    Avps.

%%%===================================================================
apply_answer_config(Answer, Answers) ->
    case Answers of
	#{Answer := #{avps := A}} -> A;
	_ -> #{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'}
    end.

assign([Key], Fun, Avps) ->
    Fun(Key, Avps);
assign([Key | Next], Fun, Avps) ->
    [V] = maps:get(Key, Avps, [#{}]),
    Avps#{Key => [assign(Next, Fun, V)]}.

repeated(Keys, Value, Avps) when is_list(Keys) ->
    assign(Keys, repeated(_, Value, _), Avps);
repeated(Key, Value, Avps)
  when is_atom(Key) ->
    maps:update_with(Key, fun(V) -> [Value|V] end, [Value], Avps).

%% optional(Keys, Value, Avps) when is_list(Keys) ->
%%     assign(Keys, optional(_, Value, _), Avps);
%% optional(Key, Value, Avps)
%%   when is_atom(Key) ->
%%     Avps#{Key => [Value]}.

%%%===================================================================

ip2bin({A,B,C,D}) ->
    <<A,B,C,D>>;
ip2bin(Bin) when is_binary(Bin)->
    Bin.

map_update_with(Key, Fun, Init, Map) ->
    V = maps:get(Key, Map, Init),
    Map#{Key => Fun(V)}.

session_to_usu_key('InOctets')  -> 'CC-Input-Octets';
session_to_usu_key('OutOctets') -> 'CC-Output-Octets'.

dynamic_address_flag(Key, {0,0,0,0}, Avps) ->
    Avps#{Key => [1]};
dynamic_address_flag(Key, {{0,0,0,0,0,0,0,0},_}, Avps) ->
    Avps#{Key => [1]};
dynamic_address_flag(_Key, _, Avps) ->
    Avps.

dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv4v6',
		       'Requested-IP-Address' := IP4,
		       'Requested-IPv6-Prefix' := IP6}, Avps0) ->
    Avps = dynamic_address_flag('Dynamic-Address-Flag-Extension', IP4, Avps0),
    dynamic_address_flag('Dynamic-Address-Flag', IP6, Avps);
dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv4',
		       'Requested-IP-Address' := IP4}, Avps) ->
    dynamic_address_flag('Dynamic-Address-Flag', IP4, Avps);
dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv6',
		       'Requested-IPv6-Prefix' := IP6}, Avps) ->
    dynamic_address_flag('Dynamic-Address-Flag', IP6, Avps);
dynamic_address_flag(_Session, Avps) ->
    Avps.

umi_session_init() ->
    [#{'Monitoring-Key'           => [<<"default">>],
       'Used-Service-Unit'        => [#{}],
       'Usage-Monitoring-Level'   => [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL']}].

umi_from_session(Key, Value, [#{'Used-Service-Unit' := [USU]} = UMI]) ->
    [UMI#{'Used-Service-Unit' := [USU#{session_to_usu_key(Key) => [Value]}]}].

from_session('Diameter-Session-Id', SId, M) ->
    M#{'Session-Id' => SId};

%% 'Subscription-Id'
from_session('3GPP-IMSI', IMSI, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_GX_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	   'Subscription-Id-Data' => IMSI},
    repeated('Subscription-Id', SI, Avps);
from_session('3GPP-MSISDN', MSISDN, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_GX_SUBSCRIPTION-ID-TYPE_END_USER_E164',
	   'Subscription-Id-Data' => MSISDN},
    repeated('Subscription-Id', SI, Avps);

%% 'OC-Supported-Features'
%% 'TDF-Information'
%% 'Network-Request-Support'
%% 'Packet-Filter-Information'
%% 'Packet-Filter-Operation'
%% 'Bearer-Identifier'

%% 'Bearer-Operation'
from_session('Bearer-Operation', Value, Avps) ->
    Avps#{'Bearer-Operation' => [Value]};

%% 'PDN-Connection-Charging-ID'
from_session('3GPP-Charging-Id', Value, Avps) ->
    Avps#{'PDN-Connection-Charging-ID' => [Value]};

%% 'Framed-IP-Address'
from_session('Framed-IP-Address', IP, Avps) ->
    Avps#{'Framed-IP-Address' => [ip2bin(IP)]};

%% 'Framed-IPv6-Prefix'
from_session('Framed-IPv6-Prefix', Prefix, Avps) ->
    Avps#{'Framed-IPv6-Prefix' => ergw_aaa_diameter:decode_ipv6prefix(Prefix)};

%% 'IP-CAN-Type'

%% '3GPP-RAT-Type'
from_session(Key, Value, Avps)
  when Key =:= '3GPP-Selection-Mode';
       Key =:= '3GPP-Charging-Characteristics';
       Key =:= '3GPP-SGSN-MCC-MNC';
       Key =:= '3GPP-MS-TimeZone';
       Key =:= '3GPP-User-Location-Info';
       Key =:= '3GPP-RAT-Type' ->
    Avps#{Key => [ergw_aaa_diameter:'3gpp_from_session'(Key, Value)]};

%% 'AN-Trusted'
%% 'RAT-Type'

%% 'User-Equipment-Info'
from_session('3GPP-IMEISV', IMEI, Avps) ->
    UE = #{'User-Equipment-Info-Type' =>
	       ?'DIAMETER_GX_USER-EQUIPMENT-INFO-TYPE_IMEISV',
	   'User-Equipment-Info-Value' => IMEI},
    Avps#{'User-Equipment-Info' => UE};

%% 'QoS-Information'
from_session('QoS-Information' = Key, Value, Avps) ->
    Avps#{Key => [ergw_aaa_diameter:qos_from_session(Value)]};

%% 'QoS-Negotiation'
%% 'QoS-Upgrade'
%% 'Default-EPS-Bearer-QoS'
%% 'Default-QoS-Information'
%% 'AN-GW-Address'
%% 'AN-GW-Status'

%% '3GPP-SGSN-Address'
from_session('3GPP-SGSN-Address', Value, M) ->
    M#{'3GPP-SGSN-Address' => [ip2bin(Value)]};

%% '3GPP-SGSN-IPv6-Address'
from_session('3GPP-SGSN-IPv6-Address', Value, M) ->
    M#{'3GPP-SGSN-IPv6-Address' => [ip2bin(Value)]};

%% '3GPP-GGSN-Address'
from_session('3GPP-GGSN-Address', Value, M) ->
    M#{'3GPP-GGSN-Address' => [ip2bin(Value)]};

%% '3GPP-GGSN-IPv6-Address'
from_session('3GPP-GGSN-IPv6-Address', Value, M) ->
    M#{'3GPP-GGSN-IPv6-Address' => [ip2bin(Value)]};

%% 'RAI'
%% 'Fixed-User-Location-Info'
%% 'User-Location-Info-Time'
%% 'User-CSG-Information'
%% 'TWAN-Identifier'
%% 'RAN-NAS-Release-Cause'

%% 'Called-Station-Id'
from_session('Called-Station-Id', Value, M) ->
    M#{'Called-Station-Id' => [Value]};

%% 'PDN-Connection-ID'
%% 'Bearer-Usage'
%% 'Online'
%% 'Offline'
%% 'TFT-Packet-Filter-Information'
%% 'Charging-Rule-Report'
%% 'Application-Detection-Information'
%% 'Event-Trigger'

from_session('Event-Trigger', Value, M) when is_integer(Value) ->
    M#{'Event-Trigger' => [Value]};
from_session('Event-Trigger', Value, M) when is_list(Value) ->
    M#{'Event-Trigger' => Value};

%% 'Event-Report-Indication'
%% 'Access-Network-Charging-Address'
%% 'Access-Network-Charging-Identifier-Gx'
%% 'CoA-Information'
%% 'Usage-Monitoring-Information'
%% 'NBIFOM-Support'
%% 'NBIFOM-Mode'
%% 'Default-Access'
%% 'Origination-Time-Stamp'
%% 'Maximum-Wait-Time'
%% 'Access-Availability-Change-Reason'
%% 'Routing-Rule-Install'
%% 'Routing-Rule-Remove'
%% 'HeNB-Local-IP-Address'
%% 'UE-Local-IP-Address'
%% 'UDP-Source-Port'
%% 'TCP-Source-Port'
%% 'Presence-Reporting-Area-Information'
%% 'Logical-Access-ID'
%% 'Physical-Access-ID'
%% '3GPP-PS-Data-Off-Status'

from_session(Key, Value, M)
  when Key =:= 'InOctets';
       Key =:= 'OutOctets' ->
    map_update_with('Usage-Monitoring-Information',
		    fun(V) -> umi_from_session(Key, Value, V) end,
		    umi_session_init(), M);

from_session(_Key, _Value, M) ->
    M.

from_session(Session, Avps0) ->
    Avps = dynamic_address_flag(Session, Avps0),
    maps:fold(fun from_session/3, Avps, Session).

%% {'diameter_gx_Granted-Service-Unit',[],[600],[],[],[],[],[],[]},
%% {'diameter_gx_Granted-Service-Unit',[],[],[],[1000],[1000],[1000],[],[]}],

gsu_to_session(Key, #'diameter_gx_Granted-Service-Unit'{
		       'CC-Time' = [Interval]}, {Session0, Events}) ->
    Session = Session0#{
			'Monitoring-Key'     => Key,
			'Interim-Accounting' => Interval * 1000
		       },
    {Session, Events};
gsu_to_session(_, _GSU, SessEv) ->
    SessEv.

umi_to_session(#'diameter_gx_Usage-Monitoring-Information'{
		  'Monitoring-Key' = Key,
		  'Granted-Service-Unit' = GSU,
		  'Usage-Monitoring-Level' =
		      [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL']
		 }, SessEv) ->
    lists:foldl(fun(G, S) -> gsu_to_session(Key, G, S) end, SessEv, GSU);
umi_to_session(_, SessEv) ->
    SessEv.

%% to_session/3
to_session(Procedure, SessEvs, Avps) ->
    maps:fold(to_session(Procedure, _, _, _), SessEvs, Avps).

%% to_session/4
to_session(_, 'Usage-Monitoring-Information', Value, SessEv) ->
    lists:foldl(fun umi_to_session/2, SessEv, Value);
to_session(_, 'Charging-Rule-Install', V, {Session, Events}) ->
    [?LOG(info, "CRI: ~p", [R]) || R <- V],
    {Session, [{pcc, install, V} | Events]};
to_session(_, 'Charging-Rule-Remove', V, {Session, Events}) ->
    [?LOG(info, "CRI: ~p", [R]) || R <- V],
    {Session, [{pcc, remove, V} | Events]};
to_session(_, _, _, SessEv) ->
    SessEv.

%% make_request(Type, #{sid := SId, cc_req_number := ReqNum}) ->
%%     #{'Session-Id'          => SId,
%%       'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
%%       'CC-Request-Type'     => Type,
%%       'CC-Request-Number'   => ReqNum}.

termination_cause(Session, Avps) ->
    Cause = maps:get('Termination-Cause', Session, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT'),
    Avps#{'Termination-Cause' => Cause}.

make_CCR(Type, Session, Opts, #state{request_number = ReqNumber}) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
		   'CC-Request-Type'     => Type,
		   'CC-Request-Number'   => ReqNumber},
    Avps2 = if Type =:=	?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST' ->
		    termination_cause(Session, Avps1);
	       true ->
		    Avps1
	    end,
    Avps = from_session(Session, Avps2),
    ['CCR' | Avps ].

get_state_atom(#state{state = State}) ->
    State.
