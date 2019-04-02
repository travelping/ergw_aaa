%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>
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
	 initialize_handler/1, initialize_service/2, invoke/5]).

%% diameter callbacks
-export([peer_up/3,
	 peer_down/3,
	 pick_peer/4, pick_peer/5,
	 prepare_request/3, prepare_request/4,
	 prepare_retransmit/3, prepare_retransmit/4,
	 handle_answer/4, handle_answer/5,
	 handle_error/4,
	 handle_request/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/diameter_3gpp_ts29_212.hrl").
-include("include/ergw_aaa_gx.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, 'Gx').
-define(DIAMETER_DICT_GX, diameter_3gpp_ts29_212).
-define(DIAMETER_APP_ID_GX, ?DIAMETER_DICT_GX:id()).

-define(DefaultOptions, [{function, "undefined"},
			 {'Destination-Realm', undefined}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

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
			  {dictionary, ?DIAMETER_DICT_GX},
			  {module, ?MODULE}]},
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

invoke(_Service, authenticate, Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, authorize, #{'Authentication-Result' := success} = Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, authorize, Session, Events, _Opts) ->
    {denied, Session, Events};

invoke(_Service, {_, 'CCR-Initial'}, Session0, Events, Opts) ->
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	stopped ->
	    Session1 = ergw_aaa_session:set_svc_opt(
			 ?MODULE, DiamSession#{'State' => 'started'}, Session0),
	    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
	    Session = maps:without(Keys, inc_number(Session1)),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_INITIAL_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, {_, 'CCR-Update'}, Session0, Events, Opts) ->
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	started ->
	    Session = inc_number(Session0),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_UPDATE_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, {_, 'CCR-Terminate'}, Session0, Events, Opts) ->
    lager:debug("Session Stop: ~p", [Session0]),
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	started ->
	    Session1 = ergw_aaa_session:set_svc_opt(
			 ?MODULE, DiamSession#{'State' => 'stopped'}, Session0),
	    Session = inc_number(Session1),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(Service, Procedure, Session, Events, _Opts) ->
    {{error, {Service, Procedure}}, Session, Events}.

call(Request, #{function := Function}) ->
    diameter:call(Function, ?APP, Request, []).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State) ->
    lager:debug("peer_up: ~p~n", [_Peer]),
    State.

peer_down(_SvcName, {PeerRef, _} = _Peer, State) ->
    ergw_aaa_diameter_srv:peer_down(?MODULE, PeerRef),
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

prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]}, _, {PeerRef, Caps})
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _},
		   origin_state_id = {OSid, _}} = Caps,

    SF = case catch(ergw_aaa_diameter_srv:is_first_request(?MODULE, PeerRef)) of
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
    lager:debug("prepare_request Msg: ~p", [Msg]),
    {send, Msg};

prepare_request(Packet, _SvcName, {PeerRef, _}) ->
    lager:debug("prepare_request to ~p: ~p", [PeerRef, lager:pr(Packet, ?MODULE)]),
    {send, Packet}.

prepare_request(Packet, SvcName, Peer, _From) ->
    prepare_request(Packet, SvcName, Peer).

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

prepare_retransmit(Packet, SvcName, Peer, _From) ->
    prepare_request(Packet, SvcName, Peer).

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer) ->
    Msg.

handle_answer(#diameter_packet{msg = ['CCA' | Avps] = Msg}, _Request, _SvcName, _Peer, From)
  when is_map(Avps), is_pid(From) ->
    From ! Msg,
    Msg;

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer, _From) ->
    Msg.

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

handle_request(#diameter_packet{msg = [Command | Avps]}, _SvcName, Peer)
  when Command =:= 'ASR'; Command =:= 'RAR' ->
    handle_common_request(Command, Avps, Peer);
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
validate_option(answers, Value) when is_map(Value) ->
    Value;
validate_option(answer_if_down, Value) when is_atom(Value) ->
    Value;
validate_option(answer_if_timeout, Value) when is_atom(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

handle_cca(['CCA' | #{'Result-Code' := [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']} = Avps],
	   Session0, Events0, _Opts) ->
    {Session, Events} = maps:fold(fun to_session/3, {Session0, Events0}, Avps),
    {ok, Session, Events};
handle_cca([Answer | #{'Result-Code' := Code}], Session, Events, _Opts)
  when Code == ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED' andalso
       (Answer =:= 'CCA' orelse Answer =:= 'answer-message') ->
    {{fail, Code}, Session, [stop | Events]};
handle_cca([Answer | #{'Result-Code' := Code}], Session, Events, _Opts)
  when Answer =:= 'CCA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, Events};
handle_cca({error, no_connection}, Session, Events,
	   #{answer_if_down := Answer, answers := Answers} = Opts) ->
    Avps = maps:get(Answer, Answers, #{'Result-Code' =>
					   ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'}),
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session),
    NewSession = ergw_aaa_session:set_svc_opt(
		   ?MODULE, DiamSession#{'State' => peer_down}, Session),
    handle_cca(['CCA' | Avps], NewSession, Events, Opts);
handle_cca({error, no_connection}, Session, Events,
	   #{answer_if_timeout := Answer, answers := Answers} = Opts) ->
    Avps = maps:get(Answer, Answers, #{'Result-Code' =>
					   ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'}),
    handle_cca(['CCA' | Avps], Session, Events, Opts);
handle_cca({error, _} = Result, Session, Events, _Opts) ->
    lager:error("CCA Result: ~p", [Result]),
    {Result, Session, [stop | Events]}.

handle_common_request(Command, #{'Session-Id' := SessionId} = Avps, {_PeerRef, Caps}) ->
    {Result, ReplyAvps0} =
	case ergw_aaa_session_reg:lookup(SessionId) of
	    Session when is_pid(Session) ->
		ergw_aaa_session:request(Session, {'gy', Command}, Avps);
	    _ ->
		{{error, unknown_session}, #{}}
	end,

    #diameter_caps{origin_host = {OH,_},
		   origin_realm = {OR,_},
		   origin_state_id = {OSid, _}} = Caps,

    ReplyAvps1 =
	ReplyAvps0#{'Origin-Host' => OH,
		    'Origin-Realm' => OR,
		    'Origin-State-Id' => OSid,
		    'Session-Id' => SessionId},
    ReplyCode = diameter_reply_code(Command),
    ReplyAvps = diameter_reply_avps(Result, ReplyAvps1),
    lager:debug("~p reply Avps: ~p", [Command, ReplyAvps]),
    {reply, [ReplyCode | ReplyAvps]}.

inc_number(Session) ->
    ModuleOpts = maps:get(?MODULE, Session, #{}),
    Number = maps:get('CC-Request-Number', ModuleOpts, -1),
    Session#{?MODULE => ModuleOpts#{'CC-Request-Number' => Number + 1}}.

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

%%%===================================================================

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

from_service('CC-Request-Number' = Key, Value, M) ->
    M#{Key => Value};
from_service(_, _, M) ->
    M.

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
from_session('Framed-IPv6-Prefix', {IP, PrefixLen}, Avps) ->
    Avps#{'Framed-IPv6-Prefix' => [<<0:4, PrefixLen:4, (ip2bin(IP))/binary>>]};

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

from_session(?MODULE, Value, M) ->
    maps:fold(fun from_service/3, M, Value);

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

add_rule(Key, Rule, {Session0, Events}) ->
    Session = maps:update_with(Key, fun(V) -> [Rule|V] end, [Rule], Session0),
    {Session, Events}.

del_rule(Key, Rule, {Session0, Events}) ->
    Session = maps:update_with(Key, fun(V) -> lists:delete(Rule, V) end, [], Session0),
    {Session, Events}.

rule_to_session(#'diameter_gx_Charging-Rule-Install'{
		   'Charging-Rule-Base-Name' = Bases,
		   'Charging-Rule-Name' = Rules}, SessEv0) ->
    SessEv = lists:foldl(add_rule('PCC-Rules', _, _), SessEv0, Rules),
    lists:foldl(add_rule('PCC-Groups', _, _), SessEv, Bases);
rule_to_session(#'diameter_gx_Charging-Rule-Remove'{
		   'Charging-Rule-Base-Name' = Bases,
		   'Charging-Rule-Name' = Rules}, SessEv0) ->
    SessEv = lists:foldl(del_rule('PCC-Rules', _, _), SessEv0, Rules),
    lists:foldl(del_rule('PCC-Groups', _, _), SessEv, Bases);
rule_to_session(_, SessEv) ->
    SessEv.

to_session('Usage-Monitoring-Information', Value, SessEv) ->
    lists:foldl(fun umi_to_session/2, SessEv, Value);
to_session('Charging-Rule-Install', Value, SessEv) ->
    [lager:info("CRI: ~p", [lager:pr(R, ?MODULE)]) || R <- Value],
    lists:foldl(fun rule_to_session/2, SessEv, Value);
to_session('Charging-Rule-Remove', Value, SessEv) ->
    [lager:info("CRI: ~p", [lager:pr(R, ?MODULE)]) || R <- Value],
    lists:foldl(fun rule_to_session/2, SessEv, Value);
to_session(_, _, SessEv) ->
    SessEv.

%% make_request(Type, #{sid := SId, cc_req_number := ReqNum}) ->
%%     #{'Session-Id'          => SId,
%%       'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
%%       'CC-Request-Type'     => Type,
%%       'CC-Request-Number'   => ReqNum}.

make_CCR(Type, Session, Opts) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
		   'CC-Request-Type'     => Type},
    Avps = from_session(Session, Avps1),
    ['CCR' | Avps ].
