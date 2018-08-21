%% Copyright 2018, Travelping GmbH <info@travelping.com>
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

-module(ergw_aaa_ro).

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
-include("include/diameter_3gpp_ts32_299_ro.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, 'Ro').
-define(DIAMETER_DICT_RO, diameter_3gpp_ts32_299_ro).
-define(DIAMETER_APP_ID_RO, ?DIAMETER_DICT_RO:id()).

-define(DefaultOptions, [{transport, "undefined"}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

-define(SI_PSI, 'Service-Information', 'PS-Information').

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{transport := Transport}) ->
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_RO,
	  'Vendor-Specific-Application-Id' =>
	      [#'diameter_base_Vendor-Specific-Application-Id'{
		  'Vendor-Id'           = ?VENDOR_ID_3GPP,
		  'Auth-Application-Id' = [?DIAMETER_APP_ID_RO]}],
	  application => [{alias, ?APP},
			  {dictionary, ?DIAMETER_DICT_RO},
			  {module, ?MODULE}]},
    ergw_aaa_diameter_srv:register_service(Transport, SvcOpts),
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
	    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, {_, 'CCR-Update'}, Session0, Events, Opts) ->
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	started ->
	    Session = inc_number(Session0),
	    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_UPDATE_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events);
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
	    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(Service, Procedure, Session, Events, _Opts) ->
    {{error, {Service, Procedure}}, Session, Events}.

call(Request, #{transport := Transport}) ->
    diameter:call(Transport, ?APP, Request, []).

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

prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]}, _, {_PeerRef, Caps})
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, DH},
		   origin_realm = {OR, DR},
		   origin_state_id = {OSid, _}} = Caps,

    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Origin-State-Id' => OSid,
		     'Destination-Host' => [DH],
		     'Destination-Realm' => DR}],
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

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(transport, Value) when is_atom(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

handle_cca(['CCA' | #{'Result-Code' := ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Avps],
	   Session0, Events0) ->
    {Session, Events} = maps:fold(fun to_session/3, {Session0, Events0}, Avps),
    {ok, Session, Events};
handle_cca([Answer | #{'Result-Code' := Code}], Session, Events)
  when Answer =:= 'CCA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, Events};
handle_cca({error, _} = Result, Session, Events) ->
    {Result, Session, Events}.

inc_number(Session) ->
    ModuleOpts = maps:get(?MODULE, Session, #{}),
    Number = maps:get('CC-Request-Number', ModuleOpts, -1),
    Session#{?MODULE => ModuleOpts#{'CC-Request-Number' => Number + 1}}.

%%%===================================================================

%% from Erlang R21:

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).

system_time_to_universal_time(Time, TimeUnit) ->
    Secs = erlang:convert_time_unit(Time, TimeUnit, second),
    calendar:gregorian_seconds_to_datetime(Secs + ?SECONDS_FROM_0_TO_1970).

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

optional(Keys, Value, Avps) when is_list(Keys) ->
    assign(Keys, optional(_, Value, _), Avps);
optional(Key, Value, Avps)
  when is_atom(Key) ->
    Avps#{Key => [Value]}.

%%%===================================================================

from_service('CC-Request-Number' = Key, Value, M) ->
    M#{Key => Value};
from_service(_, _, M) ->
    M.

%% ------------------------------------------------------------------

dynamic_address_flag(Key, {0,0,0,0}, Avps) ->
    optional(Key, 1, Avps);
dynamic_address_flag(Key, {{0,0,0,0,0,0,0,0},_}, Avps) ->
    optional(Key, 1, Avps);
dynamic_address_flag(_Key, _, Avps) ->
    Avps.

dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv4v6',
		       'Requested-IP-Address' := IP4,
		       'Requested-IPv6-Prefix' := IP6}, Avps0) ->
    Avps = dynamic_address_flag([?SI_PSI, 'Dynamic-Address-Flag-Extension'], IP4, Avps0),
    dynamic_address_flag([?SI_PSI, 'Dynamic-Address-Flag'], IP6, Avps);
dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv4',
		       'Requested-IP-Address' := IP4}, Avps) ->
    dynamic_address_flag([?SI_PSI, 'Dynamic-Address-Flag'], IP4, Avps);
dynamic_address_flag(#{'3GPP-PDP-Type' := 'IPv6',
		       'Requested-IPv6-Prefix' := IP6}, Avps) ->
    dynamic_address_flag([?SI_PSI, 'Dynamic-Address-Flag'], IP6, Avps);
dynamic_address_flag(_Session, Avps) ->
    Avps.

from_session('Event-Timestamp' = Key, Value, M) ->
    optional(Key, system_time_to_universal_time(Value, second), M);

from_session('Diameter-Session-Id', SId, M) ->
    M#{'Session-Id' => SId};

%% 'Node-Id'
from_session('Username', Value, Avps) when is_binary(Value) ->
    optional(['User-Name'], Value, Avps);

%% '3GPP-Charging-Id', 'PDN-Connection-Charging-ID'
from_session('3GPP-Charging-Id' = Key, Value, Avps0) ->
    Avps1 = optional([?SI_PSI, Key], ergw_aaa_diameter:'3gpp_from_session'(Key, Value), Avps0),
    optional([?SI_PSI, 'PDN-Connection-Charging-ID'], Value, Avps1);

%% '3GPP-PDP-Type'
%% '3GPP-IMSI-MCC-MNC'
%% '3GPP-GGSN-MCC-MNC'
%% '3GPP-NSAPI'
%% '3GPP-Session-Stop-Indicator %% '3GPP-Selection-Mode' - handled elsewhere
%% '3GPP-Charging-Characteristics'
%% '3GPP-SGSN-MCC-MNC'
%% '3GPP-MS-TimeZone'
%% '3GPP-User-Location-Info'
%% '3GPP-RAT-Type'
from_session(Key, Value, Avps)
  when Key =:= '3GPP-PDP-Type';
       Key =:= '3GPP-IMSI-MCC-MNC';
       Key =:= '3GPP-GGSN-MCC-MNC';
       Key =:= '3GPP-NSAPI';
       Key =:= '3GPP-Selection-Mode';
       Key =:= '3GPP-Charging-Characteristics';
       Key =:= '3GPP-SGSN-MCC-MNC';
       Key =:= '3GPP-MS-TimeZone';
       Key =:= '3GPP-User-Location-Info';
       Key =:= '3GPP-RAT-Type' ->
    optional([?SI_PSI, Key], ergw_aaa_diameter:'3gpp_from_session'(Key, Value), Avps);

from_session('3GPP-IMSI', IMSI, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_RO_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	   'Subscription-Id-Data' => IMSI},
    repeated(['Subscription-Id'], SI, Avps);
from_session('3GPP-MSISDN', MSISDN, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_RO_SUBSCRIPTION-ID-TYPE_END_USER_E164',
	   'Subscription-Id-Data' => MSISDN},
    repeated(['Subscription-Id'], SI, Avps);

%% 'PDN-Connection-Charging-ID'

%% 'Node-Id'
from_session('Node-Id' = Key, Value, Avps) when is_binary(Value) ->
    optional([?SI_PSI, Key], Value, Avps);

from_session('Framed-IP-Address', IP, Avps) ->
    repeated([?SI_PSI, 'PDP-Address'], IP, Avps);
from_session('Framed-IPv6-Prefix', {IP, PrefixLen}, Avps0) ->
    Avps = repeated([?SI_PSI, 'PDP-Address'], IP, Avps0),
    optional([?SI_PSI, 'PDP-Address-Prefix-Length'], PrefixLen, Avps);

%% 'Dynamic-Address-Flag'
%% 'Dynamic-Address-Flag-Extension'
%% 'QoS-Information'

%% 'SGSN-Address'
from_session(Key, IP, Avps)
  when Key =:= '3GPP-SGSN-Address';
       Key =:= '3GPP-SGSN-IPv6-Address' ->
    repeated([?SI_PSI, 'SGSN-Address'], IP, Avps);

%% 'GGSN-Address'
from_session(Key, IP, Avps)
  when Key =:= '3GPP-GGSN-Address';
       Key =:= '3GPP-GGSN-IPv6-Address' ->
    repeated([?SI_PSI, 'GGSN-Address'], IP, Avps);

%% 'TDF-IP-Address'
%% 'SGW-Address'
%% 'ePDG-Address'
%% 'TWAG-Address'

%% 'CG-Address'
from_session(Key, IP, Avps)
  when Key =:= '3GPP-Charging-Gateway-Address';
       Key =:= '3GPP-Charging-Gateway-IPv6-Address' ->
    optional([?SI_PSI, 'CG-Address'], IP, Avps);

%% 'Serving-Node-Type'
%% 'SGW-Change'

%% 'IMSI-Unauthenticated-Flag'

%% 'Called-Station-Id'
from_session('Called-Station-Id' = Key, Value, Avps) ->
    optional([?SI_PSI, Key], Value, Avps);

%% 'Charging-Characteristics-Selection-Mode'

%% 'Charging-Rule-Base-Name'
from_session('Charging-Rule-Base-Name' = Key, Value, Avps) when is_binary(Value) ->
    optional([?SI_PSI, Key], Value, Avps);

%% 'ADC-Rule-Base-Name'
%% 'User-Location-Info-Time'
%% 'User-CSG-Information'
%% 'Presence-Reporting-Area-Information'
%% '3GPP2-BSID'
%% 'TWAN-User-Location-Info'
%% 'UWAN-User-Location-Info'

%% 'PS-Furnish-Charging-Information'

%% 'PDP-Context-Type'
from_session('PDP-Context-Type' = Key, primary, Avps) ->
    optional([?SI_PSI, Key], ?'DIAMETER_RO_PDP-CONTEXT-TYPE_PRIMARY', Avps);
from_session('PDP-Context-Type' = Key, secondary, Avps) ->
    optional([?SI_PSI, Key], ?'DIAMETER_RO_PDP-CONTEXT-TYPE_SECONDARY', Avps);

%% 'Offline-Charging'
%% 'Service-Data-Container'
%% 'User-Equipment-Info'

from_session('3GPP-IMEISV', IMEI, Avps) ->
    UE = #{'User-Equipment-Info-Type' =>
	       ?'DIAMETER_RO_USER-EQUIPMENT-INFO-TYPE_IMEISV',
	   'User-Equipment-Info-Value' => IMEI},
    optional(['User-Equipment-Info'], UE, Avps);

%% 'Terminal-Information'

%% 'Start-Time'
from_session('Accounting-Start', Value, Avps) ->
    SysTime = Value + erlang:time_offset(),
    optional([?SI_PSI, 'Start-Time'],
	     system_time_to_universal_time(SysTime, native), Avps);

%% 'Stop-Time'
from_session('Accounting-Stop', Value, Avps) ->
    SysTime = Value + erlang:time_offset(),
    optional([?SI_PSI, 'Stop-Time'],
	     system_time_to_universal_time(SysTime, native), Avps);

%% 'Change-Condition'
%% 'Diagnostics'
%% 'Low-Priority-Indicator'
%% 'NBIFOM-Mode'
%% 'NBIFOM-Support'
%% 'MME-Number-for-MT-SMS'
%% 'MME-Name'
%% 'MME-Realm'
%% 'Logical-Access-ID'
%% 'Physical-Access-ID'
%% 'Fixed-User-Location-Info'
%% 'CN-Operator-Selection-Entity'
%% 'Enhanced-Diagnostics'
%% 'SGi-PtP-Tunnelling-Method'
%% 'CP-CIoT-EPS-Optimisation-Indicator'
%% 'UNI-PDU-CP-Only-Flag'
%% 'Serving-PLMN-Rate-Control'
%% 'APN-Rate-Control'
%% 'Charging-Per-IP-CAN-Session-Indicator'
%% 'RRC-Cause-Counter'
%% '3GPP-PS-Data-Off-Status'
%% 'SCS-AS-Address'
%% 'Unused-Quota-Timer'

%% 'Traffic-Data-Volumes' ========================

%% 'InOctets'
from_session('InOctets', Value, Avps) ->
    optional([?SI_PSI, 'Traffic-Data-Volumes', 'Accounting-Input-Octets'],
	     Value, Avps);
%% 'OutOctets'
from_session('OutOctets', Value, Avps) ->
    optional([?SI_PSI, 'Traffic-Data-Volumes', 'Accounting-Output-Octets'],
	     Value, Avps);

from_session(?MODULE, Value, M) ->
    maps:fold(fun from_service/3, M, Value);
from_session(_Key, _Value, M) ->
    M.

from_session(Session, Avps0) ->
    Avps1 = optional([?SI_PSI, 'Charging-Characteristics-Selection-Mode'],
		     ?'DIAMETER_RO_CHARGING-CHARACTERISTICS-SELECTION-MODE_HOME-DEFAULT',
		     Avps0),
    Avps = dynamic_address_flag(Session, Avps1),
    maps:fold(fun from_session/3, Avps, Session).

%% ------------------------------------------------------------------

to_session('Multiple-Services-Credit-Control' = K, V, {Session, Events}) ->
    {Session#{K => V}, [{update_credits, V} | Events]};
to_session(_, _, SessEv) ->
    SessEv.

attr_merge(Key, Value, Map) ->
    maps:update_with(Key,  maps:merge(_, Value), Value, Map).

request_credits(Session, MSCC) ->
    Credits = maps:get(credits, Session, #{}),
    maps:fold(
      fun(RatingGroup, empty, Request) ->
	      lager:warning("Ro Charging Key: ~p", [RatingGroup]),
	      Req = #{'Rating-Group' => [RatingGroup],
		      'Requested-Service-Unit' => [#{}]},
	      attr_merge(RatingGroup, Req, Request);
	 (RatingGroup, _, Request) ->
	      lager:error("unknown Ro Rating Group: ~p", [RatingGroup]),
	      Request
      end, MSCC, Credits).

report_credits(Session, MSCC) ->
    Credits = maps:get(used_credits, Session, #{}),
    maps:fold(
      fun(RatingGroup, Used, Report) ->
	      attr_merge(RatingGroup, #{'Rating-Group' => [RatingGroup],
					'Used-Service-Unit' => [Used]}, Report)
      end, MSCC, Credits).

context_id(_Session) ->
    %% TODO: figure out what servive we are.....
    "14.32251@3gpp.org".

make_CCR(Type, Session, _) ->
    Avps0 = #{'Auth-Application-Id' => ?DIAMETER_APP_ID_RO,
	      'CC-Request-Type'     => Type,
	      'Service-Context-Id'  => context_id(Session),
	      'Multiple-Services-Indicator' =>
		  [?'DIAMETER_RO_MULTIPLE-SERVICES-INDICATOR_SUPPORTED']},
    Avps1 = from_session(Session, Avps0),
    MSCC = case Type of
	       ?'DIAMETER_RO_CC-REQUEST-TYPE_INITIAL_REQUEST' ->
		   request_credits(Session, #{});

	       ?'DIAMETER_RO_CC-REQUEST-TYPE_UPDATE_REQUEST' ->
		   MSCC0 = request_credits(Session, #{}),
		   report_credits(Session, MSCC0);

	       ?'DIAMETER_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST' ->
		   report_credits(Session, #{})
	   end,
    Avps = Avps1#{'Multiple-Services-Credit-Control' => maps:values(MSCC)},
    ['CCR' | Avps ].
