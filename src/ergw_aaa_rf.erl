%% Copyright 2018,2019 Travelping GmbH <info@travelping.com>
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

-module(ergw_aaa_rf).

-behaviour(ergw_aaa).

-compile({parse_transform, cut}).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/6, handle_response/6]).
-export([to_session/3]).
-export([get_state_atom/1]).

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
-include("include/diameter_3gpp_ts32_299_rf.hrl").
-include("include/ergw_aaa_session.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, 'rf').
-define(DIAMETER_DICT_RF, diameter_3gpp_ts32_299_rf).
-define(DIAMETER_APP_ID_RF, ?DIAMETER_DICT_RF:id()).

-define(DefaultOptions, [{function, undefined},
			 {'Destination-Realm', undefined},
			 {termination_cause_mapping, #{}}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

-define(SI_PSI, 'Service-Information', 'PS-Information').

-record(state, {pending = undefined :: 'undefined' | reference(),
		state = init        :: atom(),
		record_number = 0   :: integer(),
		seq_number = 1      :: integer(),
		sdc = []            :: list(),
		tdv = []            :: list(),
		sec_rat_reports = [] :: list()
	       }).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{function := Function}) ->
    SvcOpts =
	#{'Acct-Application-Id' => ?DIAMETER_APP_ID_RF,
	  'Vendor-Specific-Application-Id' =>
	      [#'diameter_base_Vendor-Specific-Application-Id'{
		  'Vendor-Id'           = ?VENDOR_ID_3GPP,
		  'Acct-Application-Id' = [?DIAMETER_APP_ID_RF]}],
	  application => [{alias, ?APP},
			  {answer_errors, callback},
			  {dictionary, ?DIAMETER_DICT_RF},
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

invoke(_Service, {_, 'Initial'}, Session0, Events, Opts,
       #state{state = stopped} = State0) ->
    State1 = State0#state{state = started},
    Keys = ['service_data', traffic_data,
	    'RAN-Secondary-RAT-Usage-Report',
	    'InPackets', 'OutPackets',
	    'InOctets', 'OutOctets', 'Acct-Session-Time'],
    Session = maps:without(Keys, Session0),
    RecType = ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_START_RECORD',
    {Request, State} = create_ACR(RecType, Session, Opts, State1),
    ?LOG(debug, "Session Start: ~p", [Session]),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, 'Update'}, Session0, Events, Opts,
       #state{state = SessState} = State0) ->
    State1 = close_service_data_containers(Session0, State0),
    State2 = traffic_data_containers(Session0, State1),
    State3 = process_secondary_rat_usage_data_reports(Session0, State2),
    Session = maps:without([service_data, traffic_data,
			    'RAN-Secondary-RAT-Usage-Report'], Session0),
    case {SessState, maps:get(gy_event, Opts, interim)} of
	{started, cdr_closure} ->
	    RecType = ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD',
	    {Request, State} = create_ACR(RecType, Session, Opts, State3),
	    await_response(send_request(Request, Opts), Session, Events, State, Opts);
	_ ->
	    {ok, Session, Events, State3}
    end;

invoke(_Service, {_, 'Terminate'}, Session0, Events, Opts,
       #state{state = started} = State0) ->
    ?LOG(debug, "Session Stop: ~p", [Session0]),
    State1 = close_service_data_containers(Session0, State0#state{state = stopped}),
    State2 = traffic_data_containers(Session0, State1),
    State3 = process_secondary_rat_usage_data_reports(Session0, State2),
    Session = maps:without([service_data, traffic_data,
			    'RAN-Secondary-RAT-Usage-Report'], Session0),
    RecType = ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD',
    {Request, State} = create_ACR(RecType, Session, Opts, State3),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, terminate, Session0, Events, Opts,
       #state{state = started} = State0) ->
    ?LOG(debug, "Session Terminate: ~p", [Session0]),
    State1 = close_service_data_containers(Session0, State0#state{state = stopped}),
    State2 = traffic_data_containers(Session0, State1),
    State3 = process_secondary_rat_usage_data_reports(Session0, State2),
    Session = maps:without([service_data, traffic_data,
			    'RAN-Secondary-RAT-Usage-Report'], Session0),
    RecType = ?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD',
    {Request, State} = create_ACR(RecType, Session, Opts, State3),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, Procedure}, Session, Events, _Opts, State)
  when Procedure =:= 'Initial';
       Procedure =:= 'Update';
       Procedure =:= 'Terminate' ->
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
    handle_aca(Msg, Session, Events, Opts, State).

%% handle_response/6
handle_response(Promise, Msg, Session, Events, Opts, #state{pending = Promise} = State) ->
    handle_aca(Msg, Session, Events, Opts, State#state{pending = undefined});
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
prepare_request(#diameter_packet{msg = ['ACR' = T | Avps]} = Pkt0, SvcName,
		{_, Caps} = Peer, CallOpts)
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, _},
		   origin_realm = {OR, _},
		   origin_state_id = {OSid, _}} = Caps,
    Msg = [T | Avps#{'Origin-Host' => OH,
		     'Origin-Realm' => OR,
		     'Origin-State-Id' => OSid}],
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
validate_option(avp_filter, Value) when is_list(Value) ->
    Value;
validate_option(termination_cause_mapping, Value) ->
    ergw_aaa_diameter:validate_termination_cause_mapping(Value);
validate_option(Opt, Value) ->
    erlang:error(badarg, [Opt, Value]).

%%===================================================================
%% internal helpers
%%===================================================================

%% handle_aca/5
handle_aca(['ACA' | #{'Result-Code' := ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Avps],
	   Session0, Events0, _Opts, State) ->
    {Session, Events} = to_session({?APP, 'ACA'}, {Session0, Events0}, Avps),
    {ok, Session, Events, State};
handle_aca([Answer | #{'Result-Code' := Code}], Session, Events, _Opts, State)
  when Answer =:= 'ACA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, Events, State};
handle_aca({error, _} = Result, Session, Events, _Opts, State) ->
    {Result, Session, Events, State};

init_session_avp_defaults(#{'Acct-Interim-Interval' := Interim}, Avps)
  when not is_map_key('Acct-Interim-Interval', Avps),
       is_integer(Interim), Interim > 0 ->
    Avps#{'Acct-Interim-Interval' => [Interim]};
init_session_avp_defaults(_, Avps) ->
    Avps.

%% to_session/3
to_session(Procedure, {Session, _} = SessEvs, Avps0) ->
    Avps = init_session_avp_defaults(Session, Avps0),
    maps:fold(to_session(Procedure, _, _, _), SessEvs, Avps).

%% to_session/4
to_session(_, 'Acct-Interim-Interval', [Interim], {Session, Events})
  when is_integer(Interim), Interim > 0 ->
    Trigger = ergw_aaa_session:trigger(?APP, 'Offline', periodic, Interim),
    {Session, ergw_aaa_session:ev_set(Trigger, Events)};
to_session(_, _, _, SessEv) ->
    SessEv.

%%%===================================================================

-ifdef(OTP_RELEASE).
%% OTP 21 or higher
system_time_to_universal_time(Time, TimeUnit) ->
    calendar:system_time_to_universal_time(Time, TimeUnit).

-else.
%% from Erlang R21:

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).

system_time_to_universal_time(Time, TimeUnit) ->
    Secs = erlang:convert_time_unit(Time, TimeUnit, second),
    calendar:gregorian_seconds_to_datetime(Secs + ?SECONDS_FROM_0_TO_1970).
-endif.

assign([Key], Fun, Avps) when is_function(Fun) ->
    Fun(Key, Avps);
assign([Key], Value, Avps) when is_map(Avps) ->
    Avps#{Key => Value};
assign([Key | Next], FunOrValue, Avps) ->
    [V] = maps:get(Key, Avps, [#{}]),
    Avps#{Key => [assign(Next, FunOrValue, V)]}.

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

number_sdcs([], _) ->
    [];
number_sdcs([H|T], N) ->
    [H#{'Local-Sequence-Number' => N}|number_sdcs(T, N + 1)].

close_service_data_containers(#{service_data := SDC},
			      #state{seq_number = LocSeqNo,
				     sdc = ServiceDataCont} = State) ->
    State#state{
      seq_number = LocSeqNo + length(SDC),
      sdc = ServiceDataCont ++ number_sdcs(SDC, LocSeqNo)
     };
close_service_data_containers(_Session, State) ->
    State.

traffic_data_containers(#{traffic_data := TD}, #state{tdv = TrafficDataVolumes} = State)
  when is_list(TD) ->
    State#state{tdv = TrafficDataVolumes ++ TD};
traffic_data_containers(_Session, State) ->
    State.

process_secondary_rat_usage_data_reports(#{'RAN-Secondary-RAT-Usage-Report' := Reports},
					 #state{sec_rat_reports = SecRatRs} = State) ->
    State#state{sec_rat_reports = SecRatRs ++ Reports};
process_secondary_rat_usage_data_reports(_R, State) ->
    State.

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

%% 'InOctets'
accounting(Base, 'InOctets', Value, Avps) ->
    optional(Base ++ ['Accounting-Input-Octets'], Value, Avps);
%% 'OutOctets'
accounting(Base, 'OutOctets', Value, Avps) ->
    optional(Base ++ ['Accounting-Output-Octets'], Value, Avps).

%% Service-Data-Container

%%   [ AF-Correlation-Information ]
%%   [ Charging-Rule-Base-Name ]
%%   [ Accounting-Input-Octets ]
%%   [ Accounting-Output-Octets ]
%%   [ Local-Sequence-Number ]
%%   [ QoS-Information ]
%%   [ Rating-Group ]
%%   [ Change-Time ]
%%   [ Service-Identifier ]
%%   [ Service-Specific-Info ]
%%   [ ADC-Rule-Base-Name ]
%%   [ SGSN-Address ]
%%   [ Time-First-Usage ]
%%   [ Time-Last-Usage ]
%%   [ Time-Usage ]
%% * [ Change-Condition]
%%   [ 3GPP-User-Location-Info ]
%%   [ 3GPP2-BSID ]
%%   [ UWAN-User-Location-Info ]
%%   [ TWAN-User-Location-Info ]
%%   [ Sponsor-Identity ]
%%   [ Application-Service-Provider-Identity ]
%% * [ Presence-Reporting-Area-Information]
%%   [ Presence-Reporting-Area-Status ]
%%   [ User-CSG-Information ]
%%   [ 3GPP-RAT-Type ]
%%   [ Related-Change-Condition-Information ]
%%   [ Serving-PLMN-Rate-Control ]
%%   [ APN-Rate-Control ]
%%   [ 3GPP-PS-Data-Off-Status ]
%%   [ Traffic-Steering-Policy-Identifier-DL ]
%%   [ Traffic-Steering-Policy-Identifier-UL ]


service_data(Avps0, #state{sdc = SDC} = State) when length(SDC) /= 0 ->
    Avps = assign([?SI_PSI, 'Service-Data-Container'], SDC, Avps0),
    {Avps, State#state{sdc = []}};
service_data(Avps, State) ->
    {Avps, State}.

traffic_data(Avps0, #state{tdv = TDV} = State) when length(TDV) /= 0 ->
    Avps = assign([?SI_PSI, 'Traffic-Data-Volumes'], TDV, Avps0),
    {Avps, State#state{tdv = []}};
traffic_data(Avps, State) ->
    {Avps, State}.

ran_secondary_rat_usage_report(Avps0, #state{sec_rat_reports = Reports} = State)
  when length(Reports) /= 0 ->
    Avps = assign([?SI_PSI, 'RAN-Secondary-RAT-Usage-Report'], Reports, Avps0),
    {Avps, State#state{sec_rat_reports = []}};
ran_secondary_rat_usage_report(Avps, State) ->
    {Avps, State}.

monitors_from_session('IP-CAN', #{?MODULE := Monitor}, Avps) ->
    maps:fold(fun from_session/3, Avps, Monitor);
monitors_from_session(_, _, Avps) ->
    Avps.

from_session('Diameter-Session-Id', SId, M) ->
    M#{'Session-Id' => SId};

%% '3GPP-Charging-Id'
%% '3GPP-PDP-Type'
%% '3GPP-IMSI-MCC-MNC'
%% '3GPP-GGSN-MCC-MNC'
%% '3GPP-NSAPI'
%% '3GPP-Session-Stop-Indicator %% '3GPP-Selection-Mode' - handled elsewhere
%% '3GPP-Charging-Characteristics'
%% '3GPP-SGSN-MCC-MNC'
%% '3GPP-MS-TimeZone'
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
       Key =:= '3GPP-RAT-Type' ->
    optional([?SI_PSI, Key], ergw_aaa_diameter:'3gpp_from_session'(Key, Value), Avps);

%% '3GPP-User-Location-Info'
from_session(Key, Value, Avps)
  when Key =:= 'User-Location-Info';
       Key =:= '3GPP-User-Location-Info' ->
    optional([?SI_PSI, '3GPP-User-Location-Info'],
	     ergw_aaa_diameter:'3gpp_from_session'(Key, Value), Avps);

from_session('3GPP-IMSI', IMSI, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_RF_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	   'Subscription-Id-Data' => IMSI},
    repeated(['Service-Information', 'Subscription-Id'], SI, Avps);
from_session('3GPP-MSISDN', MSISDN, Avps) ->
    SI = #{'Subscription-Id-Type' => ?'DIAMETER_RF_SUBSCRIPTION-ID-TYPE_END_USER_E164',
	   'Subscription-Id-Data' => MSISDN},
    repeated(['Service-Information', 'Subscription-Id'], SI, Avps);

%% '3GPP-Charging-Id', 'PDN-Connection-Charging-ID'
from_session('3GPP-Charging-Id' = Key, Value, Avps0) ->
    Avps1 = optional([?SI_PSI, Key], ergw_aaa_diameter:'3gpp_from_session'(Key, Value), Avps0),
    optional([?SI_PSI, 'PDN-Connection-Charging-ID'], Value, Avps1);

%% 'Node-Id'
from_session('Node-Id' = Key, Value, Avps) when is_binary(Value) ->
    optional([?SI_PSI, Key], Value, Avps);

from_session('Framed-IP-Address', IP, Avps) ->
    repeated([?SI_PSI, 'PDP-Address'], IP, Avps);
from_session('Framed-IPv6-Prefix', {IP, PrefixLen}, Avps0) ->
    Avps = repeated([?SI_PSI, 'PDP-Address'], IP, Avps0),
    optional([?SI_PSI, 'PDP-Address-Prefix-Length'], PrefixLen, Avps);

%% 'QoS-Information'
from_session('QoS-Information' = Key, Value, Avps) ->
    optional([?SI_PSI, Key], ergw_aaa_diameter:qos_from_session(Value), Avps);

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
    optional([?SI_PSI, Key], ?'DIAMETER_RF_PDP-CONTEXT-TYPE_PRIMARY', Avps);
from_session('PDP-Context-Type' = Key, secondary, Avps) ->
    optional([?SI_PSI, Key], ?'DIAMETER_RF_PDP-CONTEXT-TYPE_SECONDARY', Avps);

%% 'Offline-Charging'
%% 'Service-Data-Container'
%% 'User-Equipment-Info'

from_session('3GPP-IMEISV', IMEI, Avps) ->
    UE = #{'User-Equipment-Info-Type' =>
	       ?'DIAMETER_RF_USER-EQUIPMENT-INFO-TYPE_IMEISV',
	   'User-Equipment-Info-Value' => IMEI},
    optional([?SI_PSI, 'User-Equipment-Info'], UE, Avps);

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
%% 'OutOctets'
from_session(Key, Value, Avps)
  when Key =:= 'InOctets'; Key =:= 'OutOctets' ->
    accounting([?SI_PSI, 'Traffic-Data-Volumes'], Key, Value, Avps);

from_session('Acct-Interim-Interval' = Key, Value, Avps) ->
    optional(Key, Value, Avps);

%% Only Session level monitoring for now
from_session(monitors, Monitors, Avps) ->
    maps:fold(fun monitors_from_session/3, Avps, Monitors);

from_session(_Key, _Value, M) ->
    M.

from_session(Session, Avps0) ->
    Avps = dynamic_address_flag(Session, Avps0),
    maps:fold(fun from_session/3, Avps, Session).

context_id(_Session) ->
    %% TODO: figure out what servive we are.....
    "14.32251@3gpp.org".

stop_indicator(?'DIAMETER_RF_ACCOUNTING-RECORD-TYPE_STOP_RECORD', Avps) ->
    optional([?SI_PSI, '3GPP-Session-Stop-Indicator'],
	     ergw_aaa_diameter:'3gpp_from_session'('3GPP-Session-Stop-Indicator', true), Avps);
stop_indicator(_Type, Avps) ->
    Avps.

create_ACR(Type, Session, #{now := Now} = Opts, #state{record_number = RecNumber} = State0) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Accounting-Record-Type' => Type,
		   'Accounting-Record-Number' => RecNumber,
		   'Acct-Application-Id'      => ?DIAMETER_APP_ID_RF,
		   'Service-Context-Id'       => [context_id(Session)],
		   'Event-Timestamp' =>
		       [system_time_to_universal_time(Now + erlang:time_offset(), native)],
		   'Service-Information'      =>
		       [#{'Subscription-Id' => [],
			  'PS-Information'  =>
			      [#{'PDP-Context-Type' => 0}]}]
		  },
    Avps2 = stop_indicator(Type, Avps1),
    {Avps3, State1} = service_data(Avps2, State0),
    {Avps4, State2} = traffic_data(Avps3, State1),
    {Avps, State} = ran_secondary_rat_usage_report(Avps4, State2),
    {['ACR' | from_session(Session, Avps)], State#state{record_number = RecNumber + 1}}.

get_state_atom(#state{state = State}) ->
    State.
