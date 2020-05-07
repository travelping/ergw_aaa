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

-module(ergw_aaa_ro).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/6, handle_response/6]).
-export([to_session/3, from_session/2]).

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
-include("include/diameter_3gpp_ts32_299_ro.hrl").

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(APP, 'Ro').
-define(DIAMETER_DICT_RO, diameter_3gpp_ts32_299_ro).
-define(DIAMETER_APP_ID_RO, ?DIAMETER_DICT_RO:id()).

-define(DefaultOptions, [{function, "undefined"},
			 {'Destination-Realm', undefined},
			 {'CC-Session-Failover', supported},
			 {'Credit-Control-Failure-Handling', terminate},
			 {answer_if_down, reject},
			 {answer_if_timeout, reject}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

-define(SI_PSI, 'Service-Information', 'PS-Information').

-record(state, {pending = undefined :: 'undefined' | reference(),
		state = init        :: atom(),
		request_number      :: 'undefined' | integer()
	       }).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{function := Function}) ->
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_RO,
	  'Vendor-Specific-Application-Id' =>
	      [#'diameter_base_Vendor-Specific-Application-Id'{
		  'Vendor-Id'           = ?VENDOR_ID_3GPP,
		  'Auth-Application-Id' = [?DIAMETER_APP_ID_RO]}],
	  application => [{alias, ?APP},
			  {answer_errors, callback},
			  {dictionary, ?DIAMETER_DICT_RO},
			  {module, ?MODULE}]},
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

invoke(_Service, {_, 'CCR-Initial'}, Session0, Events, Opts,
       #state{state = stopped} = State0) ->
    State = inc_request_number(State0#state{state = started}),
    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
    Session = maps:without(Keys, Session0),
    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_INITIAL_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, 'CCR-Update'}, Session, Events, Opts,
       #state{state = started} = State0) ->
    State = inc_request_number(State0),
    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_UPDATE_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, 'CCR-Terminate'}, Session, Events, Opts,
       #state{state = started} = State0) ->
    ?LOG(debug, "Session Stop: ~p", [Session]),
    State = inc_request_number(State0#state{state = stopped}),
    RecType = ?'DIAMETER_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST',
    Request = make_CCR(RecType, Session, Opts, State),
    await_response(send_request(Request, Opts), Session, Events, State, Opts);

invoke(_Service, {_, Procedure}, Session, Events, Opts,
       #state{state = SessState} = State)
  when Procedure =:= 'CCR-Update';
       Procedure =:= 'CCR-Terminate' ->
    case SessState of
	ocs_hold ->
	    handle_cca({error, ocs_hold_end}, Session, Events, Opts, State);
	peer_down ->
	    handle_cca({error, no_connection}, Session, Events, Opts, State);
	_ ->
	    {ok, Session, Events, State}
    end;

invoke(_Service, {_, Procedure}, Session, Events, _Opts, State)
  when Procedure =:= 'CCR-Initial';
       Procedure =:= 'CCR-Update';
       Procedure =:= 'CCR-Terminate' ->
    {ok, Session, Events, State};

invoke(Service, Procedure, Session, Events, _Opts, State) ->
    {{error, {Service, Procedure}}, Session, Events, State}.

%%%===================================================================
%%% ergw_aaa_diameter_srv wrapper
%%%===================================================================

%% TDB:
%% call(Request, Session, Events, Opts) ->
%%     CCFH = maps:with(['Credit-Control-Failure-Handling', 'CC-Session-Failover'], Session),
%%     ergw_aaa_diameter_srv:call(?APP, Request, Session, Events, maps:merge(Opts, CCFH)).

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
	      _Request, SvcName, Peer, CallOpts)
  when length(Errors) /= 0 ->
    ?LOG(error, "~p: decode of answer from ~p failed, errors ~p", [SvcName, Peer, Errors]),
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    Code =
	case Msg of
	    [_ | #{'Result-Code' := RC}] -> RC;	%% try to handle gracefully
	    _                            -> failed
	end,
    if Code >= 3000 ->
	    maybe_retry(Code, SvcName, Peer, CallOpts);
       true ->
	    {error, Code}
    end;

handle_answer(#diameter_packet{msg = [_ | #{'Result-Code' := Code}]},
	      _, SvcName, Peer, CallOpts)
  when Code >= 3000 ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    maybe_retry(Code, SvcName, Peer, CallOpts);

handle_answer(#diameter_packet{msg = Msg}, _Request, SvcName, Peer, _CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    Msg.

%% handle_error/5
handle_error(Reason, _Request, _SvcName, undefined, _CallOpts) ->
    Reason;
handle_error(Reason, _Request, SvcName, Peer, CallOpts) ->
    ok = ergw_aaa_diameter_srv:finish_request(SvcName, Peer),
    maybe_retry(Reason, SvcName, Peer, CallOpts).

maybe_retry(Reason, SvcName, Peer, CallOpts) ->
    ergw_aaa_diameter_srv:retry_request(Reason, SvcName, Peer, CallOpts).

%% maybe_retry(Reason, SvcName, Peer,
%% 	    #diam_call{opts = #{'CC-Session-Failover' := supported}} = CallOpts) ->
%%     ergw_aaa_diameter_srv:retry_request(Reason, SvcName, Peer, CallOpts);
%% maybe_retry(Reason, _SvcName, _Peer, _CallOpts) ->
%%     {error, Reason}.

handle_request(#diameter_packet{msg = [Command | Avps]}, _SvcName, Peer)
  when Command =:= 'ASR'; Command =:= 'RAR' ->
    handle_common_request(Command, Avps, Peer);
handle_request(_Packet, _SvcName, {_PeerRef, _Caps} = _Peer) ->
    ?LOG(error, "~p:handle_request(~p, ~p, ~p)",
		[?MODULE, _Packet, _SvcName, _Caps]),
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

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
validate_option('CC-Session-Failover', Value)
  when Value =:= supported;
       Value =:= not_supported ->
    Value;
validate_option('Credit-Control-Failure-Handling', Value)
  when Value =:= terminate;
       Value =:= continue;
       Value =:= retry_and_terminate ->
    Value;
validate_option(answers, Value) when is_map(Value) ->
    Value;
validate_option(diameter_flavour, Value) when is_atom(Value) ->
    Value;
validate_option(answer_if_down, Value) when is_atom(Value) ->
    Value;
validate_option(answer_if_timeout, Value) when is_atom(Value) ->
    Value;
validate_option(answer_if_rate_limit, Value) when is_atom(Value) ->
    Value;
validate_option(tx_timeout, Value) when is_integer(Value) ->
    Value;
validate_option(max_retries, Value) when is_integer(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

%% handle_cca/5
handle_cca(['CCA' | #{'Result-Code' := Code} = Avps],
	   Session0, Events0, _Opts, State)
  when Code < 3000 ->
    {Session, Events} = to_session({gy, 'CCA'}, {Session0, Events0}, Avps),
    {ok, Session, Events, State};
handle_cca([Answer | #{'Result-Code' := Code}], Session, Events, _Opts, State)
  when Answer =:= 'CCA'; Answer =:= 'answer-message' ->
    {{fail, Code}, Session, [stop | Events], State};
handle_cca({error, no_connection}, Session, Events,
	   #{answer_if_down := Answer, answers := Answers} = Opts, State0) ->
    {Avps, State} =
	apply_answer_config(Answer, Answers, State0#state{state = peer_down}),
    handle_cca(['CCA' | Avps], Session, Events, Opts, State);
handle_cca({error, timeout}, Session, Events,
	   #{answer_if_timeout := Answer, answers := Answers} = Opts, State0) ->
    {Avps, State} = apply_answer_config(Answer, Answers, State0),
    handle_cca(['CCA' | Avps], Session, Events, Opts, State);
handle_cca({error, rate_limit}, Session, Events,
	   #{answer_if_rate_limit := Answer, answers := Answers} = Opts, State0) ->
    {Avps, State} = apply_answer_config(Answer, Answers, State0),
    handle_cca(['CCA' | Avps], Session, Events, Opts, State);
handle_cca({error, _} = Result, Session, Events, _Opts, State) ->
    ?LOG(error, "CCA Result: ~p", [Result]),
    {Result, Session, [stop | Events], State}.

handle_common_request(Command, #{'Session-Id' := SessionId} = Avps, {_PeerRef, Caps}) ->
    {Result, ReplyAvps0} =
	case ergw_aaa_session_reg:lookup(SessionId) of
	    Session when is_pid(Session) ->
		ergw_aaa_session:request(Session, ?MODULE, {'gy', Command}, Avps);
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

filter_reply_avps(Command, Avps)
  when Command =:= 'ASR'; Command =:= 'RAR' ->
    Permited = ['User-Name'],
    maps:with(Permited, Avps);
filter_reply_avps(_, Avps) ->
    Avps.
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

from_session('Diameter-Session-Id', SId, Avps) ->
    Avps#{'Session-Id' => SId};

%% 'Node-Id'
from_session('Username', Value, Avps) when is_binary(Value) ->
    optional(['User-Name'], Value, Avps);

from_session('Termination-Cause', Cause, M) ->
    optional('Termination-Cause', Cause, M);

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

%%
%% some OCSs don't like this attribute on Gy, disable it for now
%%
%% 'QoS-Information'
%% from_session('QoS-Information' = Key, Value, Avps) ->
%%     optional([?SI_PSI, Key], ergw_aaa_diameter:qos_from_session(Value), Avps);

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

from_session(_Key, _Value, M) ->
    M.

% The custom 1 diameter flavour is for those who want IMSI in the 3GPP-IMSI in
% the 3GPP-IMSI AVP in the CCR root
from_session_custom1('3GPP-IMSI', IMSI, Avps) ->
    optional(['3GPP-IMSI'], IMSI, Avps);
from_session_custom1(Key, Value, Avps) ->
    from_session(Key, Value, Avps).

%% from_session/2
from_session(Session, Avps0) ->
    from_session_opts(Session, Avps0, #{}).

from_session_opts(Session, Avps0, Opts) ->
    Avps1 = optional([?SI_PSI, 'Charging-Characteristics-Selection-Mode'],
		     ?'DIAMETER_RO_CHARGING-CHARACTERISTICS-SELECTION-MODE_HOME-DEFAULT',
		     Avps0),
    Avps = dynamic_address_flag(Session, Avps1),
    Fun =
	case maps:get(diameter_flavour, Opts, standard) of
	    custom1 -> fun from_session_custom1/3;
	    _ -> fun from_session/3
	end,
    maps:fold(Fun, Avps, Session).

%% ------------------------------------------------------------------

apply_answer_config(Answer, Answers, State) ->
    apply_answer_config(maps:get(Answer, Answers, undefined), State).

apply_answer_config({ocs_hold, GCUs}, State) ->
    GCUs1 = lists:map(
	      fun (#{'Granted-Service-Unit' :=
			 [#{'CC-Time-Min' := [MinTime],
			    'CC-Time-Max' := [MaxTime]}] = [GSU]} = GCU) ->
		      GSU1 = GSU#{'CC-Time' =>
				      [MinTime + rand:uniform(MaxTime - MinTime)]},
		      GCU#{'Granted-Service-Unit' =>
			       [maps:without(['CC-Time-Min', 'CC-Time-Max'], GSU1)]};
		  (GCU) ->
		      GCU
	      end, GCUs),
    {#{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_SUCCESS',
       'Multiple-Services-Credit-Control' => GCUs1},
     State#state{state = ocs_hold}};
apply_answer_config(AVPs, State)
  when is_map(AVPs) ->
    {AVPs, State};
apply_answer_config(undefined, State) ->
    AVPs = #{'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED'},
    {AVPs, State}.

%% to_session/3
to_session(Procedure, SessEvs, Avps) ->
    maps:fold(to_session(Procedure, _, _, Avps, _), SessEvs, Avps).

%% to_session/4
to_session({_, 'CCA'}, 'CC-Session-Failover',
	   [?'DIAMETER_RO_CC-SESSION-FAILOVER_NOT_SUPPORTED'], _, {Session, Events}) ->
    {Session#{'CC-Session-Failover' => not_supported}, Events};
to_session({_, 'CCA'}, 'CC-Session-Failover',
	   [?'DIAMETER_RO_CC-SESSION-FAILOVER_SUPPORTED'], _, {Session, Events}) ->
    {Session#{'CC-Session-Failover' => supported}, Events};

to_session({_, 'CCA'}, 'Credit-Control-Failure-Handling',
	   [?'DIAMETER_RO_CREDIT-CONTROL-FAILURE-HANDLING_TERMINATE'], _, {Session, Events}) ->
    {Session#{'Credit-Control-Failure-Handling' => terminate}, Events};
to_session({_, 'CCA'}, 'Credit-Control-Failure-Handling',
	   [?'DIAMETER_RO_CREDIT-CONTROL-FAILURE-HANDLING_CONTINUE'], _, {Session, Events}) ->
    {Session#{'Credit-Control-Failure-Handling' => continue}, Events};
to_session({_, 'CCA'}, 'Credit-Control-Failure-Handling',
	   [?'DIAMETER_RO_CREDIT-CONTROL-FAILURE-HANDLING_RETRY_AND_TERMINATE'], _, {Session, Events}) ->
    {Session#{'Credit-Control-Failure-Handling' => retry_and_terminate}, Events};

to_session(_, 'Multiple-Services-Credit-Control', Value, AVPs, {Session, Events}) ->
    RC = maps:get('Result-Code', AVPs, ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'),
    MSCCmap = lists:foldl(
		fun(#{'Rating-Group' := [RG]} = G, M) -> M#{RG => G} end, #{}, Value),
    MSCC = maps:fold(
	     fun(RatingGroup, _, CC) ->
		     case maps:get(RatingGroup, MSCCmap, remove) of
			 remove ->
			     [#{'Rating-Group' => [RatingGroup],
				'Result-Code' =>
				    [?'DIAMETER_RO_RESULT-CODE_END_USER_SERVICE_DENIED']} | CC];
			 #{'Result-Code' := _} = V when RC >= 2000, RC < 3000 ->
			     [V | CC];
			 V ->
			     [V#{'Result-Code' => [RC]} | CC]
		     end
	     end, [], maps:get(credits, Session, #{})),
    {Session, [{update_credits, MSCC} | Events]};
to_session({_, 'RAR'}, 'Rating-Group', V, _, {Session, Events}) ->
    {Session, [Events | {report_rating_group, V}]};
to_session(_, _, _, _, SessEv) ->
    SessEv.

%% see 3GPP TS 32.299, Sect. 7.1.9 Multiple-Services-Credit-Control AVP
merge_mscc({Key, Value}, MSCC)
  when Key =:= 'Used-Service-Unit';
       Key =:= 'Service-Identifier';
       Key =:= 'G-S-U-Pool-Reference';
       Key =:= 'Reporting-Reason';
       Key =:= 'AF-Correlation-Information';
       Key =:= 'Envelope';
       Key =:= 'Service-Specific-Info';
       Key =:= 'Announcement-Information' ->
    maps:update_with(Key, [Value|_], [Value], MSCC);
merge_mscc({Key, Value}, MSCC) ->
    MSCC#{Key => Value}.

merge_mscc(RatingGroup, Values, Report) ->
    Init = #{'Rating-Group' => [RatingGroup]},
    Report#{RatingGroup =>
		lists:foldl(fun merge_mscc/2, maps:get(RatingGroup, Report, Init), Values)}.

request_credits(Session, MSCC) ->
    Credits = maps:get(credits, Session, #{}),
    maps:fold(
      fun(RatingGroup, empty, Request) ->
	      ?LOG(warning, "Ro Charging Key: ~p", [RatingGroup]),
	      RSU = [{'Requested-Service-Unit', #{}}],
	      merge_mscc(RatingGroup, RSU, Request);
	 (RatingGroup, _, Request) ->
	      ?LOG(error, "unknown Ro Rating Group: ~p", [RatingGroup]),
	      Request
      end, MSCC, Credits).

report_credits(Session, MSCC) ->
    Credits = maps:get(used_credits, Session, []),
    lists:foldl(
      fun({RatingGroup, Used}, Report) ->
	      RSU = [{'Used-Service-Unit', Used}],
	      merge_mscc(RatingGroup, RSU, Report)
      end, MSCC, Credits).

context_id(_Session) ->
    %% TODO: figure out what servive we are.....
    "14.32251@3gpp.org".

make_CCR(Type, Session, #{now := Now} = Opts, #state{request_number = ReqNumber}) ->
    Avps0 = maps:with(['Destination-Host', 'Destination-Realm'], Opts),
    Avps1 = Avps0#{'Auth-Application-Id' => ?DIAMETER_APP_ID_RO,
		   'CC-Request-Type'     => Type,
		   'CC-Request-Number'   => ReqNumber,
		   'Service-Context-Id'  => context_id(Session),
		   'Event-Timestamp' =>
		       [system_time_to_universal_time(Now + erlang:time_offset(), native)],
		   'Multiple-Services-Indicator' =>
		       [?'DIAMETER_RO_MULTIPLE-SERVICES-INDICATOR_SUPPORTED']},
    Avps2 = from_session_opts(Session, Avps1, Opts),
    MSCC = case Type of
	       ?'DIAMETER_RO_CC-REQUEST-TYPE_INITIAL_REQUEST' ->
		   request_credits(Session, #{});

	       ?'DIAMETER_RO_CC-REQUEST-TYPE_UPDATE_REQUEST' ->
		   MSCC0 = request_credits(Session, #{}),
		   report_credits(Session, MSCC0);

	       ?'DIAMETER_RO_CC-REQUEST-TYPE_TERMINATION_REQUEST' ->
		   report_credits(Session, #{})
	   end,
    Avps = Avps2#{'Multiple-Services-Credit-Control' => maps:values(MSCC)},
    ['CCR' | Avps ].
