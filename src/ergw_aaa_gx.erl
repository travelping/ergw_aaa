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

-define(DefaultOptions, [{transport, "undefined"}]).

-define(IS_IP(X), (is_tuple(X) andalso (tuple_size(X) == 4 orelse tuple_size(X) == 8))).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, #{transport := Transport}) ->
    SvcOpts =
	#{'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
	  'Vendor-Specific-Application-Id' =>
	      [#'diameter_base_Vendor-Specific-Application-Id'{
		  'Vendor-Id'           = ?VENDOR_ID_3GPP,
		  'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}],
	  application => [{alias, ?APP},
			  {dictionary, ?DIAMETER_DICT_GX},
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

invoke(_Service, authenticate, Session0, Events, Opts) ->
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	stopped ->
	    Session1 = ergw_aaa_session:set_svc_opt(
			 ?MODULE, DiamSession#{'State' => 'started'}, Session0),
	    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
	    Session = maps:without(Keys, inc_number(Session1)),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_INITIAL_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, authorize, #{'Authentication-Result' := success} = Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, authorize, Session, Events, _Opts) ->
    {denied, Session, Events};

invoke(_Service, start, Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, interim, Session0, Events, Opts) ->
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	started ->
	    Session = inc_number(Session0),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_UPDATE_REQUEST',
	    Request = make_CCR(RecType, Session, Opts),
	    handle_cca(call(Request, Opts), Session, Events);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, stop, Session0, Events, Opts) ->
    lager:debug("Session Stop: ~p", [Session0]),
    DiamSession = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', DiamSession, stopped) of
	started ->
	    Session1 = ergw_aaa_session:set_svc_opt(
			 ?MODULE, DiamSession#{'State' => 'stopped'}, Session0),
	    Session = inc_number(Session1),
	    RecType = ?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST',
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

prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]}, _, {PeerRef, Caps})
  when is_map(Avps) ->
    #diameter_caps{origin_host = {OH, DH},
		   origin_realm = {OR, DR},
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
		     'Destination-Host' => [DH],
		     'Destination-Realm' => DR,
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

handle_cca(['CCA' | #{'Result-Code' := [?'DIAMETER_BASE_RESULT-CODE_SUCCESS']} = Avps],
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

umi_session_init() ->
    [#{'Monitoring-Key'           => [<<"default">>],
       'Used-Service-Unit'        => [#{}],
       'Usage-Monitoring-Level'   => [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL']}].

umi_from_session(Key, Value, [#{'Used-Service-Unit' := [USU]} = UMI]) ->
    [UMI#{'Used-Service-Unit' := [USU#{session_to_usu_key(Key) => [Value]}]}].

from_session('Diameter-Session-Id', SId, M) ->
    M#{'Session-Id' => SId};

from_session('IP', Value, M) ->
    M#{'Framed-IP-Address' => [ip2bin(Value)]};
from_session('Framed-IP-Address', Value, M) ->
    M#{'Framed-IP-Address' => [ip2bin(Value)]};

%% '3GPP-Allocation-Retention-Priority'

from_session('3GPP-GGSN-Address', Value, M) ->
    M#{'3GPP-GGSN-Address' => [ip2bin(Value)]};

%% '3GPP-GPRS-Negotiated-QoS-Profile'

from_session('3GPP-IMSI', Value, M) ->
    Id = #{'Subscription-Id-Type' => ?'DIAMETER_GX_SUBSCRIPTION-ID-TYPE_END_USER_IMSI',
	   'Subscription-Id-Data' => Value},
    M#{'Subscription-Id' => [Id]};

%% '3GPP-IMSI-MCC-MNC'
%% '3GPP-NSAPI'

from_session('3GPP-RAT-Type', Value, M) ->
    M#{'3GPP-RAT-Type' => [<<Value>>]};

from_session('3GPP-SGSN-Address', Value, M) ->
    M#{'3GPP-SGSN-Address' => [ip2bin(Value)]};

from_session('Called-Station-Id', Value, M) ->
    M#{'Called-Station-Id' => [Value]};

%% 'Calling-Station-Id'
%% 'Framed-Protocol'
%% 'Interim-Accounting'
%% 'Multi-Session-Id'
%% 'Password'
%% 'Service-Type'
%% 'Session-Id'
%% 'Username'

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

from_session(Session, Avps) ->
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

make_CCR(Type, Session, _) ->
    Avps0 = #{'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
	      'CC-Request-Type'     => Type},
    Avps = from_session(Session, Avps0),
    ['CCR' | Avps ].
