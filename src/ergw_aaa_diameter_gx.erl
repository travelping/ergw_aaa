%% Copyright 2017, Travelping GmbH <info@travelping.com>
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

-module(ergw_aaa_diameter_gx).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_options/1, initialize_provider/1,
         init/1, authorize/3, start_authentication/3, start_accounting/4]).

%% DIAMETER API
-export([handle_answer/5, handle_error/5]).

-import(ergw_aaa_session, [attr_get/2, attr_get/3, attr_set/3,
			   attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/diameter_3gpp_ts29_212.hrl").
-include("include/ergw_aaa_gx.hrl").

-include("include/ergw_aaa_profile.hrl").
-include("include/ergw_aaa_variable.hrl").

-define(SERVER, ?MODULE).

-define(DIAMETER_APP_ID_GX, diameter_3gpp_ts29_212:id()).

-define(Start, 2).
-define(Interim, 3).
-define(Stop, 4).

-record(state, {sid    :: binary(), % diameter Session-Id
                auth_state,
		cc_req_number,
		diameter_session = []}).

%%===================================================================
%% API
%%===================================================================

initialize_provider(_Opts) ->
    {ok, []}.

validate_options(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, []).

init(_Opts) ->
    State = #state{
	       sid = diameter:session_id("erGW-AAA"),
	       cc_req_number = 0
    },
    {ok, State}.

start_authentication(From, Session, State0) ->
    {Avps0, State1} = make_request(?'DIAMETER_GX_CC-REQUEST-TYPE_INITIAL_REQUEST', State0),
    Avps = from_session(Session, Avps0),

    CCR = ['CCR' | to_list(Avps) ],
    lager:debug("CCR: ~p", [CCR]),

    State = State1#state{auth_state = pending},
    call(CCR, From, State),
    {ok, State}.

authorize(_From, _Session, State = #state{auth_state = Verdict}) ->
    {reply, Verdict, to_session([]), State}.

start_accounting(_From, 'Start', _Session, State) ->
    {ok, State};

start_accounting(From, 'Interim', Session, State0) ->
    {Avps0, State} = make_request(?'DIAMETER_GX_CC-REQUEST-TYPE_UPDATE_REQUEST', State0),
    Avps = from_session(Session, Avps0),

    CCR = ['CCR' | to_list(Avps) ],
    lager:debug("CCR: ~p", [CCR]),

    call(CCR, From, State),
    {ok, State};

start_accounting(From, 'Stop', Session, State0) ->
    {Avps0, State} = make_request(?'DIAMETER_GX_CC-REQUEST-TYPE_TERMINATION_REQUEST', State0),
    Avps = from_session(Session, Avps0),

    CCR = ['CCR' | to_list(Avps) ],
    lager:debug("CCR: ~p", [CCR]),

    call(CCR, From, State),
    {ok, State}.

%%===================================================================
%% Helper
%%===================================================================

call(Request, From, State) ->
    ergw_aaa_gx:call(Request, ?MODULE, [{From, State}]).

handle_answer(P = #diameter_packet{msg = Msg}, _Request, _SvcName, {_PeerRef, _}, {From, State})
  when is_record(Msg, diameter_gx_CCA), is_pid(From) ->
    lager:debug("handle_answer Packet: ~p", [lager:pr(P, ?MODULE)]),
    #diameter_gx_CCA{'Result-Code' = Code} = Msg,
    if Code =/= ?'DIAMETER_BASE_RESULT-CODE_SUCCESS' ->
	    List = tl(diameter_3gpp_ts29_212:'#get-'(Msg)),
	    %% lager:debug("List: ~p", [List]),
	    Verdict = success,
	    SessionOpts = lists:foldl(fun to_session/2, #{}, List);
       true ->
	    Verdict = deny,
	    SessionOpts = #{}
    end,
    AuthReply = {Verdict, SessionOpts, State#state{auth_state = Verdict}},
    ?queue_event(From, {'AuthenticationRequestReply', AuthReply}),
    ok;

handle_answer(#diameter_packet{msg = Msg0}, _Request, _SvcName, {_PeerRef, _}, {From, State})
  when element(1, Msg0) =:= 'diameter_base_answer-message', is_pid(From) ->
    Msg = try to_map(Msg0, 'diameter_gen_base_rfc6733')
	  catch
	      error:badarg ->
		  to_map(Msg0, 'diameter_gen_base_rfc3588')
	  end,
    lager:warning("DIAMETER request failed with ~p", [Msg]),
    AuthReply = {fail, #{}, State#state{auth_state = fail}},
    ?queue_event(From, {'AuthenticationRequestReply', AuthReply}),
    ok.

handle_error(Reason, _Request, _SvcName, _Peer, {From, State}) ->
    lager:error("Diam Gx: ~p Pid ~p", [Reason, From]),
    Verdict = failed,
    AuthReply = {Verdict, #{}, State#state{auth_state = Verdict}},
    ?queue_event(From, {'AuthenticationRequestReply', AuthReply}),
    ok.

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

make_request(Type, #state{sid = SId, cc_req_number = ReqNum} = State0) ->
    Avps = #{'Session-Id'          => SId,
	     'Auth-Application-Id' => ?DIAMETER_APP_ID_GX,
	     'CC-Request-Type'     => Type,
	     'CC-Request-Number'   => ReqNum},
    State = State0#state{cc_req_number = ReqNum + 1},
    {Avps, State}.

ip2bin({A,B,C,D}) ->
    <<A,B,C,D>>;
ip2bin(Bin) when is_binary(Bin)->
    Bin.

map_update_with(Key, Fun, Init, Map) ->
    V = maps:get(Key, Map, Init),
    Map#{Key => Fun(V)}.

session_to_usu_key('InOctets')  -> 'CC-Input-Octets';
session_to_usu_key('OutOctets') -> 'CC-Output-Octets'.

umi_session_init() ->
    [#{'Monitoring-Key'           => [<<"default">>],
       'Used-Service-Unit'        => [#{}],
       'Usage-Monitoring-Level'   => [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL']}].

umi_from_session(Key, Value, [#{'Used-Service-Unit' := [USU]} = UMI]) ->
    [UMI#{'Used-Service-Unit' := [USU#{session_to_usu_key(Key) => [Value]}]}].

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

from_session(_Key, _Value, M) ->
    M.

from_session(Session, Avps) ->
    maps:fold(fun from_session/3, Avps, Session).

%% {'diameter_gx_Granted-Service-Unit',[],[600],[],[],[],[],[],[]},
%% {'diameter_gx_Granted-Service-Unit',[],[],[],[1000],[1000],[1000],[],[]}],

gsu_to_session(Key, #'diameter_gx_Granted-Service-Unit'{
		       'CC-Time' = [Interval]}, Session) ->
    Session#{
      'Monitoring-Key'     => Key,
      'Interim-Accounting' => Interval * 1000
     };
gsu_to_session(_, _GSU, Session) ->
    Session.

umi_to_session(#'diameter_gx_Usage-Monitoring-Information'{
		  'Monitoring-Key' = Key,
		  'Granted-Service-Unit' = GSU,
		  'Usage-Monitoring-Level' =
		      [?'DIAMETER_GX_USAGE-MONITORING-LEVEL_SESSION_LEVEL']
		 }, Session) ->
    lists:foldl(fun(G, S) -> gsu_to_session(Key, G, S) end, Session, GSU);
umi_to_session(_, Session) ->
    Session.

add_rule(Key, Rule, Session) ->
    maps:update_with(Key, fun(V) -> [Rule|V] end, [Rule], Session).

del_rule(Key, Rule, Session) ->
    maps:update_with(Key, fun(V) -> lists:delete(Rule, V) end, [], Session).

rule_to_session(#'diameter_gx_Charging-Rule-Install'{
		   'Charging-Rule-Base-Name' = Bases,
		   'Charging-Rule-Name' = Rules}, Session0) ->
    Session = lists:foldl(add_rule('PCC-Rules', _, _), Session0, Rules),
    lists:foldl(add_rule('PCC-Groups', _, _), Session, Bases);
rule_to_session(#'diameter_gx_Charging-Rule-Remove'{
		   'Charging-Rule-Base-Name' = Bases,
		   'Charging-Rule-Name' = Rules}, Session0) ->
    Session = lists:foldl(del_rule('PCC-Rules', _, _), Session0, Rules),
    lists:foldl(del_rule('PCC-Groups', _, _), Session, Bases);
rule_to_session(_, Session) ->
    Session.

%% to_session({K, V}, Session) ->
%%     Session#{K => V};
to_session({'Usage-Monitoring-Information', Value}, Session) ->
    lists:foldl(fun umi_to_session/2, Session, Value);
to_session({'Charging-Rule-Install', Value}, Session) ->
    [lager:info("CRI: ~p", [lager:pr(R, ?MODULE)]) || R <- Value],
    lists:foldl(fun rule_to_session/2, Session, Value);
to_session({'Charging-Rule-Remove', Value}, Session) ->
    [lager:info("CRI: ~p", [lager:pr(R, ?MODULE)]) || R <- Value],
    lists:foldl(fun rule_to_session/2, Session, Value);
to_session(_, Session) ->
    Session.

to_list({Key, [A | _] = Avps}) when is_map(A) ->
    {Key, lists:map(fun to_list/1, Avps)};
to_list({Key, Avps}) when is_map(Avps) ->
    {Key, lists:map(fun to_list/1, maps:to_list(Avps))};
to_list(Avps) when is_map(Avps) ->
    lists:map(fun to_list/1, maps:to_list(Avps));
to_list(Avp) ->
    Avp.

to_map({_K, []}, M, _Dict) ->
    M;
to_map({K, V}, M, Dict)
  when is_tuple(V), is_atom(element(1, V)) ->
    M#{K => to_map(V, Dict)};
to_map({K, [V]}, M, Dict) ->
    M#{K => to_map(V, Dict)};
to_map({K, [_|_] = V}, M, Dict) ->
    M#{K => lists:map(fun (Item) -> to_map(Item, Dict) end, V)};
to_map({K, V}, M, _Dict) ->
    M#{K => V}.

to_map(Rec, Dict)
  when is_tuple(Rec) ->
    L = Dict:'#get-'(Rec),
    lists:foldl(to_map(_, _, Dict), #{}, tl(L));
to_map(Other, _Dict) ->
    Other.
