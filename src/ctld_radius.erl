-module(ctld_radius).

-behaviour(ctld_aaa).

%% AAA API
-export([init/1, authenticate/3, authorize/3, start/2, interim/2, stop/2]).

-import(ctld_session, [attr_get/2, attr_get/3, attr_set/3, attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

-record(state, {nas_id, auth_server, acct_server,
		auth_state, accounting = []}).

%%===================================================================
%% API
%%===================================================================
init(Opts) ->
    State = #state{
      nas_id = proplists:get_value(nas_identifier, Opts, <<"NAS">>),
      auth_server = proplists:get_value(radius_auth_server, Opts, {{127,0,0,1}, 1812, <<"secret">>}),
      acct_server = proplists:get_value(radius_acct_server, Opts, {{127,0,0,1}, 1813, <<"secret">>})
     },
    {ok, State}.

authenticate(_From, Session, State0 = #state{auth_server = NAS}) ->
    ExtraAttrs0 = accounting_options(State0, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?User_Name,       attr_get('Username', Session, <<>>)},
	     {?User_Password ,  attr_get('Password', Session, <<>>)},
	     {?Service_Type,    2},
	     {?Framed_Protocol, 1},
	     {?NAS_Identifier,  State0#state.nas_id}
	     | ExtraAttrs],
    Req = #radius_request{
             cmd = request,
             attrs = Attrs,
             msg_hmac = false},
    {Verdict, SessionOpts0, State} =
        radius_response(eradius_client:send_request(NAS, Req), NAS, State0),
    SessionOpts =
    if Verdict /= success -> [];
       true               -> SessionOpts0
    end,
    {reply, Verdict, SessionOpts, State#state{auth_state = Verdict}}.

authorize(_From, _Session, State = #state{auth_state = Verdict}) ->
    {reply, Verdict, to_session([]), State}.

start(Session, State = #state{acct_server = NAS, accounting = Accounting}) ->
    Now = now_ticks(),

    UserName0 = attr_get('Username', Session, <<>>),
    UserName = case proplists:get_value('Username', Accounting) of
		   undefined -> UserName0;
		   Value -> Value
	       end,
    ExtraAttrs0 = accounting_options(State, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Start},
	     {?User_Name,       UserName},
	     {?Service_Type,    2},
	     {?Framed_Protocol, 1},
	     {?NAS_Identifier,  State#state.nas_id}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},
    eradius_client:send_request(NAS, Req),

    SessionOpts = [{'Accounting-Start', Now}],
    {to_session(SessionOpts), State}.

interim(Session, State = #state{acct_server = NAS, accounting = Accounting}) ->
    Now = now_ticks(),

    UserName0 = attr_get('Username', Session, <<>>),
    UserName = case proplists:get_value('Username', Accounting) of
		   undefined -> UserName0;
		   Value -> Value
	       end,
    Start = attr_get('Accounting-Start', Session, Now),

    ExtraAttrs0 = accounting_options(State, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Update},
	     {?User_Name,       UserName},
	     {?Service_Type,    2},
	     {?Framed_Protocol, 1},
	     {?NAS_Identifier,  State#state.nas_id},
	     {?RSession_Time,   round((Now - Start) / 10)}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},
    eradius_client:send_request(NAS, Req),

    SessionOpts = [{'Last-Interim-Update', Now}],
    {to_session(SessionOpts), State}.

stop(Session, State = #state{acct_server = NAS}) ->
    Now = now_ticks(),

    Start = attr_get('Accounting-Start', Session, Now),

    ExtraAttrs0 = accounting_options(State, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Stop},
	     {?User_Name,       attr_get('Username', Session, <<>>)},
	     {?Service_Type,    2},
	     {?Framed_Protocol, 1},
	     {?NAS_Identifier,  State#state.nas_id},
	     {?RSession_Time,   round((Now - Start) / 10)}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},

    Res = eradius_client:send_request(NAS, Req),
    lager:debug("Accounting stop response from ~p: ~p : ~p", [NAS, Req, Res]),

    SessionOpts = [{'Accounting-Stop', Now}],
    {to_session(SessionOpts), State}.

%%===================================================================
%% Internal Helpers
%%===================================================================

%% get time with 100ms +/50ms presision
now_ticks() ->
    now_ticks(erlang:now()).

now_ticks({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 10000000 + Secs * 10 + round(MicroSecs div 100000).

accounting_options(#state{accounting = Accounting}, Acc) ->
    lists:foldl(fun({Key, Value}, Acc0) -> session_options(Key, Value, Acc0) end, Acc, Accounting).

session_options(Session, Acc) ->
    attr_fold(fun session_options/3, Acc, Session).

session_options('Framed-IP-Address', Value, Acc) ->
    [{?Framed_IP_Address, Value}|Acc];
session_options('Framed-Interface-Id', Value, Acc) ->
    [{?Framed_Interface_Id, Value}|Acc];
session_options('Session-Id', Value, Acc) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Session_Id, Id}|Acc];
session_options('Class', Class, Acc) ->
    [{?Class, Class}|Acc];
session_options('Calling-Station', Value, Acc) ->
    [{?Calling_Station_Id, Value}|Acc];
session_options('Called-Station', Value, Acc) ->
    [{?Called_Station_Id, Value}|Acc];
session_options('Port-Id', Value, Acc) ->
    [{?NAS_Port_Id, Value}|Acc];

session_options('Port-Type', pppoe_eth, Acc) ->
    [{?NAS_Port_Type, 32}|Acc];
session_options('Port-Type', pppoe_vlan, Acc) ->
    [{?NAS_Port_Type, 33}|Acc];
session_options('Port-Type', pppoe_qinq, Acc) ->
    [{?NAS_Port_Type, 34}|Acc];

session_options('Tunnel-Type', 'CAPWAP', Acc) ->
    [{?Tunnel_Type, 16#ff00}|Acc];
session_options('Tunnel-Medium-Type', 'IPv4', Acc) ->
    [{?Tunnel_Medium_Type, 1}|Acc];
session_options('Tunnel-Medium-Type', 'IPv6', Acc) ->
    [{?Tunnel_Medium_Type, 2}|Acc];
session_options('Tunnel-Client-Endpoint', Value, Acc) ->
    [{?Tunnel_Client_Endpoint, Value}|Acc];

%% DSL-Forum PPPoE Intermediate Agent Attributes
session_options('ADSL-Agent-Circuit-Id', Value, Acc) ->
    [{?ADSL_Agent_Circuit_Id, Value}|Acc];
session_options('ADSL-Agent-Remote-Id', Value, Acc) ->
    [{?ADSL_Agent_Remote_Id, Value}|Acc];
session_options('Actual-Data-Rate-Upstream', Value, Acc) ->
    [{?Actual_Data_Rate_Upstream, Value}|Acc];
session_options('Actual-Data-Rate-Downstream', Value, Acc) ->
    [{?Actual_Data_Rate_Downstream, Value}|Acc];
session_options('Minimum-Data-Rate-Upstream', Value, Acc) ->
    [{?Minimum_Data_Rate_Upstream, Value}|Acc];
session_options('Minimum-Data-Rate-Downstream', Value, Acc) ->
    [{?Minimum_Data_Rate_Downstream, Value}|Acc];
session_options('Attainable-Data-Rate-Upstream', Value, Acc) ->
    [{?Attainable_Data_Rate_Upstream, Value}|Acc];
session_options('Attainable-Data-Rate-Downstream', Value, Acc) ->
    [{?Attainable_Data_Rate_Downstream, Value}|Acc];
session_options('Maximum-Data-Rate-Upstream', Value, Acc) ->
    [{?Maximum_Data_Rate_Upstream, Value}|Acc];
session_options('Maximum-Data-Rate-Downstream', Value, Acc) ->
    [{?Maximum_Data_Rate_Downstream, Value}|Acc];
session_options('Minimum-Data-Rate-Upstream-Low-Power', Value, Acc) ->
    [{?Minimum_Data_Rate_Upstream_Low_Power, Value}|Acc];
session_options('Minimum-Data-Rate-Downstream-Low-Power', Value, Acc) ->
    [{?Minimum_Data_Rate_Downstream_Low_Power, Value}|Acc];
session_options('Maximum-Interleaving-Delay-Upstream', Value, Acc) ->
    [{?Maximum_Interleaving_Delay_Upstream, Value}|Acc];
session_options('Actual-Interleaving-Delay-Upstream', Value, Acc) ->
    [{?Actual_Interleaving_Delay_Upstream, Value}|Acc];
session_options('Maximum-Interleaving-Delay-Downstream', Value, Acc) ->
    [{?Maximum_Interleaving_Delay_Downstream, Value}|Acc];
session_options('Actual-Interleaving-Delay-Downstream', Value, Acc) ->
    [{?Actual_Interleaving_Delay_Downstream, Value}|Acc];

session_options('Authentication-Method', {'TLS', 'Pre-Shared-Key'}, Acc) ->
    [{?TP_TLS_Auth_Type, 0}|Acc];
session_options('Authentication-Method', {'TLS', 'X509-Subject-CN'}, Acc) ->
    [{?TP_TLS_Auth_Type, 1}|Acc];

session_options(_Key, _Value, Acc) ->
    Acc.

radius_response({ok, Response}, {_, _, Secret}, State) ->
    radius_reply(eradius_lib:decode_request(Response, Secret), State);
radius_response(Response, _, State) ->
    lager:error("RADIUS failed with ~p", [Response]),
    {fail, [], State}.

radius_reply(#radius_request{cmd = accept} = Reply, State0) ->
    lager:debug("RADIUS Reply: ~p", [Reply]),
    case process_radius_attrs(fun process_pap_attrs/2, Reply, success, State0) of
	{success, _, _} = Result ->
	    Result;
	_ ->
	    {fail, [], State0}
    end;
radius_reply(#radius_request{cmd = reject} = Reply, State) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    {fail, [], State};
radius_reply(Reply, State) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    {fail, [], State}.

%% iterate over the RADIUS attributes
process_radius_attrs(Fun, #radius_request{attrs = Attrs}, Verdict, State) ->
    lists:foldr(Fun, {Verdict, to_session([]), State}, Attrs).

process_pap_attrs(AVP, {_Verdict, _Opts, _State} = Acc0) ->
    process_gen_attrs(AVP, Acc0).

verdict(Verdict, {_, Opts, State}) ->
    {Verdict, Opts, State}.
session_opt(Key, Opt, {Verdict, Opts, State}) ->
    {Verdict, attr_set(Key, Opt, Opts), State}.
session_opt_append(Key, Opt, {Verdict, Opts, State}) ->
    {Verdict, attr_append(Key, Opt, Opts), State}.
accounting_opt(Opt, {Verdict, Opts, State = #state{accounting = Accounting}}) ->
    {Verdict, Opts, State#state{accounting = [Opt|Accounting]}}.

%% Class
process_gen_attrs({#attribute{id = ?Class}, Class}, Acc) ->
    accounting_opt({class, Class}, Acc);

%% User-Name
process_gen_attrs({#attribute{id = ?User_Name}, UserName}, Acc) ->
    accounting_opt({username, UserName}, Acc);

process_gen_attrs({#attribute{id = ?Acct_Interim_Interval}, InterimAccounting}, Acc) ->
    session_opt('Interim-Accounting', InterimAccounting * 1000, Acc);

%% Session-Timeout
process_gen_attrs({#attribute{id = ?Session_Timeout}, TimeOut}, Acc) ->
    session_opt('Session-Timeout', TimeOut * 1000, Acc);

%% Service-Type = Framed-User
process_gen_attrs({#attribute{id = ?Service_Type}, 2}, Acc) ->
    Acc;
process_gen_attrs(AVP = {#attribute{id = ?Service_Type}, _}, Acc) ->
    process_unexpected_value(AVP, Acc);

%% Framed-Protocol = PPP
process_gen_attrs({#attribute{id = ?Framed_Protocol}, 1}, Acc) ->
    Acc;
process_gen_attrs(AVP = {#attribute{id = ?Framed_Protocol}, _}, Acc) ->
    process_unexpected_value(AVP, Acc);

%% Framed-IP-Address = xx.xx.xx.xx
process_gen_attrs({#attribute{id = ?Framed_IP_Address}, IP}, Acc) ->
    session_opt('Framed-IP-Address', IP, Acc);

%% Framed-Interface-Id
process_gen_attrs({#attribute{id = ?Framed_Interface_Id}, Id}, Acc) ->
    session_opt('Framed-Interface-Id', Id, Acc);

%% Alc-Primary-Dns
process_gen_attrs({#attribute{id = ?Alc_Primary_Dns}, DNS}, Acc) ->
    session_opt_append('DNS', DNS, Acc);
%% Alc-Secondary-Dns
process_gen_attrs({#attribute{id = ?Alc_Secondary_Dns}, DNS}, Acc) ->
    session_opt_append('DNS', DNS, Acc);

%% TP-TLS-Pre-Shared-Key
process_gen_attrs({#attribute{id = ?TP_TLS_Pre_Shared_Key}, PSK}, Acc) ->
    session_opt('TLS-Pre-Shared-Key', PSK, Acc);

process_gen_attrs({#attribute{name = Name}, Value} , Acc) ->
    lager:debug("unhandled reply AVP: ~s: ~p", [Name, Value]),
    Acc;

process_gen_attrs({Attr, Value} , Acc) ->
    lager:debug("unhandled undecoded reply AVP: ~w: ~p", [Attr, Value]),
    Acc.

process_unexpected_value({#attribute{name = Name}, Value} , Acc) ->
    lager:debug("unexpected Value in AVP: ~s: ~p", [Name, Value]),
    verdict(fail, Acc).
