-module(ergw_aaa_radius).

-behaviour(ergw_aaa).

%% AAA API
-export([init/1, authorize/3, start_authentication/3, start_accounting/4]).

-import(ergw_aaa_session, [attr_get/2, attr_get/3, attr_set/3, attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include("include/ergw_aaa_profile.hrl").
-include("include/ergw_aaa_variable.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_microsoft.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

-record(state, {nas_id,
		auth_server, acct_server,
		auth_state,
		radius_session = [],
		acct_app_id = default}).

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

copy_session_id(#{'Session-Id' := SessionId}, Opts) ->
    Opts#{'Session-Id' => SessionId};
copy_session_id(_, Opts) ->
    Opts.

session_auth_options('PAP', Session, Attrs) ->
    [{?User_Password , attr_get('Password', Session, <<>>)} | Attrs];
session_auth_options(_, _Session, Attrs) ->
    Attrs.

start_authentication(From, Session, State0 = #state{auth_server = NAS}) ->
    Attrs0 = [{?User_Name,       attr_get('Username', Session, <<>>)},
	      {?NAS_Identifier,  State0#state.nas_id}],
    Attrs1 = session_auth_options(attr_get('Authentication-Method', Session, 'PAP'), Session, Attrs0),
    Attrs2 = radius_session_options(State0, Attrs1),
    Attrs3 = session_options(Session, Attrs2),
    Attrs = remove_accounting_attrs(Attrs3),

    Req = #radius_request{
             cmd = request,
             attrs = Attrs,
	     msg_hmac = true,
	     eap_msg = attr_get('EAP-Data', Session, <<>>)},

    Pid = proc_lib:spawn_link(fun() ->
				      {Verdict, SessionOpts0, State} =
					  radius_response(eradius_client:send_request(NAS, Req), NAS, State0),
				      NewSessionOpts0 = to_session(SessionOpts0),
				      NewSessionOpts1 = copy_session_id(Session, NewSessionOpts0),
				      ?queue_event(From, {'AuthenticationRequestReply', {Verdict, NewSessionOpts1, State#state{auth_state = Verdict}}})
			      end),
    {ok, State0#state{auth_state = Pid}}.

authorize(_From, _Session, State = #state{auth_state = Verdict}) ->
    {reply, Verdict, to_session([]), State}.

start_accounting(_From, 'Start', Session, State = #state{acct_server = NAS, radius_session = RadiusSession}) ->
    UserName0 = attr_get('Username', Session, <<>>),
    UserName = case proplists:get_value('Username', RadiusSession) of
		   undefined -> UserName0;
		   Value -> Value
	       end,
    ExtraAttrs1 = radius_session_options(State, []),
    ExtraAttrs0 = session_options(Session, ExtraAttrs1),
    ExtraAttrs = [X || X = {K, _} <- ExtraAttrs0,
		       K /= ?Acct_Input_Octets, K /= ?Acct_Output_Octets,
		       K /= ?Acct_Input_Gigawords, K /= ?Acct_Output_Gigawords,
		       K /= ?Acct_Input_Packets, K /= ?Acct_Output_Packets],
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Start},
	     {?User_Name,       UserName},
	     {?NAS_Identifier,  State#state.nas_id}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},

    proc_lib:spawn_link(fun() -> eradius_client:send_request(NAS, Req) end),

    {ok, State};

start_accounting(_From, 'Interim', Session, State = #state{acct_server = NAS, radius_session = RadiusSession}) ->
    Now = ergw_aaa_variable:now_ms(),

    UserName0 = attr_get('Username', Session, <<>>),
    UserName = case proplists:get_value('Username', RadiusSession) of
		   undefined -> UserName0;
		   Value -> Value
	       end,
    Start = attr_get('Accounting-Start', Session, Now),

    ExtraAttrs0 = radius_session_options(State, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Update},
	     {?User_Name,       UserName},
	     {?NAS_Identifier,  State#state.nas_id},
	     {?RSession_Time,   round((Now - Start) / 1000)}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},

    proc_lib:spawn_link(fun() -> eradius_client:send_request(NAS, Req) end),

    {ok, State};

start_accounting(_From, 'Stop', Session, State = #state{acct_server = NAS}) ->
    Now = ergw_aaa_variable:now_ms(),

    Start = attr_get('Accounting-Start', Session, Now),

    ExtraAttrs0 = radius_session_options(State, []),
    ExtraAttrs = session_options(Session, ExtraAttrs0),
    Attrs = [
	     {?RStatus_Type,    ?RStatus_Type_Stop},
	     {?User_Name,       attr_get('Username', Session, <<>>)},
	     {?Service_Type,    2},
	     {?Framed_Protocol, 1},
	     {?NAS_Identifier,  State#state.nas_id},
	     {?RSession_Time,   round((Now - Start) / 1000)}
	     | ExtraAttrs],
    Req = #radius_request{
	     cmd = accreq,
	     attrs = Attrs,
	     msg_hmac = false},

    proc_lib:spawn(fun() -> eradius_client:send_request(NAS, Req) end),

    {ok, State}.

%%===================================================================
%% Internal Helpers
%%===================================================================

%% %% get time with 100ms +/50ms presision
%% now_ticks() ->
%%     now_ticks(erlang:now()).

%% now_ticks({MegaSecs, Secs, MicroSecs}) ->
%%     MegaSecs * 10000000 + Secs * 10 + round(MicroSecs div 100000).

radius_session_options(#state{radius_session = RadiusSession}, Acc) ->
    lists:foldl(fun({Key, Value}, Acc0) -> session_options(Key, Value, Acc0) end, Acc, RadiusSession).

session_options(Session, Acc) ->
    attr_fold(fun session_options/3, Acc, Session).

session_options('IP', Value, Acc) ->
    [{?Framed_IP_Address, Value}|Acc];

session_options('Service-Type', Value, Acc) ->
    [{?Service_Type, service_type(Value)}|Acc];
session_options('Framed-Protocol', Value, Acc) ->
    [{?Framed_Protocol, framed_protocol(Value)}|Acc];

session_options('Framed-IP-Address', Value, Acc) ->
    [{?Framed_IP_Address, Value}|Acc];
session_options('Framed-Interface-Id', Value, Acc) ->
    [{?Framed_Interface_Id, Value}|Acc];
session_options('Session-Id', Value, Acc) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Session_Id, Id}|Acc];
session_options('Multi-Session-Id', Value, Acc) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Multi_Session_Id, Id}|Acc];
session_options('Class', Class, Acc) ->
    [{?Class, Class}|Acc];
session_options('RADIUS-State', State, Acc) ->
    [{?State, State}|Acc];
session_options('Calling-Station', Value, Acc) ->
    [{?Calling_Station_Id, Value}|Acc];
session_options('Called-Station', Value, Acc) ->
    [{?Called_Station_Id, Value}|Acc];
session_options('Port-Id', Value, Acc) ->
    [{?NAS_Port_Id, Value}|Acc];

session_options('InOctets', Octets, Acc) when is_integer(Octets) ->
    [{?Acct_Input_Octets, Octets}, {?Acct_Input_Gigawords, Octets bsr 32}|Acc];
session_options('InOctets', Value, Acc) when is_record(Value, var) ->
    Octets = ergw_aaa_variable:get(Value),
    [{?Acct_Input_Octets, Octets}, {?Acct_Input_Gigawords, Octets bsr 32}|Acc];
session_options('InPackets', Packets, Acc) when is_integer(Packets) ->
    [{?Acct_Input_Packets, Packets}|Acc];
session_options('InPackets', Value, Acc) when is_record(Value, var) ->
    [{?Acct_Input_Packets, ergw_aaa_variable:get(Value)}|Acc];
session_options('OutOctets', Octets, Acc) when is_integer(Octets) ->
    [{?Acct_Output_Octets, Octets}, {?Acct_Output_Gigawords, Octets bsr 32}|Acc];
session_options('OutOctets', Value, Acc) when is_record(Value, var) ->
    Octets = ergw_aaa_variable:get(Value),
    [{?Acct_Output_Octets, Octets}, {?Acct_Output_Gigawords, Octets bsr 32}|Acc];
session_options('OutPackets', Packets, Acc) when is_integer(Packets) ->
    [{?Acct_Output_Packets, Packets}|Acc];
session_options('OutPackets', Value, Acc) when is_record(Value, var) ->
    [{?Acct_Output_Packets, ergw_aaa_variable:get(Value)}|Acc];

session_options('Port-Type', Value, Acc) ->
    [{?NAS_Port_Type, port_type(Value)}|Acc];

session_options(Type, Value, Acc)
  when is_list(Value) andalso
       (Type == 'Tunnel-Type' orelse
	Type == 'Tunnel-Medium-Type' orelse
	Type == 'Tunnel-Client-Endpoint') ->
    lists:foldl(fun(V, A) -> session_options(Type, V, A) end, Acc, Value);

session_options('Tunnel-Type', {Tag, Type}, Acc) ->
    [{?Tunnel_Type, {Tag, tunnel_type(Type)}}|Acc];
session_options('Tunnel-Type', Type, Acc) ->
    [{?Tunnel_Type, tunnel_type(Type)}|Acc];

session_options('Tunnel-Medium-Type', {Tag, Type}, Acc) ->
    [{?Tunnel_Medium_Type, {Tag, tunnel_medium_type(Type)}}|Acc];
session_options('Tunnel-Medium-Type', Type, Acc) ->
    [{?Tunnel_Medium_Type, tunnel_medium_type(Type)}|Acc];

session_options('Tunnel-Client-Endpoint', {Tag, Value}, Acc) ->
    [{?Tunnel_Client_Endpoint, {Tag, Value}}|Acc];
session_options('Tunnel-Client-Endpoint', Value, Acc) ->
    [{?Tunnel_Client_Endpoint, Value}|Acc];

session_options('Acct-Authentic', 'RADIUS', Acc) ->
    [{?Acct_Authentic, 1}|Acc];
session_options('Acct-Authentic', 'Local', Acc) ->
    [{?Acct_Authentic, 2}|Acc];
session_options('Acct-Authentic', 'Remote', Acc) ->
    [{?Acct_Authentic, 3}|Acc];
session_options('Acct-Authentic', 'Diameter', Acc) ->
    [{?Acct_Authentic, 4}|Acc];

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

%% Travelping Extension
session_options('Zone-Id', Value, Acc) ->
    [{?TP_Zone_Id, Value}|Acc];
session_options('Location-Id', Value, Acc) ->
    [{?TP_Location_Id, Value}|Acc];
session_options('Access-Group', Value, Acc) ->
    [{?TP_Access_Group, list_to_binary(Value)}|Acc];
session_options('Access-Class-Id', Value, Acc) ->
    [{?TP_Access_Class_Id, list_to_binary(Value)}|Acc];
session_options('NAT-Pool-Id', Value, Acc) ->
    [{?TP_NAT_Pool_Id, Value}|Acc];
session_options('NAT-IP-Address', Value, Acc) ->
    [{?TP_NAT_IP_Address, Value}|Acc];
session_options('NAT-Port-Start', Value, Acc) ->
    [{?TP_NAT_Port_Start, Value}|Acc];
session_options('NAT-Port-End', Value, Acc) ->
    [{?TP_NAT_Port_End, Value}|Acc];

session_options('DHCP-Parameter-Request-List', Value, Acc) ->
    [{?TP_DHCP_Parameter_Request_List, Value}|Acc];
session_options('DHCP-Request-Option-List', Value, Acc) ->
    [{?TP_DHCP_Request_Option_List, Value}|Acc];

%% TP CAPWAP extensions - Versions

session_options('CAPWAP-WTP-Version', Value, Acc) ->
    [{?TP_CAPWAP_WTP_Version, Value}|Acc];
session_options('CAPWAP-Hardware-Version', Version, Acc) ->
    [{?TP_CAPWAP_Hardware_Version, Version}|Acc];
session_options('CAPWAP-Software-Version', Version, Acc) ->
    [{?TP_CAPWAP_Software_Version, Version}|Acc];
session_options('CAPWAP-Boot-Version', Version, Acc) ->
    [{?TP_CAPWAP_Boot_Version, Version}|Acc];
session_options('CAPWAP-Other-Software-Version', Version, Acc) ->
    [{?TP_CAPWAP_Other_Software_Version, Version}|Acc];

%% TP CAPWAP extensions - generic

session_options('CAPWAP-Timestamp', Value, Acc) ->
    [{?TP_CAPWAP_Timestamp, Value}|Acc];
session_options('CAPWAP-Session-Id', Value, Acc) ->
    [{?TP_CAPWAP_Session_Id, Value}|Acc];
session_options('CAPWAP-Radio-Id', Value, Acc) ->
    [{?TP_CAPWAP_Radio_Id, Value}|Acc];

%% TP CAPWAP extensions - statistics

session_options('CAPWAP-WWAN-Id', Value, Acc) ->
    [{?TP_CAPWAP_WWAN_Id, Value}|Acc];
session_options('CAPWAP-WWAN-RAT', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_RAT, VALUE}|ACC];
session_options('CAPWAP-WWAN-RSSi', Value, Acc) ->
    [{?TP_CAPWAP_WWAN_RSSi, Value}|Acc];
session_options('CAPWAP-WWAN-CREG', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_CREG, VALUE}|ACC];
session_options('CAPWAP-WWAN-LAC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_LAC, VALUE}|ACC];
session_options('CAPWAP-WWAN-Latency', Value, Acc) ->
    [{?TP_CAPWAP_WWAN_Latency, Value}|Acc];
session_options('CAPWAP-WWAN-MCC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_MCC, VALUE}|ACC];
session_options('CAPWAP-WWAN-MNC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_MNC, VALUE}|ACC];
session_options('CAPWAP-WWAN-Cell-Id', Value, Acc) ->
    [{?TP_CAPWAP_WWAN_Cell_Id, Value}|Acc];

%% TP CAPWAP extensions - GPSATC

session_options('CAPWAP-GPS-Timestamp', Value, Acc) ->
    [{?TP_CAPWAP_GPS_Timestamp, Value}| Acc];
session_options('CAPWAP-GPS-Latitude', Value, Acc) ->
    [{?TP_CAPWAP_GPS_Latitude, Value} | Acc];
session_options('CAPWAP-GPS-Longitude', Value, Acc) ->
    [{?TP_CAPWAP_GPS_Longitude, Value} | Acc];
session_options('CAPWAP-GPS-Altitude', Value, Acc) ->
    [{?TP_CAPWAP_GPS_Altitude, Value} | Acc];
session_options('CAPWAP-GPS-Hdop', Value, Acc) ->
    [{?TP_CAPWAP_GPS_Hdop, Value} | Acc];

session_options(_Key, _Value, Acc) ->
    Acc.

port_type(pppoe_eth)  -> 32;
port_type(pppoe_vlan) -> 33;
port_type(pppoe_qinq) -> 34.

tunnel_type('CAPWAP') -> 16#ff00.

tunnel_medium_type('IPv4') -> 1;
tunnel_medium_type('IPv6') -> 2.

service_type('Login-User')              -> 1;
service_type('Framed-User')             -> 2;
service_type('Callback-Login-User')     -> 3;
service_type('Callback-Framed-User')    -> 4;
service_type('Outbound-User')           -> 5;
service_type('Administrative-User')     -> 6;
service_type('NAS-Prompt-User')         -> 7;
service_type('Authenticate-Only')       -> 8;
service_type('Callback-NAS-Prompt')     -> 9;
service_type('Call-Check')              -> 10;
service_type('Callback-Administrative') -> 11;
service_type('Voice')                   -> 12;
service_type('Fax')                     -> 13;
service_type('Modem-Relay')             -> 14;
service_type('IAPP-Register')           -> 15;
service_type('IAPP-AP-Check')           -> 16;
service_type('TP-CAPWAP-WTP')           -> 16#48f90001;
service_type('TP-CAPWAP-STA')           -> 16#48f90002;
service_type(_)                         -> 2.

framed_protocol('PPP')               -> 1;
framed_protocol('SLIP')              -> 2;
framed_protocol('ARAP')              -> 3;
framed_protocol('Gandalf-SLML')      -> 4;
framed_protocol('Xylogics-IPX-SLIP') -> 5;
framed_protocol('X.75-Synchronous')  -> 6;
framed_protocol('GPRS-PDP-Context')  -> 7;
framed_protocol('TP-CAPWAP')         -> 16#48f90001;
framed_protocol(_)                   -> 1.

radius_response({ok, Response, RequestAuthenticator}, {_, _, Secret}, State) ->
    radius_reply(eradius_lib:decode_request(Response, Secret, RequestAuthenticator), State);
radius_response(Response, _, State) ->
    lager:error("RADIUS failed with ~p", [Response]),
    {fail, [], State}.

radius_reply(#radius_request{cmd = accept} = Reply, State0) ->
    lager:debug("RADIUS Reply: ~p", [Reply]),
    case process_radius_attrs(fun process_pap_attrs/2, Reply, success, State0) of
	{success, _, _} = Result0 ->
	    Result = handle_eap_msg(Reply, Result0),
	    session_opt('Acct-Authentic', 'RADIUS', Result);
	_ ->
	    {fail, [], State0}
    end;

radius_reply(#radius_request{cmd = challenge} = Reply, State0) ->
    lager:debug("RADIUS Challenge: ~p", [lager:pr(Reply, ?MODULE)]),
    case process_radius_attrs(fun process_chap_attrs/2, Reply, challenge, State0) of
	{challenge, _, _} = Result ->
	    handle_eap_msg(Reply, Result);
	_ ->
	    {fail, [], State0}
    end;

radius_reply(#radius_request{cmd = reject} = Reply, State) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    handle_eap_msg(Reply, {fail, to_session([]), State});

radius_reply(Reply, State) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    {fail, [], State}.

handle_eap_msg(#radius_request{eap_msg = EAP}, Result)
  when EAP /= <<>> ->
    session_opt('EAP-Data', EAP, Result);
handle_eap_msg(_, Result) ->
    Result.

%% iterate over the RADIUS attributes
process_radius_attrs(Fun, #radius_request{attrs = Attrs}, Verdict, State) ->
    lists:foldr(Fun, {Verdict, to_session([]), State}, Attrs).

process_pap_attrs(AVP, {_Verdict, _Opts, _State} = Acc0) ->
    process_gen_attrs(AVP, Acc0).

process_chap_attrs(AVP, {_Verdict, _Opts, _State} = Acc0) ->
    process_gen_attrs(AVP, Acc0).

verdict(Verdict, {_, Opts, State}) ->
    {Verdict, Opts, State}.
session_opt(Key, Opt, {Verdict, Opts, State}) ->
    {Verdict, attr_set(Key, Opt, Opts), State}.
session_opt_append(Key, Opt, {Verdict, Opts, State}) ->
    {Verdict, attr_append(Key, Opt, Opts), State}.

radius_session_opt(Key, Opt, {Verdict, Opts, State = #state{radius_session = RadiusSession}}) ->
    {Verdict, Opts, State#state{radius_session = lists:keystore(Key, 1, RadiusSession, {Key, Opt})}}.

%% Class
process_gen_attrs({#attribute{id = ?Class}, Class}, Acc) ->
    radius_session_opt('Class', Class, Acc);

%% State
process_gen_attrs({#attribute{id = ?State}, State}, Acc) ->
    radius_session_opt('RADIUS-State', State, Acc);

%% User-Name
process_gen_attrs({#attribute{id = ?User_Name}, UserName}, Acc) ->
    radius_session_opt('Username', UserName, Acc);

process_gen_attrs({#attribute{id = ?Acct_Interim_Interval}, InterimAccounting}, Acc) ->
    session_opt('Interim-Accounting', InterimAccounting * 1000, Acc);

%% Session-Timeout
process_gen_attrs({#attribute{id = ?Session_Timeout}, TimeOut}, Acc) ->
    session_opt('Session-Timeout', TimeOut * 1000, Acc);

%% Idle-Timeout
process_gen_attrs({#attribute{id = ?Idle_Timeout}, TimeOut}, Acc) ->
    session_opt('Idle-Timeout', TimeOut * 1000, Acc);

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


%% Microsoft MPPE Keys

%% MS-MPPE-Send-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Send_Key}, Value}, Acc) ->
    session_opt('MS-MPPE-Send-Key', Value, Acc);

%% MS-MPPE-Recv-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Recv_Key}, Value}, Acc) ->
    session_opt('MS-MPPE-Recv-Key', Value, Acc);

%% Travelping Extensions

%% TP-Access-Rule
process_gen_attrs({#attribute{id = ?TP_Access_Rule}, Value}, Acc) ->
    Rule = list_to_tuple([binary_to_list(V) || V <- binary:split(Value, <<":">>, [global])]),
    session_opt_append('Access-Rules', Rule, Acc);

%% TP-Access-Group
process_gen_attrs({#attribute{id = ?TP_Access_Group}, Value}, Acc) ->
    session_opt('Access-Group', binary_to_list(Value), Acc);

%% TP-NAT-Pool-Id
process_gen_attrs({#attribute{id = ?TP_NAT_Pool_Id}, Value}, Acc) ->
    session_opt('NAT-Pool-Id', Value, Acc);

%% TP-NAT-IP-Address
process_gen_attrs({#attribute{id = ?TP_NAT_IP_Address}, Value}, Acc) ->
    session_opt('NAT-IP-Address', Value, Acc);

%% TP-NAT-Port-Start
process_gen_attrs({#attribute{id = ?TP_NAT_Port_Start}, Value}, Acc) ->
    session_opt('NAT-Port-Start', Value, Acc);

%% TP-NAT-Port-End
process_gen_attrs({#attribute{id = ?TP_NAT_Port_End}, Value}, Acc) ->
    session_opt('NAT-Port-End', Value, Acc);

%% TP-Max-Input-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Total_Octets}, Value}, Acc) ->
    session_opt('Max-Input-Octets', Value, Acc);

%% TP-Max-Output-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Output_Octets}, Value}, Acc) ->
    session_opt('Max-Output-Octets', Value, Acc);

%% TP-Max-Total-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Input_Octets}, Value}, Acc) ->
    session_opt('Max-Total-Octets', Value, Acc);

%% TP-TLS-Pre-Shared-Key
process_gen_attrs({#attribute{id = ?TP_TLS_Pre_Shared_Key}, PSK}, Acc) ->
    session_opt('TLS-Pre-Shared-Key', PSK, Acc);

%% CAPWAP LocationProfile attributes
process_gen_attrs({#attribute{id = ?TP_CAPWAP_SSID}, SSID}, Acc) ->
    session_opt('CAPWAP-SSID', SSID, Acc);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Max_WIFI_Clients}, MaxClients}, Acc) ->
    session_opt('CAPWAP-Max-WIFI-Clients', MaxClients, Acc);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Power_Save_Idle_Timeout}, WG}, Acc) ->
    session_opt('CAPWAP-Power-Save-Idle-Timeout', WG, Acc);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Power_Save_Busy_Timeout}, WG}, Acc) ->
    session_opt('CAPWAP-Power-Save-Busy-Timeout', WG, Acc);

%% Handling undefined cases
process_gen_attrs({#attribute{name = Name}, Value} , Acc) ->
    lager:debug("unhandled reply AVP: ~s: ~p", [Name, Value]),
    Acc;

process_gen_attrs({Attr, Value} , Acc) ->
    lager:debug("unhandled undecoded reply AVP: ~w: ~p", [Attr, Value]),
    Acc.

process_unexpected_value({#attribute{name = Name}, Value} , Acc) ->
    lager:debug("unexpected Value in AVP: ~s: ~p", [Name, Value]),
    verdict(fail, Acc).

remove_accounting_attrs(Attrs) ->
    FilterOpts = radius_accounting_opts(),
    lists:filter(fun({Opt, _}) -> not gb_sets:is_member(Opt, FilterOpts) end, Attrs).

radius_accounting_opts() ->
    gb_sets:from_list([?Acct_Type,
		       ?Acct_Session_Start_Time,
		       ?Acct_Interim_Interval,
		       ?Acct_Tunnel_Connection,
		       ?Acct_Output_Gigawords,
		       ?Acct_Input_Gigawords,
		       ?Acct_Link_Count,
		       ?Acct_Terminate_Cause,
		       ?Acct_Output_Packets,
		       ?Acct_Input_Packets,
		       ?Acct_Session_Time,
		       ?Acct_Authentic,
		       ?Acct_Output_Octets,
		       ?Acct_Input_Octets,
		       ?Acct_Delay_Time,
		       ?Acct_Status_Type]).
