%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_radius).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_options/1, initialize_provider/1,
	 init/1, authorize/3, start_authentication/3, start_accounting/4]).

-import(ergw_aaa_session, [attr_get/2, attr_get/3, attr_set/3, attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include("include/ergw_aaa_profile.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_3gpp.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_microsoft.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

-record(state, {nas_id,
		auth_server, acct_server,
		auth_state,
		radius_session = [],
		acct_app_id = default,
                disabled_acct = false :: boolean(),
                disabled_auth = false :: boolean()
               }).

-define(DefaultOptions, [{nas_identifier, undefined},
			 {radius_auth_server, undefined},
			 {radius_acct_server, undefined}
			]).

%%===================================================================
%% API
%%===================================================================

initialize_provider(_Opts) ->
    eradius_dict:load_tables([dictionary,
			      dictionary_3gpp,
			      dictionary_tunnel,
			      dictionary_rfc4679,
			      dictionary_alcatel_sr,
			      dictionary_microsoft,
			      dictionary_travelping]),
    {ok, []}.

validate_options(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions).

init(Opts) ->
    DisabledOpts = proplists:get_value(disabled, Opts, []),
    State = #state{
      nas_id = proplists:get_value(nas_identifier, Opts, <<"NAS">>),
      auth_server = proplists:get_value(radius_auth_server, Opts, {{127,0,0,1}, 1812, <<"secret">>}),
      acct_server = proplists:get_value(radius_acct_server, Opts, {{127,0,0,1}, 1813, <<"secret">>}),
      disabled_auth = lists:member(auth, DisabledOpts),
      disabled_acct = lists:member(acct, DisabledOpts)
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

start_authentication(From, Session, State = #state{disabled_auth = true}) ->
    SessionOpts = copy_session_id(Session, #{}),
    Verdict = success,
    ?queue_event(From, {'AuthenticationRequestReply', {Verdict, SessionOpts, State}}),
    {ok, State};
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
				      MetricsInfo = <<(State0#state.nas_id)/binary, <<"_auth">>/binary>>,
				      RadiusClientOpts = [{client_name, MetricsInfo}, {server_name, MetricsInfo}],
				      {Verdict, SessionOpts0, State} =
					  radius_response(eradius_client:send_request(NAS, Req, RadiusClientOpts), NAS, State0),
				      NewSessionOpts0 = to_session(SessionOpts0),
				      NewSessionOpts1 = copy_session_id(Session, NewSessionOpts0),
				      ?queue_event(From, {'AuthenticationRequestReply', {Verdict, NewSessionOpts1, State#state{auth_state = Verdict}}})
			      end),
    {ok, State0#state{auth_state = Pid}}.

authorize(_From, _Session, State = #state{auth_state = Verdict}) ->
    {reply, Verdict, to_session([]), State}.

start_accounting(_From, _, _, State = #state{disabled_acct = true}) ->
    {ok, State};
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

    proc_lib:spawn_link(fun() ->
                                MetricsInfo = <<(State#state.nas_id)/binary, <<"_acct">>/binary>>,
                                RadiusClientOpts = [{client_name, MetricsInfo}, {server_name, MetricsInfo}],
                                eradius_client:send_request(NAS, Req, RadiusClientOpts)
                        end),

    {ok, State};

start_accounting(_From, 'Interim', Session, State = #state{acct_server = NAS, radius_session = RadiusSession}) ->
    Now = erlang:monotonic_time(milli_seconds),

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

    proc_lib:spawn_link(fun() ->
                                MetricsInfo = <<(State#state.nas_id)/binary, <<"_acct">>/binary>>,
                                RadiusClientOpts = [{client_name, MetricsInfo}, {server_name, MetricsInfo}],
                                eradius_client:send_request(NAS, Req, RadiusClientOpts)
                        end),

    {ok, State};

start_accounting(_From, 'Stop', Session, State = #state{acct_server = NAS}) ->
    Now = erlang:monotonic_time(milli_seconds),

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

    proc_lib:spawn(fun() ->
                           MetricsInfo = <<(State#state.nas_id)/binary, <<"_acct">>/binary>>,
                           RadiusClientOpts = [{client_name, MetricsInfo}, {server_name, MetricsInfo}],
                           eradius_client:send_request(NAS, Req, RadiusClientOpts)
                   end),

    {ok, State}.


%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_ip(Opt, Host) when is_list(Host) ->
    case gethostbyname(Host) of
	{ok, #hostent{h_addr_list = [IP | _]}} ->
	    IP;
	_ ->
	    lager:error("can't resolve remote RADIUS server name '~s'", [Host]),
	    throw({error, {options, {Opt, Host}}})
    end;
validate_ip(_Opt, {_,_,_,_} = IP) ->
    IP;
validate_ip(_Opt, {_,_,_,_,_,_,_,_} = IP) ->
    IP;
validate_ip(Opt, IP) ->
    throw({error, {options, {Opt, IP}}}).

validate_server_spec(Opt, {IP, Port, Secret} = Value)
  when is_integer(Port), is_binary(Secret) ->
    validate_ip(Opt, IP),
    Value;
validate_server_spec(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_option(nas_identifier, Value) when is_binary(Value) ->
    Value;
validate_option(radius_auth_server, Value) ->
    validate_server_spec(radius_auth_server, Value);
validate_option(radius_acct_server, Value) ->
    validate_server_spec(radius_auth_server, Value);
validate_option(Opt = disabled, Value) when is_list(Value) ->
    lists:all(fun(El) ->
        lists:member(El, [acct, auth])
    end, Value) orelse throw({error, {options, {Opt, Value}}}),
    lists:usort(Value);
validate_option(acct_interim_interval, Value) when is_integer(Value) ->
    Value;
validate_option(service_type, Value) when is_atom(Value) ->
    Value;
validate_option(framed_protocol, Value) when is_atom(Value) ->
    Value;
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% Internal Helpers
%%===================================================================

gethostbyname(Name) ->
    case inet:gethostbyname(Name, inet6) of
        {error, nxdomain} ->
            inet:gethostbyname(Name, inet);
        Other ->
            Other
    end.

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
session_options('Calling-Station-Id', Value, Acc) ->
    [{?Calling_Station_Id, Value}|Acc];
session_options('Called-Station-Id', Value, Acc) ->
    [{?Called_Station_Id, Value}|Acc];
session_options('Port-Id', Value, Acc) ->
    [{?NAS_Port_Id, Value}|Acc];
session_options('NAS-IP-Address', Value, Acc) ->
    [{?NAS_IP_Address, Value}|Acc];

session_options('InOctets', Octets, Acc) when is_integer(Octets) ->
    [{?Acct_Input_Octets, Octets}, {?Acct_Input_Gigawords, Octets bsr 32}|Acc];
session_options('InPackets', Packets, Acc) when is_integer(Packets) ->
    [{?Acct_Input_Packets, Packets}|Acc];
session_options('OutOctets', Octets, Acc) when is_integer(Octets) ->
    [{?Acct_Output_Octets, Octets}, {?Acct_Output_Gigawords, Octets bsr 32}|Acc];
session_options('OutPackets', Packets, Acc) when is_integer(Packets) ->
    [{?Acct_Output_Packets, Packets}|Acc];

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

%% 3GPP TS 29.061 Attributes
session_options('3GPP-IMSI', Value, ACC) ->
    [{?X_3GPP_IMSI, Value}|ACC];
session_options('3GPP-Charging-Id', Value, Acc) ->
    [{?X_3GPP_Charging_ID, Value}|Acc];

session_options('3GPP-PDP-Type', 'IPv4', Acc) ->
    [{?X_3GPP_PDP_Type, 0}|Acc];
session_options('3GPP-PDP-Type', 'IPv6', Acc) ->
    [{?X_3GPP_PDP_Type, 2}|Acc];
session_options('3GPP-PDP-Type', 'IPv4v6', Acc) ->
    [{?X_3GPP_PDP_Type, 3}|Acc];
session_options('3GPP-PDP-Type', 'PPP', Acc) ->
    [{?X_3GPP_PDP_Type, 1}|Acc];
session_options('3GPP-PDP-Type', 'Non-IP', Acc) ->
    [{?X_3GPP_PDP_Type, 4}|Acc];

session_options('3GPP-Charging-Gateway-Address', Value, Acc) ->
    [{?X_3GPP_Charging_Gateway_Address, Value}|Acc];
session_options('3GPP-GPRS-Negotiated-QoS-Profile', QoS, Acc) ->
    Value0 = iolist_to_binary([io_lib:format("~2.16.0B", [X]) || <<X>> <= QoS ]),
    Value =
	case byte_size(QoS) of
	    3  -> ["98", Value0];
	    11 -> ["99", Value0];
	    14 -> ["05", Value0];
	    _  -> ["07", Value0]
	end,
    [{?X_3GPP_GPRS_Negotiated_QoS_profile, Value}|Acc];


session_options('3GPP-SGSN-Address', Value, Acc) ->
    [{?X_3GPP_SGSN_Address, Value}|Acc];
session_options('3GPP-GGSN-Address', Value, Acc) ->
    [{?X_3GPP_GGSN_Address, Value}|Acc];
session_options('3GPP-IMSI-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_IMSI_MCC_MNC, Value}|ACC];
session_options('3GPP-GGSN-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_GGSN_MCC_MNC, Value}|ACC];
session_options('3GPP-NSAPI', Value, ACC) ->
    [{?X_3GPP_NSAPI, io_lib:format("~1.16B", [Value])}|ACC];
session_options('3GPP-Session-Stop-Indicator', Value, Acc) ->
    [{?X_3GPP_Session_Stop_Indicator, Value}|Acc];
session_options('3GPP-Selection-Mode', Value, Acc) ->
    [{?X_3GPP_Selection_Mode, io_lib:format("~1.16B", [Value])}|Acc];
session_options('3GPP-Charging-Characteristics', Value, Acc) ->
    Hex = [ io_lib:format("~2.16.0B", [X]) || <<X>> <= Value ],
    [{?X_3GPP_Charging_Characteristics, Hex}|Acc];
session_options('3GPP-Charging-Gateway-IPv6-Address', Value, Acc) ->
    [{?X_3GPP_Charging_Gateway_IPv6_Address, Value}|Acc];
session_options('3GPP-SGSN-IPv6-Address', Value, Acc) ->
    [{?X_3GPP_SGSN_IPv6_Address, Value}|Acc];
session_options('3GPP-GGSN-IPv6-Address', Value, Acc) ->
    [{?X_3GPP_GGSN_IPv6_Address, Value}|Acc];
session_options('3GPP-IPv6-DNS-Servers', Value, Acc) ->
    [{?X_3GPP_IPv6_DNS_Servers, Value}|Acc];
session_options('3GPP-SGSN-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_SGSN_MCC_MNC, Value}|ACC];
session_options('3GPP-Teardown-Indicator', Value, Acc) ->
    [{?X_3GPP_Teardown_Indicator, Value}|Acc];
session_options('3GPP-IMEISV', Value, ACC) ->
    [{?X_3GPP_IMEISV, Value}|ACC];
session_options('3GPP-RAT-Type', Value, Acc) ->
    [{?X_3GPP_RAT_Type, Value}|Acc];
session_options('3GPP-User-Location-Info', Value, Acc) ->
    [{?X_3GPP_User_Location_Info, Value}|Acc];
session_options('3GPP-MS-TimeZone', {TZ, DST}, Acc) ->
    [{?X_3GPP_MS_TimeZone, <<TZ:8, DST:8>>}|Acc];
session_options('3GPP-Camel-Charging', Value, Acc) ->
    [{?X_3GPP_Camel_Charging, Value}|Acc];
session_options('3GPP-Packet-Filter', Value, Acc) ->
    [{?X_3GPP_Packet_Filter, Value}|Acc];
session_options('3GPP-Negotiated-DSCP', Value, Acc) ->
    [{?X_3GPP_Negotiated_DSCP, Value}|Acc];

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
session_opt(Fun, {Verdict, Opts, State}) ->
    {Verdict, Fun(Opts), State}.
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

process_gen_attrs({#attribute{id = ?Acct_Interim_Interval}, Interim}, Acc) ->
    session_opt(
      ergw_aaa_session:trigger(session, 'IP-CAN', time, Interim * 1000, [recurring], _), Acc);

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
%% Framed-Protocol = GPRS-PDP-Context
process_gen_attrs({#attribute{id = ?Framed_Protocol}, 7}, Acc) ->
    session_opt('Framed-Protocol', 'GPRS-PDP-Context', Acc);
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

%% 3GPP TS 29.061 Attributes

%% TODO: 3GPP-Ipv6-DNSServers

%% Microsoft MPPE Keys

%% MS-MPPE-Send-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Send_Key}, Value}, Acc) ->
    session_opt('MS-MPPE-Send-Key', Value, Acc);

%% MS-MPPE-Recv-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Recv_Key}, Value}, Acc) ->
    session_opt('MS-MPPE-Recv-Key', Value, Acc);

%% MS-Primary-DNS-Server
process_gen_attrs({#attribute{id = ?MS_Primary_DNS_Server}, Value}, Acc) ->
    session_opt('MS-Primary-DNS-Server', Value, Acc);

%% MS-Secondary-DNS-Server
process_gen_attrs({#attribute{id = ?MS_Secondary_DNS_Server}, Value}, Acc) ->
    session_opt('MS-Secondary-DNS-Server', Value, Acc);

%% MS-Primary-NBNS-Server
process_gen_attrs({#attribute{id = ?MS_Primary_NBNS_Server}, Value}, Acc) ->
    session_opt('MS-Primary-NBNS-Server', Value, Acc);

%% MS-Secondary-NBNS-Server
process_gen_attrs({#attribute{id = ?MS_Secondary_NBNS_Server}, Value}, Acc) ->
    session_opt('MS-Secondary-NBNS-Server', Value, Acc);

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
process_gen_attrs({#attribute{id = ?TP_Max_Input_Octets}, Value}, Acc) ->
    session_opt('Max-Input-Octets', Value, Acc);

%% TP-Max-Output-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Output_Octets}, Value}, Acc) ->
    session_opt('Max-Output-Octets', Value, Acc);

%% TP-Max-Total-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Total_Octets}, Value}, Acc) ->
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
