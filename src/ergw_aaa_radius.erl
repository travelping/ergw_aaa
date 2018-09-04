%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_radius).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/5]).

-import(ergw_aaa_session, [attr_append/3, merge/2]).

-include("ergw_aaa_internal.hrl").

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

-define(DefaultOptions, [{server, undefined}
			]).

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    eradius_dict:load_tables([dictionary,
			      dictionary_3gpp,
			      dictionary_tunnel,
			      dictionary_rfc4679,
			      dictionary_alcatel_sr,
			      dictionary_microsoft,
			      dictionary_travelping]),
    {ok, []}.

initialize_service(_ServiceId, _Opts) ->
    {ok, []}.

validate_handler(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions, map).

validate_service(_Service, HandlerOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, HandlerOpts, map).

validate_procedure(_Application, _Procedure, _Service, ServiceOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ServiceOpts, map).

session_auth_options('PAP', Session, Attrs) ->
    [{?User_Password , maps:get('Password', Session, <<>>)} | Attrs];
session_auth_options(_, _Session, Attrs) ->
    Attrs.

invoke(_Service, init, Session, Events, _Opts) ->
    {ok, Session, Events};

invoke(_Service, authenticate, Session, Events, Opts) ->
    RadiusSession = maps:get(?MODULE, Session, #{}),

    UserName0 = maps:get('Username', Session, <<>>),
    UserName = maps:get('Username', RadiusSession, UserName0),

    Attrs0 = [{?User_Name,      UserName}],
    Attrs1 = session_auth_options(
	       maps:get('Authentication-Method', Session, 'PAP'), Session, Attrs0),
    Attrs2 = radius_session_options(RadiusSession, Attrs1),
    Attrs3 = session_options(Session, Attrs2),
    Attrs = remove_accounting_attrs(Attrs3),

    Req = #radius_request{
	     cmd = request,
	     attrs = Attrs,
	     msg_hmac = true,
	     eap_msg = maps:get('EAP-Data', Session, <<>>)},

    {Verdict, SessionOpts} =
	radius_response(send_request(Req, Session, Opts), Opts, Session, Events),
    case Verdict of
	success ->
	    {ok, SessionOpts#{'Authentication-Result' => Verdict}};
	_ ->
	    {denied, SessionOpts#{'Authentication-Result' => Verdict}}
    end;

invoke(_Service, authorize, #{'Authentication-Result' := success} = Session, Events, _) ->
    {ok, Session, Events};

invoke(_Service, authorize, Session, Events, _) ->
    {denied, Session, Events};

invoke(_Service, start, Session0, Events, Opts) ->
    RadiusSession0 = ergw_aaa_session:get_svc_opt(?MODULE, Session0),
    case maps:get('State', RadiusSession0, stopped) of
	stopped ->
	    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
	    Session = maps:without(Keys, Session0),
	    RadiusSession = RadiusSession0#{'State' => 'started'},
	    accounting(?RStatus_Type_Start, [], Session, Events, RadiusSession, Opts);
	_ ->
	    {ok, Session0, Events}
    end;

invoke(_Service, interim, Session, Events, Opts) ->
    RadiusSession = ergw_aaa_session:get_svc_opt(?MODULE, Session),
    case maps:get('State', RadiusSession, stopped) of
	started ->
	    accounting(?RStatus_Type_Update, [], Session, Events, RadiusSession, Opts);
	_ ->
	    {ok, Session, Events}
    end;

invoke(_Service, stop, Session, Events, Opts) ->
    RadiusSession0 = ergw_aaa_session:get_svc_opt(?MODULE, Session),
    case maps:get('State', RadiusSession0, stopped) of
	started ->
	    RadiusSession = RadiusSession0#{'State' => 'stopped'},
	    accounting(?RStatus_Type_Stop, [], Session, Events, RadiusSession, Opts);
	_ ->
	    {ok, Session, Events}
    end;

invoke(Service, Procedure, Session, Events, _Opts) ->
    {{error, {Service, Procedure}}, Session, Events}.

accounting(Type, Attrs0, Session0, Events, RadiusSession, Opts) ->
    UserName0 = maps:get('Username', Session0, <<>>),
    UserName = maps:get('Username', RadiusSession, UserName0),

    Attrs1 = radius_session_options(RadiusSession, Attrs0),
    Attrs2 = session_options(Session0, Attrs1),
    Attrs = [{?RStatus_Type,   Type},
	     {?User_Name,      UserName}
	     | Attrs2],
    Req = #radius_request{cmd = accreq, attrs = Attrs, msg_hmac = false},
    send_request(Req, Session0, Opts),

    Session = ergw_aaa_session:set_svc_opt(?MODULE, RadiusSession, Session0),
    {ok, Session, Events}.

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

validate_option(server, Value) ->
    validate_server_spec(server, Value);
validate_option(Opt, Value)
  when (Opt =:= server_name orelse Opt =:= client_name)
       andalso is_binary(Value) ->
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

radius_session_options(RadiusSession, Attrs) ->
    maps:fold(fun session_options/3, Attrs, RadiusSession).

session_options(Session, Attrs) ->
    maps:fold(fun session_options/3, Attrs, Session).

session_options('NAS-Identifier', Value, Attrs) ->
    [{?NAS_Identifier, Value}|Attrs];

session_options('IP', Value, Attrs) ->
    [{?Framed_IP_Address, Value}|Attrs];

session_options('Service-Type', Value, Attrs) ->
    [{?Service_Type, service_type(Value)}|Attrs];
session_options('Framed-Protocol', Value, Attrs) ->
    [{?Framed_Protocol, framed_protocol(Value)}|Attrs];

session_options('Framed-IP-Address', Value, Attrs) ->
    [{?Framed_IP_Address, Value}|Attrs];
session_options('Framed-Interface-Id', Value, Attrs) ->
    [{?Framed_Interface_Id, Value}|Attrs];
session_options('Session-Id', Value, Attrs) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Session_Id, Id}|Attrs];
session_options('Multi-Session-Id', Value, Attrs) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Multi_Session_Id, Id}|Attrs];
session_options('Class', Class, Attrs) ->
    [{?Class, Class}|Attrs];
session_options('RADIUS-State', State, Attrs) ->
    [{?State, State}|Attrs];
session_options('Calling-Station-Id', Value, Attrs) ->
    [{?Calling_Station_Id, Value}|Attrs];
session_options('Called-Station-Id', Value, Attrs) ->
    [{?Called_Station_Id, Value}|Attrs];
session_options('Port-Id', Value, Attrs) ->
    [{?NAS_Port_Id, Value}|Attrs];
session_options('NAS-IP-Address', Value, Acc) ->
    [{?NAS_IP_Address, Value}|Acc];

session_options('Acct-Session-Time', Time, Attrs) ->
    [{?Acct_Session_Time, Time}|Attrs];

session_options('InOctets', Octets, Attrs) when is_integer(Octets) ->
    [{?Acct_Input_Octets, Octets}, {?Acct_Input_Gigawords, Octets bsr 32}|Attrs];
session_options('InPackets', Packets, Attrs) when is_integer(Packets) ->
    [{?Acct_Input_Packets, Packets}|Attrs];
session_options('OutOctets', Octets, Attrs) when is_integer(Octets) ->
    [{?Acct_Output_Octets, Octets}, {?Acct_Output_Gigawords, Octets bsr 32}|Attrs];
session_options('OutPackets', Packets, Attrs) when is_integer(Packets) ->
    [{?Acct_Output_Packets, Packets}|Attrs];

%% Only Session level monitoring for now
session_options(monitors, #{'IP-CAN' := #{?MODULE := Monitor}}, Attrs) ->
    maps:fold(fun session_options/3, Attrs, Monitor);

session_options('Event-Timestamp', Value, Attrs) ->
    [{?Event_Timestamp, Value}|Attrs];

session_options('Port-Type', Value, Attrs) ->
    [{?NAS_Port_Type, port_type(Value)}|Attrs];

session_options(Type, Value, Attrs)
  when is_list(Value) andalso
       (Type == 'Tunnel-Type' orelse
	Type == 'Tunnel-Medium-Type' orelse
	Type == 'Tunnel-Client-Endpoint') ->
    lists:foldl(fun(V, A) -> session_options(Type, V, A) end, Attrs, Value);

session_options('Tunnel-Type', {Tag, Type}, Attrs) ->
    [{?Tunnel_Type, {Tag, tunnel_type(Type)}}|Attrs];
session_options('Tunnel-Type', Type, Attrs) ->
    [{?Tunnel_Type, tunnel_type(Type)}|Attrs];

session_options('Tunnel-Medium-Type', {Tag, Type}, Attrs) ->
    [{?Tunnel_Medium_Type, {Tag, tunnel_medium_type(Type)}}|Attrs];
session_options('Tunnel-Medium-Type', Type, Attrs) ->
    [{?Tunnel_Medium_Type, tunnel_medium_type(Type)}|Attrs];

session_options('Tunnel-Client-Endpoint', {Tag, Value}, Attrs) ->
    [{?Tunnel_Client_Endpoint, {Tag, Value}}|Attrs];
session_options('Tunnel-Client-Endpoint', Value, Attrs) ->
    [{?Tunnel_Client_Endpoint, Value}|Attrs];

session_options('Acct-Authentic', 'RADIUS', Attrs) ->
    [{?Acct_Authentic, 1}|Attrs];
session_options('Acct-Authentic', 'Local', Attrs) ->
    [{?Acct_Authentic, 2}|Attrs];
session_options('Acct-Authentic', 'Remote', Attrs) ->
    [{?Acct_Authentic, 3}|Attrs];
session_options('Acct-Authentic', 'Diameter', Attrs) ->
    [{?Acct_Authentic, 4}|Attrs];

%% 3GPP TS 29.061 Attributes
session_options('3GPP-IMSI', Value, ACC) ->
    [{?X_3GPP_IMSI, Value}|ACC];
session_options('3GPP-Charging-Id', Value, Attrs) ->
    [{?X_3GPP_Charging_ID, Value}|Attrs];

session_options('3GPP-PDP-Type', 'IPv4', Attrs) ->
    [{?X_3GPP_PDP_Type, 0}|Attrs];
session_options('3GPP-PDP-Type', 'IPv6', Attrs) ->
    [{?X_3GPP_PDP_Type, 2}|Attrs];
session_options('3GPP-PDP-Type', 'IPv4v6', Attrs) ->
    [{?X_3GPP_PDP_Type, 3}|Attrs];
session_options('3GPP-PDP-Type', 'PPP', Attrs) ->
    [{?X_3GPP_PDP_Type, 1}|Attrs];
session_options('3GPP-PDP-Type', 'Non-IP', Attrs) ->
    [{?X_3GPP_PDP_Type, 4}|Attrs];

session_options('3GPP-Charging-Gateway-Address', Value, Attrs) ->
    [{?X_3GPP_Charging_Gateway_Address, Value}|Attrs];
session_options('3GPP-GPRS-Negotiated-QoS-Profile', QoS, Attrs) ->
    Value0 = iolist_to_binary([io_lib:format("~2.16.0B", [X]) || <<X>> <= QoS ]),
    Value =
	case byte_size(QoS) of
	    3  -> ["98", Value0];
	    11 -> ["99", Value0];
	    14 -> ["05", Value0];
	    _  -> ["07", Value0]
	end,
    [{?X_3GPP_GPRS_Negotiated_QoS_profile, Value}|Attrs];


session_options('3GPP-SGSN-Address', Value, Attrs) ->
    [{?X_3GPP_SGSN_Address, Value}|Attrs];
session_options('3GPP-GGSN-Address', Value, Attrs) ->
    [{?X_3GPP_GGSN_Address, Value}|Attrs];
session_options('3GPP-IMSI-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_IMSI_MCC_MNC, Value}|ACC];
session_options('3GPP-GGSN-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_GGSN_MCC_MNC, Value}|ACC];
session_options('3GPP-NSAPI', Value, ACC) ->
    [{?X_3GPP_NSAPI, io_lib:format("~1.16B", [Value])}|ACC];
session_options('3GPP-Session-Stop-Indicator', Value, Attrs) ->
    [{?X_3GPP_Session_Stop_Indicator, Value}|Attrs];
session_options('3GPP-Selection-Mode', Value, Attrs) ->
    [{?X_3GPP_Selection_Mode, io_lib:format("~1.16B", [Value])}|Attrs];
session_options('3GPP-Charging-Characteristics', Value, Attrs) ->
    Hex = [ io_lib:format("~2.16.0B", [X]) || <<X>> <= Value ],
    [{?X_3GPP_Charging_Characteristics, Hex}|Attrs];
session_options('3GPP-Charging-Gateway-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_Charging_Gateway_IPv6_Address, Value}|Attrs];
session_options('3GPP-SGSN-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_SGSN_IPv6_Address, Value}|Attrs];
session_options('3GPP-GGSN-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_GGSN_IPv6_Address, Value}|Attrs];
session_options('3GPP-IPv6-DNS-Servers', Value, Attrs) ->
    [{?X_3GPP_IPv6_DNS_Servers, Value}|Attrs];
session_options('3GPP-SGSN-MCC-MNC', Value, ACC) ->
    [{?X_3GPP_SGSN_MCC_MNC, Value}|ACC];
session_options('3GPP-Teardown-Indicator', Value, Attrs) ->
    [{?X_3GPP_Teardown_Indicator, Value}|Attrs];
session_options('3GPP-IMEISV', Value, ACC) ->
    [{?X_3GPP_IMEISV, Value}|ACC];
session_options('3GPP-RAT-Type', Value, Attrs) ->
    [{?X_3GPP_RAT_Type, Value}|Attrs];
session_options('3GPP-User-Location-Info', Value, Attrs) ->
    [{?X_3GPP_User_Location_Info, Value}|Attrs];
session_options('3GPP-MS-TimeZone', {TZ, DST}, Attrs) ->
    [{?X_3GPP_MS_TimeZone, <<TZ:8, DST:8>>}|Attrs];
session_options('3GPP-Camel-Charging', Value, Attrs) ->
    [{?X_3GPP_Camel_Charging, Value}|Attrs];
session_options('3GPP-Packet-Filter', Value, Attrs) ->
    [{?X_3GPP_Packet_Filter, Value}|Attrs];
session_options('3GPP-Negotiated-DSCP', Value, Attrs) ->
    [{?X_3GPP_Negotiated_DSCP, Value}|Attrs];

%% DSL-Forum PPPoE Intermediate Agent Attributes
session_options('ADSL-Agent-Circuit-Id', Value, Attrs) ->
    [{?ADSL_Agent_Circuit_Id, Value}|Attrs];
session_options('ADSL-Agent-Remote-Id', Value, Attrs) ->
    [{?ADSL_Agent_Remote_Id, Value}|Attrs];
session_options('Actual-Data-Rate-Upstream', Value, Attrs) ->
    [{?Actual_Data_Rate_Upstream, Value}|Attrs];
session_options('Actual-Data-Rate-Downstream', Value, Attrs) ->
    [{?Actual_Data_Rate_Downstream, Value}|Attrs];
session_options('Minimum-Data-Rate-Upstream', Value, Attrs) ->
    [{?Minimum_Data_Rate_Upstream, Value}|Attrs];
session_options('Minimum-Data-Rate-Downstream', Value, Attrs) ->
    [{?Minimum_Data_Rate_Downstream, Value}|Attrs];
session_options('Attainable-Data-Rate-Upstream', Value, Attrs) ->
    [{?Attainable_Data_Rate_Upstream, Value}|Attrs];
session_options('Attainable-Data-Rate-Downstream', Value, Attrs) ->
    [{?Attainable_Data_Rate_Downstream, Value}|Attrs];
session_options('Maximum-Data-Rate-Upstream', Value, Attrs) ->
    [{?Maximum_Data_Rate_Upstream, Value}|Attrs];
session_options('Maximum-Data-Rate-Downstream', Value, Attrs) ->
    [{?Maximum_Data_Rate_Downstream, Value}|Attrs];
session_options('Minimum-Data-Rate-Upstream-Low-Power', Value, Attrs) ->
    [{?Minimum_Data_Rate_Upstream_Low_Power, Value}|Attrs];
session_options('Minimum-Data-Rate-Downstream-Low-Power', Value, Attrs) ->
    [{?Minimum_Data_Rate_Downstream_Low_Power, Value}|Attrs];
session_options('Maximum-Interleaving-Delay-Upstream', Value, Attrs) ->
    [{?Maximum_Interleaving_Delay_Upstream, Value}|Attrs];
session_options('Actual-Interleaving-Delay-Upstream', Value, Attrs) ->
    [{?Actual_Interleaving_Delay_Upstream, Value}|Attrs];
session_options('Maximum-Interleaving-Delay-Downstream', Value, Attrs) ->
    [{?Maximum_Interleaving_Delay_Downstream, Value}|Attrs];
session_options('Actual-Interleaving-Delay-Downstream', Value, Attrs) ->
    [{?Actual_Interleaving_Delay_Downstream, Value}|Attrs];

session_options('Authentication-Method', {'TLS', 'Pre-Shared-Key'}, Attrs) ->
    [{?TP_TLS_Auth_Type, 0}|Attrs];
session_options('Authentication-Method', {'TLS', 'X509-Subject-CN'}, Attrs) ->
    [{?TP_TLS_Auth_Type, 1}|Attrs];

%% Travelping Extension
session_options('Zone-Id', Value, Attrs) ->
    [{?TP_Zone_Id, Value}|Attrs];
session_options('Location-Id', Value, Attrs) ->
    [{?TP_Location_Id, Value}|Attrs];
session_options('Access-Group', Value, Attrs) ->
    [{?TP_Access_Group, list_to_binary(Value)}|Attrs];
session_options('Access-Class-Id', Value, Attrs) ->
    [{?TP_Access_Class_Id, list_to_binary(Value)}|Attrs];
session_options('NAT-Pool-Id', Value, Attrs) ->
    [{?TP_NAT_Pool_Id, Value}|Attrs];
session_options('NAT-IP-Address', Value, Attrs) ->
    [{?TP_NAT_IP_Address, Value}|Attrs];
session_options('NAT-Port-Start', Value, Attrs) ->
    [{?TP_NAT_Port_Start, Value}|Attrs];
session_options('NAT-Port-End', Value, Attrs) ->
    [{?TP_NAT_Port_End, Value}|Attrs];

session_options('DHCP-Parameter-Request-List', Value, Attrs) ->
    [{?TP_DHCP_Parameter_Request_List, Value}|Attrs];
session_options('DHCP-Request-Option-List', Value, Attrs) ->
    [{?TP_DHCP_Request_Option_List, Value}|Attrs];

%% TP CAPWAP extensions - Versions

session_options('CAPWAP-WTP-Version', Value, Attrs) ->
    [{?TP_CAPWAP_WTP_Version, Value}|Attrs];
session_options('CAPWAP-Hardware-Version', Version, Attrs) ->
    [{?TP_CAPWAP_Hardware_Version, Version}|Attrs];
session_options('CAPWAP-Software-Version', Version, Attrs) ->
    [{?TP_CAPWAP_Software_Version, Version}|Attrs];
session_options('CAPWAP-Boot-Version', Version, Attrs) ->
    [{?TP_CAPWAP_Boot_Version, Version}|Attrs];
session_options('CAPWAP-Other-Software-Version', Version, Attrs) ->
    [{?TP_CAPWAP_Other_Software_Version, Version}|Attrs];

%% TP CAPWAP extensions - generic

session_options('CAPWAP-Timestamp', Value, Attrs) ->
    [{?TP_CAPWAP_Timestamp, Value}|Attrs];
session_options('CAPWAP-Session-Id', Value, Attrs) ->
    [{?TP_CAPWAP_Session_Id, Value}|Attrs];
session_options('CAPWAP-Radio-Id', Value, Attrs) ->
    [{?TP_CAPWAP_Radio_Id, Value}|Attrs];

%% TP CAPWAP extensions - statistics

session_options('CAPWAP-WWAN-Id', Value, Attrs) ->
    [{?TP_CAPWAP_WWAN_Id, Value}|Attrs];
session_options('CAPWAP-WWAN-RAT', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_RAT, VALUE}|ACC];
session_options('CAPWAP-WWAN-RSSi', Value, Attrs) ->
    [{?TP_CAPWAP_WWAN_RSSi, Value}|Attrs];
session_options('CAPWAP-WWAN-CREG', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_CREG, VALUE}|ACC];
session_options('CAPWAP-WWAN-LAC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_LAC, VALUE}|ACC];
session_options('CAPWAP-WWAN-Latency', Value, Attrs) ->
    [{?TP_CAPWAP_WWAN_Latency, Value}|Attrs];
session_options('CAPWAP-WWAN-MCC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_MCC, VALUE}|ACC];
session_options('CAPWAP-WWAN-MNC', VALUE, ACC) ->
    [{?TP_CAPWAP_WWAN_MNC, VALUE}|ACC];
session_options('CAPWAP-WWAN-Cell-Id', Value, Attrs) ->
    [{?TP_CAPWAP_WWAN_Cell_Id, Value}|Attrs];

%% TP CAPWAP extensions - GPSATC

session_options('CAPWAP-GPS-Timestamp', Value, Attrs) ->
    [{?TP_CAPWAP_GPS_Timestamp, Value}| Attrs];
session_options('CAPWAP-GPS-Latitude', Value, Attrs) ->
    [{?TP_CAPWAP_GPS_Latitude, Value} | Attrs];
session_options('CAPWAP-GPS-Longitude', Value, Attrs) ->
    [{?TP_CAPWAP_GPS_Longitude, Value} | Attrs];
session_options('CAPWAP-GPS-Altitude', Value, Attrs) ->
    [{?TP_CAPWAP_GPS_Altitude, Value} | Attrs];
session_options('CAPWAP-GPS-Hdop', Value, Attrs) ->
    [{?TP_CAPWAP_GPS_Hdop, Value} | Attrs];

session_options('TP-Trace-Id', Value, Acc) ->
    [{?TP_Trace_Id, Value} | Acc];

session_options(_Key, _Value, Attrs) ->
    Attrs.

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

radius_response({ok, Response, RequestAuthenticator}, #{server := {_, _, Secret}},
		Session, Events) ->
    radius_reply(
      eradius_lib:decode_request(Response, Secret, RequestAuthenticator), Session, Events);
radius_response(Response, _, Session, Events) ->
    lager:error("RADIUS failed with ~p", [Response]),
    {fail, Session, Events}.

radius_reply(#radius_request{cmd = accept} = Reply, Session0, Events0) ->
    lager:debug("RADIUS Reply: ~p", [Reply]),
    try
	{Session1, Events} = process_radius_attrs(Reply, Session0, Events0),
	Session2 = Session1#{'Acct-Authentic' => 'RADIUS'},
	Session = handle_eap_msg(Reply, Session2),
	{ok, Session, Events}
    catch
	throw:#aaa_err{} = _CtxErr ->
	    {fail, Session0, Events0}
    end;

radius_reply(#radius_request{cmd = challenge} = Reply, Session0, Events0) ->
    lager:debug("RADIUS Challenge: ~p", [lager:pr(Reply, ?MODULE)]),
    try
	{Session1, Events} = process_radius_attrs(Reply, Session0, Events0),
	Session = handle_eap_msg(Reply, Session1),
	{ok, Session, Events}
    catch
	throw:#aaa_err{} = _CtxErr ->
	    {fail, Session0, Events0}
    end;

radius_reply(#radius_request{cmd = reject} = Reply, Session0, Events) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    Session = handle_eap_msg(Reply, Session0),
    {fail, Session, Events};

radius_reply(Reply, Session, Events) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    {fail, Session, Events}.

handle_eap_msg(#radius_request{eap_msg = EAP}, Session)
  when EAP /= <<>> ->
    Session#{'EAP-Data' => EAP};
handle_eap_msg(_, Session) ->
    Session.

%% iterate over the RADIUS attributes
process_radius_attrs(#radius_request{attrs = Attrs}, Session, Events) ->
    lists:foldr(fun process_gen_attrs/2, {Session, Events}, Attrs).

%% verdict(Verdict, {_, Opts, State}) ->
%%     {Verdict, Opts, State}.
%% session_opt(Fun, {Verdict, Opts, State}) ->
%%     {Verdict, Fun(Opts), State}.
session_opt(Key, Opt, {Session, Events}) ->
    {maps:put(Key, Opt, Session), Events}.
session_opt_append(Key, Opt, {Session, Events}) ->
    {attr_append(Key, Opt, Session), Events}.

radius_session_opt(Key, Opt, {Session, Events}) ->
    RadiusSession = maps:get(?MODULE, Session, #{}),
    {maps:put(?MODULE, maps:put(Key, Opt, RadiusSession), Session), Events}.

%% Class
process_gen_attrs({#attribute{id = ?Class}, Class}, Session) ->
    radius_session_opt('Class', Class, Session);

%% State
process_gen_attrs({#attribute{id = ?State}, State}, Session) ->
    radius_session_opt('RADIUS-State', State, Session);

%% User-Name
process_gen_attrs({#attribute{id = ?User_Name}, UserName}, Session) ->
    radius_session_opt('Username', UserName, Session);

process_gen_attrs({#attribute{id = ?Acct_Interim_Interval}, Interim}, {Session, Events}) ->
    Monit = {'IP-CAN', periodic, Interim},
    Trigger = ergw_aaa_session:trigger(?MODULE, 'IP-CAN', periodic, Interim),
    {maps:update_with(monitoring, maps:put(?MODULE, Monit, _), #{?MODULE => Monit}, Session),
     ergw_aaa_session:ev_set(Trigger, Events)};

%% Session-Timeout
process_gen_attrs({#attribute{id = ?Session_Timeout}, TimeOut}, Session) ->
    session_opt('Session-Timeout', TimeOut * 1000, Session);

%% Idle-Timeout
process_gen_attrs({#attribute{id = ?Idle_Timeout}, TimeOut}, Session) ->
    session_opt('Idle-Timeout', TimeOut * 1000, Session);

%% Service-Type = Framed-User
process_gen_attrs({#attribute{id = ?Service_Type}, 2}, Session) ->
    Session;
process_gen_attrs({#attribute{id = ?Service_Type, name = Name}, Value}, _Session) ->
    lager:debug("unexpected Value in AVP: ~s: ~p", [Name, Value]),
    throw(?AAA_ERR(?FATAL));

%% Framed-Protocol = PPP
process_gen_attrs({#attribute{id = ?Framed_Protocol}, 1}, Session) ->
    Session;
%% Framed-Protocol = GPRS-PDP-Context
process_gen_attrs({#attribute{id = ?Framed_Protocol}, 7}, Session) ->
    session_opt('Framed-Protocol', 'GPRS-PDP-Context', Session);
process_gen_attrs({#attribute{id = ?Framed_Protocol, name = Name}, Value}, _Session) ->
    lager:debug("unexpected Value in AVP: ~s: ~p", [Name, Value]),
    throw(?AAA_ERR(?FATAL));

%% Framed-IP-Address = xx.xx.xx.xx
process_gen_attrs({#attribute{id = ?Framed_IP_Address}, IP}, Session) ->
    session_opt('Framed-IP-Address', IP, Session);

%% Framed-Interface-Id
process_gen_attrs({#attribute{id = ?Framed_Interface_Id}, Id}, Session) ->
    session_opt('Framed-Interface-Id', Id, Session);

%% Alc-Primary-Dns
process_gen_attrs({#attribute{id = ?Alc_Primary_Dns}, DNS}, Session) ->
    session_opt_append('DNS', DNS, Session);
%% Alc-Secondary-Dns
process_gen_attrs({#attribute{id = ?Alc_Secondary_Dns}, DNS}, Session) ->
    session_opt_append('DNS', DNS, Session);

%% 3GPP TS 29.061 Attributes

%% TODO: 3GPP-Ipv6-DNSServers

%% Microsoft MPPE Keys

%% MS-MPPE-Send-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Send_Key}, Value}, Session) ->
    session_opt('MS-MPPE-Send-Key', Value, Session);

%% MS-MPPE-Recv-Key
process_gen_attrs({#attribute{id = ?MS_MPPE_Recv_Key}, Value}, Session) ->
    session_opt('MS-MPPE-Recv-Key', Value, Session);

%% MS-Primary-DNS-Server
process_gen_attrs({#attribute{id = ?MS_Primary_DNS_Server}, Value}, Session) ->
    session_opt('MS-Primary-DNS-Server', Value, Session);

%% MS-Secondary-DNS-Server
process_gen_attrs({#attribute{id = ?MS_Secondary_DNS_Server}, Value}, Session) ->
    session_opt('MS-Secondary-DNS-Server', Value, Session);

%% MS-Primary-NBNS-Server
process_gen_attrs({#attribute{id = ?MS_Primary_NBNS_Server}, Value}, Session) ->
    session_opt('MS-Primary-NBNS-Server', Value, Session);

%% MS-Secondary-NBNS-Server
process_gen_attrs({#attribute{id = ?MS_Secondary_NBNS_Server}, Value}, Session) ->
    session_opt('MS-Secondary-NBNS-Server', Value, Session);

%% Travelping Extensions

%% TP-Access-Rule
process_gen_attrs({#attribute{id = ?TP_Access_Rule}, Value}, Session) ->
    Rule = list_to_tuple([binary_to_list(V) || V <- binary:split(Value, <<":">>, [global])]),
    session_opt_append('Access-Rules', Rule, Session);

%% TP-Access-Group
process_gen_attrs({#attribute{id = ?TP_Access_Group}, Value}, Session) ->
    session_opt('Access-Group', binary_to_list(Value), Session);

%% TP-NAT-Pool-Id
process_gen_attrs({#attribute{id = ?TP_NAT_Pool_Id}, Value}, Session) ->
    session_opt('NAT-Pool-Id', Value, Session);

%% TP-NAT-IP-Address
process_gen_attrs({#attribute{id = ?TP_NAT_IP_Address}, Value}, Session) ->
    session_opt('NAT-IP-Address', Value, Session);

%% TP-NAT-Port-Start
process_gen_attrs({#attribute{id = ?TP_NAT_Port_Start}, Value}, Session) ->
    session_opt('NAT-Port-Start', Value, Session);

%% TP-NAT-Port-End
process_gen_attrs({#attribute{id = ?TP_NAT_Port_End}, Value}, Session) ->
    session_opt('NAT-Port-End', Value, Session);

%% TP-Max-Input-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Input_Octets}, Value}, Acc) ->
    session_opt('Max-Input-Octets', Value, Acc);

%% TP-Max-Output-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Output_Octets}, Value}, Session) ->
    session_opt('Max-Output-Octets', Value, Session);

%% TP-Max-Total-Octets
process_gen_attrs({#attribute{id = ?TP_Max_Total_Octets}, Value}, Acc) ->
    session_opt('Max-Total-Octets', Value, Acc);

%% TP-TLS-Pre-Shared-Key
process_gen_attrs({#attribute{id = ?TP_TLS_Pre_Shared_Key}, PSK}, Session) ->
    session_opt('TLS-Pre-Shared-Key', PSK, Session);

%% CAPWAP LocationProfile attributes
process_gen_attrs({#attribute{id = ?TP_CAPWAP_SSID}, SSID}, Session) ->
    session_opt('CAPWAP-SSID', SSID, Session);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Max_WIFI_Clients}, MaxClients}, Session) ->
    session_opt('CAPWAP-Max-WIFI-Clients', MaxClients, Session);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Power_Save_Idle_Timeout}, WG}, Session) ->
    session_opt('CAPWAP-Power-Save-Idle-Timeout', WG, Session);

process_gen_attrs({#attribute{id = ?TP_CAPWAP_Power_Save_Busy_Timeout}, WG}, Session) ->
    session_opt('CAPWAP-Power-Save-Busy-Timeout', WG, Session);

%% Handling undefined cases
process_gen_attrs({#attribute{name = Name}, Value} , Session) ->
    lager:debug("unhandled reply AVP: ~s: ~p", [Name, Value]),
    Session;

process_gen_attrs({Attr, Value}, Session) ->
    lager:debug("unhandled undecoded reply AVP: ~w: ~p", [Attr, Value]),
    Session.

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
		       ?Acct_Status_Type,
		       ?Event_Timestamp]).

send_request(Req, Session, #{server := NAS} = Opts) ->
    Id = maps:get('NAS-Identifier', Session, <<"NAS">>),
    RadiusClientOpts = [{client_name, maps:get(client_name, Opts, Id)},
			{server_name, maps:get(server_name, Opts, Id)}],
    eradius_client:send_request(NAS, Req, RadiusClientOpts).
