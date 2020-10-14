%% Copyright 2016-2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_radius).

-compile({parse_transform, cut}).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/6, handle_response/6]).
-export([to_session/3]).
-export([get_state_atom/1]).

-include("ergw_aaa_internal.hrl").
-include("include/ergw_aaa_session.hrl").

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_3gpp.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_rfc6911.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_microsoft.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

%% RFC: https://tools.ietf.org/html/rfc2866#section-5.10
-define(DEFAULT_TERMINATION_CAUSE_MAPPING, [
    {normal, 1},
    {administrative, 6},
    {link_broken, 2},
    {upf_failure, 2},
    {session_error, 2},
    {inactivity_timeout, 4},
    {path_restart, 2}
]).

-define(DefaultOptions, [{server, undefined},
			 {timeout, 5000},
			 {retries, 3},
			 {termination_cause_mapping, []}
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

invoke(_Service, init, Session, Events, _Opts, _) ->
    State = #{'State' => stopped},
    {ok, Session, Events, State};

invoke(_Service, authenticate = Procedure, Session0, Events0, #{now := Now} = Opts, State0) ->
    UserName0 = maps:get('Username', Session0, <<>>),
    UserName = maps:get('Username', State0, UserName0),

    Attrs0 = [{?User_Name, UserName},
	      {?Event_Timestamp,
	       system_time_to_universal_time(Now + erlang:time_offset(), native)}],
    Attrs1 = session_auth_options(
	       maps:get('Authentication-Method', Session0, 'PAP'), Session0, Attrs0),
    Attrs2 = radius_session_options(State0, Attrs1),
    Attrs3 = session_options(Session0, Attrs2),
    Attrs = remove_accounting_attrs(Attrs3),

    Req = #radius_request{
	     cmd = request,
	     attrs = Attrs,
	     msg_hmac = true,
	     eap_msg = maps:get('EAP-Data', Session0, <<>>)},

    {Verdict, Session, Events, State} =
	radius_response(Procedure, send_request(Req, Session0, Opts),
			Opts, Session0, Events0, State0),
    case Verdict of
	success ->
	    {ok, Session, Events, State#{'Authentication-Result' => Verdict}};
	challenge ->
	    {Verdict, Session, Events, State#{'Authentication-Result' => pending}};
	_ ->
	    {Verdict, Session, Events, State#{'Authentication-Result' => Verdict}}
    end;

invoke(_Service, authorize, Session, Events, _,
       #{'Authentication-Result' := success} = State) ->
    {ok, Session, Events, State};

invoke(_Service, authorize, Session, Events, _, State) ->
    {denied, Session, Events, State};

invoke(_Service, start, Session0, Events, Opts, #{'State' := stopped} = State) ->
    Keys = ['InPackets', 'OutPackets', 'InOctets', 'OutOctets', 'Acct-Session-Time'],
    Session = maps:without(Keys, Session0),
    accounting(?RStatus_Type_Start, [], Session, Events, Opts, State#{'State' => started});

invoke(_Service, interim, Session, Events, Opts, #{'State' := started} = State) ->
    accounting(?RStatus_Type_Update, [], Session, Events, Opts, State);

invoke(_Service, stop, Session, Events, Opts, #{'State' := started} = State) ->
    accounting(?RStatus_Type_Stop, [], Session, Events, Opts, State#{'State' => stopped});

invoke(_Service, Procedure, Session, Events, _Opts, State)
  when Procedure =:= start; Procedure =:= interim; Procedure =:= stop ->
    {ok, Session, Events, State};

invoke(Service, Procedure, Session, Events, _Opts, State) ->
    {{error, {Service, Procedure}}, Session, Events, State}.

accounting(Type, Attrs0, Session, Events, #{now := Now} = Opts, State) ->
    UserName0 = maps:get('Username', Session, <<>>),
    UserName = maps:get('Username', State, UserName0),

    Attrs1 = radius_session_options(State, Attrs0),
    Attrs2 = session_options(Session, Attrs1),
    Attrs = [{?RStatus_Type,   Type},
	     {?User_Name,      UserName},
	     {?Event_Timestamp,
	      system_time_to_universal_time(Now + erlang:time_offset(), native)}
	     | Attrs2],
    Req = #radius_request{cmd = accreq, attrs = Attrs, msg_hmac = false},
    case Opts of
	#{async := true} ->
	    proc_lib:spawn(
	      fun() -> send_request(Req, Session, Opts) end);
	_ ->
	    send_request(Req, Session, Opts)
    end,

    {ok, Session, Events, State}.

%% handle_response/6
handle_response(_Promise, _Msg, Session, Events, _Opts, State) ->
    {ok, Session, Events, State}.

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_ip(Opt, Host) when is_list(Host) ->
    case gethostbyname(Host) of
	{ok, #hostent{h_addr_list = [IP | _]}} ->
	    IP;
	_ ->
	    ?LOG(error, "can't resolve remote RADIUS server name '~s'", [Host]),
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
validate_option(timeout, Value)
  when is_integer(Value) andalso Value > 0 ->
    Value;
validate_option(retries, Value)
  when is_integer(Value) andalso Value > 0 ->
    Value;
validate_option(async, Value) when is_boolean(Value) ->
    Value;
validate_option(termination_cause_mapping, Value) ->
    validate_termination_cause_mapping(Value);
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

validate_termination_cause_mapping(Opts) when is_list(Opts); is_map(Opts) ->
    ergw_aaa_config:validate_options(fun validate_termination_cause_mapping/2, Opts, ?DEFAULT_TERMINATION_CAUSE_MAPPING, map);
validate_termination_cause_mapping(Opts) ->
    throw({error, {termination_cause_mapping, Opts}}).

validate_termination_cause_mapping(Opt, Value) when is_atom(Opt), is_integer(Value) ->
    Value;
validate_termination_cause_mapping(Opt, Value) ->
    throw({error, {termination_cause_mapping, {Opt, Value}}}).

%%===================================================================
%% Internal Helpers
%%===================================================================

%% to_session/3
to_session(Procedure, {Session0, Events0}, Avps) ->
    {Session, Events, _} =
	maps:fold(to_session(Procedure, _, _, _), {Session0, Events0, #{}}, Avps),
    {Session, Events};
to_session(Procedure, {_Session, _Events, _State} = Iter, Avps) ->
    maps:fold(to_session(Procedure, _, _, _), Iter, Avps).

%% to_session/4
to_session(_, 'Acct-Interim-Interval', Interim, {Session, Events, State}) ->
    Trigger = ergw_aaa_session:trigger(accounting, 'IP-CAN', periodic, Interim),
    {Session, ergw_aaa_session:ev_set(Trigger, Events), State};

to_session(_, Key, Value, {Session, Events, State})
when Key =:= 'Session-Timeout';
     Key =:= 'Idle-Timeout' ->
    {Session#{Key => Value * 1000}, Events, State};

to_session(_, Key, Value, {Session, Events, State})
  when Key =:= 'Class';
       Key =:= 'RADIUS-State';
       Key =:= 'Username' ->
    {Session, Events, maps:put(Key, Value, State)};
to_session(_, _, _, SessEvSt) ->
    SessEvSt.

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

gethostbyname(Name) ->
    case inet:gethostbyname(Name, inet6) of
	{error, nxdomain} ->
	    inet:gethostbyname(Name, inet);
	Other ->
	    Other
    end.

%% radius_session_options/2
radius_session_options(RadiusSession, Attrs) ->
    maps:fold(fun radius_session_options/3, Attrs, RadiusSession).

radius_session_options('Class', [], Attrs) ->
    Attrs;
radius_session_options('Class', [H|T], Attrs) ->
    [{?Class, H}|radius_session_options('Class', T, Attrs)];
radius_session_options('RADIUS-State', State, Attrs) ->
    [{?State, State}|Attrs];
radius_session_options(_Key, _Value, Attrs) ->
    Attrs.

%% session_options/2
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
session_options('Framed-Pool', Value, Attrs) ->
    [{?Framed_Pool, Value}|Attrs];
session_options('Framed-IPv6-Prefix', Value, Attrs) ->
    [{?Framed_IPv6_Prefix, Value}|Attrs];
session_options('Framed-IPv6-Pool', Value, Attrs) ->
    [{?Framed_IPv6_Pool, Value}|Attrs];
session_options('Framed-Interface-Id', Value, Attrs) ->
    [{?Framed_Interface_Id, Value}|Attrs];

session_options('Session-Id', Value, Attrs) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Session_Id, Id}|Attrs];
session_options('Multi-Session-Id', Value, Attrs) ->
    Id = io_lib:format("~40.16.0B", [Value]),
    [{?Acct_Multi_Session_Id, Id}|Attrs];
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
session_options('3GPP-IMSI', Value, Attrs) ->
    [{?X_3GPP_IMSI, Value}|Attrs];
session_options('3GPP-Charging-Id', Value, Attrs) ->
    [{?X_3GPP_Charging_ID, Value}|Attrs];
session_options('3GPP-PDP-Type' = Key, Value, Attrs) ->
    [{?X_3GPP_PDP_Type, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-Charging-Gateway-Address', Value, Attrs) ->
    [{?X_3GPP_Charging_Gateway_Address, Value}|Attrs];
session_options('3GPP-GPRS-Negotiated-QoS-Profile' = Key, Value, Attrs) ->
    [{?X_3GPP_GPRS_Negotiated_QoS_profile, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-SGSN-Address', Value, Attrs) ->
    [{?X_3GPP_SGSN_Address, Value}|Attrs];
session_options('3GPP-GGSN-Address', Value, Attrs) ->
    [{?X_3GPP_GGSN_Address, Value}|Attrs];
session_options('3GPP-IMSI-MCC-MNC', Value, Attrs) ->
    [{?X_3GPP_IMSI_MCC_MNC, Value}|Attrs];
session_options('3GPP-GGSN-MCC-MNC', Value, Attrs) ->
    [{?X_3GPP_GGSN_MCC_MNC, Value}|Attrs];
session_options('3GPP-NSAPI' = Key, Value, Attrs) ->
    [{?X_3GPP_NSAPI, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-Session-Stop-Indicator' = Key, Value, Attrs) ->
    [{?X_3GPP_Session_Stop_Indicator, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-Selection-Mode' = Key, Value, Attrs) ->
    [{?X_3GPP_Selection_Mode, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-Charging-Characteristics' = Key, Value, Attrs) ->
    [{?X_3GPP_Charging_Characteristics, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-Charging-Gateway-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_Charging_Gateway_IPv6_Address, Value}|Attrs];
session_options('3GPP-SGSN-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_SGSN_IPv6_Address, Value}|Attrs];
session_options('3GPP-GGSN-IPv6-Address', Value, Attrs) ->
    [{?X_3GPP_GGSN_IPv6_Address, Value}|Attrs];
session_options('3GPP-IPv6-DNS-Servers' = Key, Value, Attrs) ->
    [{?X_3GPP_IPv6_DNS_Servers, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-SGSN-MCC-MNC', Value, Attrs) ->
    [{?X_3GPP_SGSN_MCC_MNC, Value}|Attrs];
session_options('3GPP-Teardown-Indicator' = Key, Value, Attrs) ->
    [{?X_3GPP_Teardown_Indicator, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-IMEISV', Value, Attrs) ->
    [{?X_3GPP_IMEISV, Value}|Attrs];
session_options('3GPP-RAT-Type' = Key, Value, Attrs) ->
    [{?X_3GPP_RAT_Type, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
session_options('3GPP-User-Location-Info', Value, Attrs) ->
    [{?X_3GPP_User_Location_Info, Value}|Attrs];
session_options('3GPP-MS-TimeZone' = Key, Value, Attrs) ->
    [{?X_3GPP_MS_TimeZone, ergw_aaa_3gpp_dict:encode(Key, Value)}|Attrs];
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

radius_response(Procedure, {ok, Response, RequestAuthenticator}, #{server := {_, _, Secret}},
		Session, Events, State) ->
    radius_reply(Procedure,
      eradius_lib:decode_request(Response, Secret, RequestAuthenticator), Session, Events, State);
radius_response(_Procedure, Response, _, Session, Events, State) ->
    ?LOG(error, "RADIUS failed with ~p", [Response]),
    {fail, Session, Events, State}.

radius_reply(Procedure, #radius_request{cmd = accept} = Reply, Session0, Events0, State0) ->
    ?LOG(debug, "RADIUS Reply: ~p", [Reply]),
    try
	SOpts = process_radius_attrs(Reply),
	{Session1, Events, State} = to_session(Procedure, {Session0, Events0, State0}, SOpts),
	Session2 = Session1#{'Acct-Authentic' => 'RADIUS'},
	Session = handle_eap_msg(Reply, Session2),
	{success, Session, Events, State}
    catch
	throw:#aaa_err{} = _CtxErr ->
	    {fail, Session0, Events0, State0}
    end;

radius_reply(Procedure, #radius_request{cmd = challenge} = Reply, Session0, Events0, State0) ->
    ?LOG(debug, "RADIUS Challenge: ~p", [Reply]),
    try
	SOpts = process_radius_attrs(Reply),
	{Session1, Events, State} = to_session(Procedure, {Session0, Events0, State0}, SOpts),
	Session = handle_eap_msg(Reply, Session1),
	{challenge, Session, Events, State}
    catch
	throw:#aaa_err{} = _CtxErr ->
	    {fail, Session0, Events0, State0}
    end;

radius_reply(_Procedure, #radius_request{cmd = reject} = Reply, Session0, Events, State) ->
    ?LOG(debug, "RADIUS failed with ~p", [Reply]),
    Session = handle_eap_msg(Reply, Session0),
    {fail, Session, Events, State};

radius_reply(_Procedure, Reply, Session, Events, State) ->
    ?LOG(debug, "RADIUS failed with ~p", [Reply]),
    {fail, Session, Events, State}.

handle_eap_msg(#radius_request{eap_msg = EAP}, Session)
  when EAP /= <<>> ->
    Session#{'EAP-Data' => EAP};
handle_eap_msg(_, Session) ->
    Session.

%% iterate over the RADIUS attributes
process_radius_attrs(#radius_request{attrs = Attrs}) ->
    lists:foldr(fun to_session_opts/2, #{}, Attrs).

repeated(Key, Value, Opts) ->
    maps:update_with(Key, fun(X) -> X ++ [Value] end, [Value], Opts).

to_session_opts({#attribute{name = "TP-" ++ Name} = Attr, Value}, SOpts) ->
    to_session_opts(Attr, catch (list_to_existing_atom(Name)), Value, SOpts);
to_session_opts({#attribute{name = "X_" ++ Name} = Attr, Value}, SOpts) ->
    to_session_opts(Attr, catch (list_to_existing_atom(Name)), Value, SOpts);
to_session_opts({#attribute{name = Name} = Attr, Value}, SOpts) ->
    to_session_opts(Attr, catch (list_to_existing_atom(Name)), Value, SOpts);
to_session_opts({Attr, Value}, SOpts) ->
    ?LOG(debug, "unhandled undecoded reply AVP: ~w: ~p", [Attr, Value]),
    SOpts.

%% Service-Type = Framed-User
to_session_opts(_Attr, 'Service-Type', 2, SOpts) ->
    SOpts#{'Service-Type' => 'Framed-User'};

to_session_opts(_Attr, 'Service-Type', Value, _SOpts) ->
    ?LOG(debug, "unexpected Value in Service-Type: ~p", [Value]),
    throw(?AAA_ERR(?FATAL));

%% Framed-Protocol = PPP
to_session_opts(_Attr, 'Framed-Protocol', 1, SOpts) ->
    SOpts#{'Framed-Protocol' => 'PPP'};
%% Framed-Protocol = GPRS-PDP-Context
to_session_opts(_Attr, 'Framed-Protocol', 7, SOpts) ->
    SOpts#{'Framed-Protocol' => 'GPRS-PDP-Context'};
to_session_opts(_Attr, 'Framed-Protocol', Value, _SOpts) ->
    ?LOG(debug, "unexpected Value in Framed-Protocol: ~p", [Value]),
    throw(?AAA_ERR(?FATAL));

%% Alc-Primary-Dns
to_session_opts(_Attr, 'Alc-Primary-DNS', DNS, SOpts) ->
    repeated('DNS', DNS, SOpts);
%% Alc-Secondary-Dns
to_session_opts(_Attr, 'Alc-Secondary-DNS', DNS, SOpts) ->
    repeated('DNS', DNS, SOpts);

%% Travelping Extensions

%% TP-Access-Rule
to_session_opts(_Attr, 'Access-Rule', Value, SOpts) ->
    Rule = list_to_tuple([binary_to_list(V) || V <- binary:split(Value, <<":">>, [global])]),
    repeated('Access-Rules', Rule, SOpts);

to_session_opts(_Attr, Key, Value, SOpts)
  when
      %% 3GPP TS 29.061 Attributes
      Key =:= '3GPP-IPv6-DNS-Servers' ->
    SOpts#{Key => ergw_aaa_3gpp_dict:decode(Key, Value)};

to_session_opts(_Attr, Key, Value, SOpts)
  when Key =:= 'Class';
       Key =:= 'DNS-Server-IPv6-Address' ->
    repeated(Key, Value, SOpts);

to_session_opts(_Attr, 'State', Value, SOpts) ->
    SOpts#{'RADIUS-State' => Value};

to_session_opts(_Attr, Key, Value, SOpts)
  when
      %% Generic Attributes
      Key =:= 'RADIUS-State';
      Key =:= 'Username';
      Key =:= 'Acct-Interim-Interval';
      Key =:= 'Session-Timeout';
      Key =:= 'Idle-Timeout';
      Key =:= 'Framed-IP-Address';
      Key =:= 'Framed-IPv6-Prefix';
      Key =:= 'Framed-Interface-Id';
      %% Microsoft MPPE Keys
      Key =:= 'MS-MPPE-Send-Key';
      Key =:= 'MS-MPPE-Recv-Key';
      Key =:= 'MS-Primary-DNS-Server';
      Key =:= 'MS-Secondary-DNS-Server';
      Key =:= 'MS-Primary-NBNS-Server';
      Key =:= 'MS-Secondary-NBNS-Server';
      %% Travelping Extensions
      Key =:= 'Access-Group';
      Key =:= 'NAT-Pool-Id';
      Key =:= 'NAT-IP-Address';
      Key =:= 'NAT-Port-Start';
      Key =:= 'NAT-Port-End';
      Key =:= 'Max-Input-Octets';
      Key =:= 'Max-Output-Octets';
      Key =:= 'Max-Total-Octets';
      Key =:= 'TLS-Pre-Shared-Key';
      %% CAPWAP LocationProfile attributes
      Key =:= 'CAPWAP-SSID';
      Key =:= 'CAPWAP-Max-WIFI-Clients';
      Key =:= 'CAPWAP-Power-Save-Idle-Timeout';
      Key =:= 'CAPWAP-Power-Save-Busy-Timeout' ->
    SOpts#{Key => Value};
to_session_opts(Attr, {'EXIT', {badarg, _}}, Value, SOpts) ->
    ?LOG(debug, "unhandled undecoded reply AVP: ~w: ~p", [Attr, Value]),
    SOpts.

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

send_request(Req, Session, #{server := NAS, retries := Retries, timeout := Timeout} = Opts)
  when is_tuple(NAS)->
    Id = maps:get('NAS-Identifier', Session, <<"NAS">>),
    RadiusClientOpts = [{client_name, maps:get(client_name, Opts, Id)},
			{server_name, maps:get(server_name, Opts, Id)},
			{retries, Retries},
			{timeout, Timeout}],
    eradius_client:send_request(NAS, Req, RadiusClientOpts).

get_state_atom(#{'State' := State}) ->
    State.
