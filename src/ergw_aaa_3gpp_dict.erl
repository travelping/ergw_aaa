%% Copyright 2019, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

%% Translate 3GPP TS 29.061 Attributes from/to wire format

-module(ergw_aaa_3gpp_dict).

-export([ip2bin/1, bin2ip/1]).
-export([encode/2, decode/2]).

%% 3GPP-IMSI
%% 3GPP-Charging-Id
%% 3GPP-PDP-Type
%% 3GPP-GPRS-Negotiated-QoS-Profile
%% 3GPP-IMSI-MCC-MNC
%% 3GPP-GGSN- MCC-MNC
%% 3GPP-NSAPI
%% 3GPP-Session-Stop-Indicator
%% 3GPP-Selection-Mode
%% 3GPP-Charging-Characteristics
%% 3GPP-Ipv6-DNS-Servers
%% 3GPP-SGSN-MCC-MNC
%% 3GPP-Teardown-Indicator
%% 3GPP-IMEISV
%% 3GPP-RAT-Type
%% 3GPP-User-Location-Info
%% 3GPP-MS-TimeZone
%% 3GPP-CAMEL-Charging-Info
%% 3GPP-Packet-Filter
%% 3GPP-Negotiated-DSCP
%% 3GPP-Allocate-IP-Type
%% External-Identifier
%% TWAN-Identifier
%% 3GPP-User-Location-Info-Time
%% 3GPP-Secondary-RAT-Usage

%% The following attributes need to be handled by DIAMETER and RADIUS seperatly:
%% - 3GPP-CG-Address
%% - 3GPP-SGSN-Address
%% - 3GPP-GGSN-Address
%% - 3GPP-CG-Ipv6-Address
%% - 3GPP-SGSN-Ipv6-Address
%% - 3GPP-GGSN-Ipv6-Address


%%====================================================================
%% IP Address helpers
%%====================================================================

ip2bin(IP) when is_binary(IP) ->
    IP;
ip2bin({A, B, C, D}) ->
    <<A, B, C, D>>;
ip2bin({A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.

bin2ip(<<A, B, C, D>>) ->
    {A, B, C, D};
bin2ip(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    {A, B, C, D, E, F, G, H}.

%%===================================================================
%% Encode/Decode API
%%===================================================================

encode('3GPP-PDP-Type', 'IPv4')   -> 0;
encode('3GPP-PDP-Type', 'PPP')    -> 1;
encode('3GPP-PDP-Type', 'IPv6')   -> 2;
encode('3GPP-PDP-Type', 'IPv4v6') -> 3;
encode('3GPP-PDP-Type', 'Non-IP') -> 4;

encode('3GPP-GPRS-Negotiated-QoS-Profile', Value) ->
    BinV = hexstr(Value),
    case byte_size(Value) of
	 3 -> <<"98", BinV/binary>>;
	11 -> <<"99", BinV/binary>>;
	14 -> <<"05", BinV/binary>>;
	_  -> <<"07", BinV/binary>>
    end;

encode(Key, Value) when Key =:= '3GPP-NSAPI'; Key =:= '3GPP-Selection-Mode' ->
    erlang:integer_to_binary(Value, 16);

encode('3GPP-Session-Stop-Indicator', true) -> <<255>>;
encode('3GPP-Session-Stop-Indicator', _)    -> <<0>>;

encode('3GPP-Charging-Characteristics', Value) ->
    hexstr(Value);

encode('3GPP-IPv6-DNS-Servers', Value) when is_list(Value) ->
    << <<(ip2bin(IP))/binary>> || IP <- Value >>;

encode('3GPP-Teardown-Indicator', true) -> <<1>>;
encode('3GPP-Teardown-Indicator', _)    -> <<0>>;

encode('3GPP-RAT-Type', Value) when is_integer(Value) ->
    <<Value:8>>;

encode('3GPP-MS-TimeZone', {TS, DST}) ->
    <<TS:8, DST:8>>;

encode('3GPP-Allocate-IP-Type', Value) when is_integer(Value) ->
    <<Value:8>>;

encode('3GPP-Secondary-RAT-Usage', {RAT, Start, Stop, DL, UL}) ->
    <<0:4, RAT:4, Start:32, Stop:32, DL:64, UL:64>>;

encode(_Key, Value) ->
    Value.

decode('3GPP-IPv6-DNS-Servers', Value) when byte_size(Value) rem 16 =:= 0 ->
    [bin2ip(IP) || <<IP:128/bits>> <= Value];

decode('3GPP-Teardown-Indicator', <<_:7, 1:1>>) -> true;
decode('3GPP-Teardown-Indicator', _)            -> false;

decode('3GPP-Allocate-IP-Type', Value) when is_integer(Value) ->
    <<Value:8>>;

decode(_Key, Value) ->
    Value.

%%===================================================================
%% Helper
%%===================================================================

hexstr(Value) when is_binary(Value) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X]) || <<X>> <= Value]).
