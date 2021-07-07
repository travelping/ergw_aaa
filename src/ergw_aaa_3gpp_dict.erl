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

-include_lib("gtplib/include/gtp_packet.hrl").

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

%% 3GPP-User-Location-Info
%%
%% types from 3GPP TS 29.061, Rel. 16.2:
%%  0        CGI
%%  1        SAI
%%  2        RAI
%%  3-127    Spare for future use
%%  128      TAI
%%  129      ECGI
%%  130      TAI and ECGI
%%  131      eNodeB ID
%%  132      TAI and eNodeB ID
%%  133      extended eNodeB ID
%%  134      TAI and extended eNodeB ID
%%  135      NCGI
%%  136      5GS TAI
%%  137      5GS TAI and NCGI
%%  138      NG-RAN Node ID
%%  139      5GS TAI and NG-RAN Node ID
%%  140-255  Spare for future use
encode('User-Location-Info', #{'TAI' := TAI, 'ext-macro-eNB' := EMeNB})
  when is_record(TAI, tai), is_record(EMeNB, ext_macro_enb) ->
    <<134, (encode_tai(TAI))/binary, (encode_ext_macro_enb(EMeNB))/binary>>;
encode('User-Location-Info', #{'ext-macro-eNB' := EMeNB})
  when is_record(EMeNB, ext_macro_enb) ->
    <<133, (encode_ext_macro_enb(EMeNB))/binary>>;
encode('User-Location-Info', #{'TAI' := TAI, 'macro-eNB' := MeNB})
  when is_record(TAI, tai), is_record(MeNB, macro_enb) ->
    <<132, (encode_tai(TAI))/binary, (encode_macro_enb(MeNB))/binary>>;
encode('User-Location-Info', #{'macro-eNB' := MeNB})
  when is_record(MeNB, macro_enb) ->
    <<131, (encode_macro_enb(MeNB))/binary>>;
encode('User-Location-Info', #{'TAI' := TAI, 'ECGI' := ECGI})
  when is_record(TAI, tai), is_record(ECGI, ecgi) ->
    <<130, (encode_tai(TAI))/binary, (encode_ecgi(ECGI))/binary>>;
encode('User-Location-Info', #{'ECGI' := ECGI})
  when is_record(ECGI, ecgi) ->
    <<129, (encode_ecgi(ECGI))/binary>>;
encode('User-Location-Info', #{'TAI' := TAI})
  when is_record(TAI, tai) ->
    <<128, (encode_tai(TAI))/binary>>;
encode('User-Location-Info', #{'RAI' := RAI})
  when is_record(RAI, rai) ->
    <<2, (encode_rai(RAI))/binary>>;
encode('User-Location-Info', #{'SAI' := SAI})
  when is_record(SAI, sai) ->
    <<1, (encode_sai(SAI))/binary>>;
encode('User-Location-Info', #{'CGI' := CGI})
  when is_record(CGI, cgi) ->
    <<0, (encode_cgi(CGI))/binary>>;
encode('User-Location-Info', _Value) ->
    <<>>;

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

encode_cgi(#cgi{plmn_id = PLMN, lac = LAC, ci = CI}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, LAC:16, CI:16>>.

encode_sai(#sai{plmn_id = PLMN, lac = LAC, sac = SAC}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, LAC:16, SAC:16>>.

encode_rai(#rai{plmn_id = PLMN, lac = LAC, rac = RAC}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, LAC:16, RAC:16>>.

encode_tai(#tai{plmn_id = PLMN, tac = TAC}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, TAC:16>>.

encode_ecgi(#ecgi{plmn_id = PLMN, eci = ECI}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, 0:4, ECI:28>>.

%% encode_lai(#lai{plmn_id = PLMN, lac = LAC}) ->
%%     <<(gtp_packet:encode_plmn_id(PLMN))/binary, LAC:16>>.

encode_macro_enb(#macro_enb{plmn_id = PLMN, id = Id}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, 0:4, Id:20>>.

encode_ext_macro_enb(#ext_macro_enb{plmn_id = PLMN, id = Id})
  when Id =< 16#03ffff ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, 0:1, 0:5, Id:18>>;
encode_ext_macro_enb(#ext_macro_enb{plmn_id = PLMN, id = Id}) ->
    <<(gtp_packet:encode_plmn_id(PLMN))/binary, 1:1, 0:2, Id:21>>.
