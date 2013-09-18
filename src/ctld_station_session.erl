-module(ctld_station_session).

%% AAA API
-export([association/3, disassociation/3]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_tunnel.hrl").
-include_lib("eradius/include/dictionary_rfc4679.hrl").
-include_lib("eradius/include/dictionary_alcatel_sr.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

%%===================================================================
%% API
%%===================================================================
association(StationMac, WtpIp, Opts) ->
    NAS = proplists:get_value(radius_auth_server, Opts, false),
    NasId = proplists:get_value(nas_identifier, Opts, <<"NAS">>),
    Attrs = [
             {?Calling_Station_Id, StationMac},
             {?TP_Location_Id,  ip_to_bin(WtpIp)},
             {?Service_Type,    2},
             {?Framed_Protocol, 1},
             {?NAS_Identifier,  NasId}
            ],
    Req = #radius_request{
             cmd = request,
             attrs = Attrs,
             msg_hmac = false},
    case NAS of
        false ->
            success;
        _ ->
            radius_response(Req, eradius_client:send_request(NAS, Req), NAS)
    end.

disassociation(StationMac, WtpIp, Opts) ->
    NAS = proplists:get_value(radius_acct_server, Opts, false),
    NasId = proplists:get_value(nas_identifier, Opts, <<"NAS">>),
    Attrs = [
             {?Calling_Station_Id, StationMac},
             {?TP_Location_Id, ip_to_bin(WtpIp)},
             {?NAS_Identifier, NasId},
             {?Acct_Terminate_Cause, ?RTCUser_Request},
             {?RStatus_Type, ?RStatus_Type_Stop}
            ],
    Req = #radius_request{
             cmd = accreq,
             attrs = Attrs,
             msg_hmac = false},
    case NAS of
        false ->
            success;
        _ ->
            radius_response(Req, eradius_client:send_request(NAS, Req), NAS)
    end.

radius_response(Req, {ok, Response}, {_, _, Secret}) ->
    radius_reply(Req, eradius_lib:decode_request(Response, Secret));
radius_response(Req, Response, _) ->
    lager:error("RADIUS request ~p failed with ~p", [Req, Response]),
    fail.

radius_reply(_, #radius_request{cmd = accept} = Reply) ->
    lager:debug("RADIUS Reply: ~p", [Reply]),
    success;
radius_reply(Req, #radius_request{cmd = reject} = Reply) ->
    lager:debug("RADIUS request ~p failed with ~p", [Req, Reply]),
    fail;
radius_reply(Req, Reply) ->
    lager:debug("RADIUS request ~p failed with ~p", [Req, Reply]),
    fail.

ip_to_bin(Ip) when is_binary(Ip) ->
    Ip;
ip_to_bin({_, _, _, _}=Ip) ->
    erlang:list_to_binary(inet_parse:ntoa(Ip)).
