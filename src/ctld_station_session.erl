-module(ctld_station_session).

%% AAA API
-export([association/3]).

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
    NAS = proplists:get_value(radius_auth_server, Opts, {{127,0,0,1}, 1812, <<"secret">>}),
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
    radius_response(eradius_client:send_request(NAS, Req), NAS).

radius_response({ok, Response}, {_, _, Secret}) ->
    radius_reply(eradius_lib:decode_request(Response, Secret));
radius_response(Response, _) ->
    lager:error("RADIUS failed with ~p", [Response]),
    fail.

radius_reply(#radius_request{cmd = accept} = Reply) ->
    lager:debug("RADIUS Reply: ~p", [Reply]),
    success;
radius_reply(#radius_request{cmd = reject} = Reply) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    fail;
radius_reply(Reply) ->
    lager:debug("RADIUS failed with ~p", [Reply]),
    fail.

ip_to_bin(Ip) when is_binary(Ip) ->
    Ip;
ip_to_bin({_, _, _, _}=Ip) ->
    erlang:list_to_binary(inet_parse:ntoa(Ip)).
