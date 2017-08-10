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

-module(diameter_test_server).

-include_lib("diameter/include/diameter.hrl").
-include("../include/diameter_3gpp_ts29_061_sgi.hrl").

-export([start/0, stop/0]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).


%%===================================================================
%% API
%%===================================================================

start() ->
    application:ensure_all_started(diameter),
    SvcOpts = [{'Origin-Host', "server.example.com"},
               {'Origin-Realm', "example.com"},
               {'Vendor-Id', 193},
               {'Product-Name', "Server"},
               {'Auth-Application-Id', [1]},
               {restrict_connections, false},
               {string_decode, false},
               {application, [{alias, nasreq},
                              {dictionary, diameter_3gpp_ts29_061_sgi},
                              {module, ?MODULE}]}],
    diameter:start_service(?MODULE, SvcOpts),
    Opts = [{transport_module, diameter_tcp},
            {transport_config, [{reuseaddr, true},
                                {ip, {127,0,0,1}},
                                {port, 3868}]}],
    diameter:add_transport(?MODULE, {listen, Opts}).

stop() ->
    diameter:stop_service(?MODULE).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

peer_up(_SvcName, _Peer, State) ->
    State.

peer_down(_SvcName, _Peer, State) ->
    State.

pick_peer(_, _, _SvcName, _State) ->
    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_request(#diameter_packet{msg = Msg}, _SvcName, {_, Caps}) 
  when is_record(Msg, diameter_sgi_ACR) ->
    #diameter_caps{origin_host = {OH, _},
                   origin_realm = {OR, _}} = Caps,
    #diameter_sgi_ACR{'Session-Id' = Id,
                      'Accounting-Record-Type' = Type,
                      'Accounting-Record-Number' = Number,
                      'Acct-Application-Id' = AppId} = Msg,
    ACA =  #diameter_sgi_ACA{'Session-Id' = Id,
                             'Result-Code' = 2001,
                             'Origin-Host' = OH,
                             'Origin-Realm' = OR,
                             'Acct-Interim-Interval' = [1],
                             'Accounting-Record-Type' = Type,
                             'Accounting-Record-Number' = Number,
                             'Acct-Application-Id' = AppId},
    case Msg#diameter_sgi_ACR.'3GPP-IMSI' of
         [] -> {reply, ACA};
         _IMSI -> check_3gpp(Msg, ACA)
    end;

handle_request(#diameter_packet{msg = _Msg}, _SvcName, _) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

check_3gpp(Msg, ACA) ->
    Success = [<<"250071234567890">>] == Msg#diameter_sgi_ACR.'3GPP-IMSI'
        andalso [<<214, 208, 226, 238>>] == Msg#diameter_sgi_ACR.'3GPP-Charging-Id'
        andalso [1] == Msg#diameter_sgi_ACR.'3GPP-PDP-Type'
        andalso [<<10, 10, 10, 10>>] == Msg#diameter_sgi_ACR.'3GPP-SGSN-Address'
        andalso [<<"250999">>] == Msg#diameter_sgi_ACR.'3GPP-IMSI-MCC-MNC'
        andalso [<<"250888">>] == Msg#diameter_sgi_ACR.'3GPP-GGSN-MCC-MNC'
        andalso [<<100, 10, 10, 10>>] == Msg#diameter_sgi_ACR.'3GPP-SGSN-IPv6-Address'
        andalso [<<200, 10, 10, 10>>] == Msg#diameter_sgi_ACR.'3GPP-GGSN-IPv6-Address'
        andalso [<<"250777">>] == Msg#diameter_sgi_ACR.'3GPP-SGSN-MCC-MNC'
        andalso [<<"3566190531472414">>] == Msg#diameter_sgi_ACR.'3GPP-IMEISV'
        andalso [<<1>>] == Msg#diameter_sgi_ACR.'3GPP-RAT-Type',
    if Success == true -> {reply, ACA};
       true -> {answer_message, 3001}
    end.
