%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_test_server).

-include_lib("diameter/include/diameter.hrl").
-include("../include/diameter_nasreq_rfc7155.hrl").

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
                              {dictionary, diameter_nasreq_rfc7155},
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

handle_request(#diameter_packet{msg = _Msg}, _SvcName, _) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED
