%% Copyright 2016-2021, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_radius_handler).

-behaviour(eradius_server).

-export([radius_request/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

-define(API, radius).

%%===================================================================
%% API
%%===================================================================
radius_request(#radius_request{cmd = discreq, attrs = Attrs} = Req, _NasProp, _Args) ->
    #{?Acct_Session_Id := HexSessionId} = maps:from_list([{Id, V} || {#attribute{id = Id}, V} <- Attrs]),
    Cmd = case ergw_aaa_session_reg:lookup(to_session_id(HexSessionId)) of
        Session when is_pid(Session) ->
                %% NOTE : Strictly speaking 'ASR' is a wrong name for radius disconnect.
                %%        It may be considered in the future to rename the ergw API to
                %%        something more generic (which may require change in termination
                %%        cause mapping as well).
                ergw_aaa_session:request(Session, ergw_aaa_radius, {?API, 'ASR'}, #{}),
                discack;
        _ ->
                discnak
        end,
    {reply, Req#radius_request{cmd = Cmd}};

radius_request(#radius_request{}, _NasProp, _Args) ->
    %% unsupported requests, eradius_server will logs the `noreply` as an error, no need to duplicate that
    noreply.

to_session_id(HexSessionId) ->
    H = fun(N) when N < 58  -> N - 48;
           (N) when N < 71  -> N - 55;
           (N) when N < 103 -> N - 87
    end,
    <<0,0,0,0, SessionId:128>> = << <<(H(A)):4, (H(B)):4>> || <<A, B>> <= HexSessionId >>,
    SessionId.
