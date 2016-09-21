%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_radius_handler).

-behaviour(eradius_server).

-export([
    radius_request/3
]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_travelping.hrl").

%%===================================================================
%% API
%%===================================================================
radius_request(#radius_request{cmd=discreq}=Request, _NasProp, [_NasId, _Other]) ->
    % TODO: handle disconnect request
    lager:warning("Unhandled radius request: ~p", [Request]),
    {reply, #radius_request{cmd=discnak}};

radius_request(#radius_request{cmd=coareq}=Request, _NasProp, [_NasId, _Other]) ->
    % TODO: handle coa request
    lager:warning("Unhandled radius request: ~p", [Request]),
    {reply, #radius_request{cmd=coanak}};

radius_request(#radius_request{}=Request, _NasProp, [_NasId, _Other]) ->
    lager:warning("Unhandled radius request: ~p", [Request]),
    noreply.
