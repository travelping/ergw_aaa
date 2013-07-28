%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>
-module(ctld_radius_handler).

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
