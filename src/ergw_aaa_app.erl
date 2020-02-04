%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ergw_aaa_sup:start_link() of
	{ok, _} = Ret ->
	    Config = ergw_aaa_config:load_config(),
		ergw_aaa_prometheus:declare(),
	    SrvSupSpecs0 = initialize_handlers(Config, []),
	    SrvSupSpecs1 = initialize_services(Config, SrvSupSpecs0),
	    SrvSupSpecs = initialize_functions(Config, SrvSupSpecs1),
	    ergw_aaa_sup:start_childs(SrvSupSpecs),
	    Ret;
	Other ->
	    Other
    end.

stop(_State) ->
    ok.

%%===================================================================
%% Internal
%%===================================================================

initialize_functions(#{functions := Functions}, SupSpecs) ->
    maps:fold(fun(Function, #{handler := Handler} = Opts, Specs) ->
		      {ok, SupSpec} = Handler:initialize_function(Function, Opts),
		      Specs ++ SupSpec
	      end, SupSpecs, Functions).

initialize_handlers(#{handlers := Handlers}, SupSpecs) ->
    maps:fold(fun(Handler, Opts, Specs) ->
		      {ok, SupSpec} = Handler:initialize_handler(Opts),
		      Specs ++ SupSpec
	      end, SupSpecs, Handlers).

initialize_services(#{services := Services}, SupSpecs) ->
    maps:fold(fun(ServiceId, #{handler := Handler} = Opts, Specs) ->
		      {ok, SupSpec} = Handler:initialize_service(ServiceId, Opts),
		      Specs ++ SupSpec
	      end, SupSpecs, Services).
