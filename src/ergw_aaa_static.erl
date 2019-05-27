%% Copyright 2016,2018 Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ergw_aaa_static).

-behaviour(ergw_aaa).

%% AAA API
-export([validate_handler/1, validate_service/3, validate_procedure/5,
	 initialize_handler/1, initialize_service/2, invoke/5]).

-import(ergw_aaa_session, [to_session/1]).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

%%===================================================================
%% API
%%===================================================================

initialize_handler(_Opts) ->
    {ok, []}.

initialize_service(_ServiceId, _Opts) ->
    {ok, []}.

validate_handler(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, [], map).

validate_service(_Service, HandlerOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, HandlerOpts, map).

validate_procedure(_Application, _Procedure, _Service, ServiceOpts, Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ServiceOpts, map).

invoke(_Service, Procedure, Session, Events, #{answers := Answers, answer := Answer}) ->
    handle_response(Procedure, maps:get(Answer, Answers, #{}), Session, Events);
invoke(_Service, _Procedure, Session, Events, Opts) ->
    {ok, maps:merge(Session, Opts), Events}.

%%%===================================================================
%%% Options Validation
%%%===================================================================

%% TODO: only permit session options
validate_option(answers, Value) when is_map(Value) ->
    Value;
validate_option(answer, Value) when is_atom(Value) ->
    Value;
validate_option(_Opt, Value) ->
    Value.
%% validate_option(Opt, Value) ->
%%     throw({error, {options, {Opt, Value}}}).

%%===================================================================
%% internal helpers
%%===================================================================

%% to_session/3
to_session({rf, _} = Procedure, SessEvs, Avps) ->
    ergw_aaa_rf:to_session(Procedure, SessEvs, Avps);
to_session({gx, _} = Procedure, SessEvs, Avps) ->
    ergw_aaa_gx:to_session(Procedure, SessEvs, Avps);
to_session({gy, _} = Procedure, SessEvs, Avps) ->
    ergw_aaa_ro:to_session(Procedure, SessEvs, Avps);
to_session(_Procedure, {Session, Events}, Avps) ->
    {maps:merge(Session, Avps), Events}.

handle_response(Procedure, #{'Result-Code' := ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'} = Avps,
		Session0, Events0) ->
    {Session, Events} = to_session(Procedure, {Session0, Events0}, Avps),
    {ok, Session, Events};
handle_response(_Procedure, #{'Result-Code' := Code}, Session, Events)
  when Code == ?'DIAMETER_BASE_RESULT-CODE_AUTHORIZATION_REJECTED' ->
    {{fail, Code}, Session, [stop | Events]};
handle_response(_Procedure, #{'Result-Code' := Code}, Session, Events) ->
    {{fail, Code}, Session, Events};
handle_response(_Procedure, Avps, Session, Events)
  when is_map(Avps) ->
    {ok, maps:merge(Session, Avps), Events};
handle_response(_Procedure, Response, Session, Events) ->
    lager:error("Response: ~p", [Response]),
    {Response, Session, Events}.
