-module(eradius_test_handler).

-behaviour(eradius_server).

-export([start/0, stop/0, send_request/1, radius_request/3]).

-include_lib("eradius/include/eradius_lib.hrl").
-include_lib("eradius/include/eradius_dict.hrl").
-include_lib("eradius/include/dictionary.hrl").
-include_lib("eradius/include/dictionary_ituma.hrl").

start() ->
    application:stop(eradius),
    application:load(eradius),
    application:set_env(eradius, radius_callback, ?MODULE),
    application:set_env(eradius, session_nodes, local),
    application:set_env(eradius, one, [{{"ONE", []}, [{"127.0.0.1", "secret"}]}]),
    application:set_env(eradius, servers, [{one, {"127.0.0.1", [1812, 1813, 3799]}}]),
    application:ensure_all_started(eradius).

stop() ->
    application:stop(eradius),
    application:unload(eradius),
    application:start(eradius).

send_request(IP) ->
    {ok, R, A} = eradius_client:send_request({IP, 1812, "secret"}, #radius_request{cmd = request}, []),
    #radius_request{cmd = Cmd} = eradius_lib:decode_request(R, <<"secret">>, A),
    Cmd.

radius_request(#radius_request{cmd = request, attrs = Attrs} = _Req, _Nasprop, _Args) ->
    AttrsMap =
	lists:foldl(fun({#attribute{id = Id}, V}, M) -> M#{Id => V} end, #{}, Attrs),
    ct:pal("Req: ~p", [AttrsMap]),
    case AttrsMap of
	#{?User_Name := <<"AVP-Filter">>} ->
	    case maps:is_key(?Framed_IPv6_Pool, AttrsMap) of
		true ->  {reply, #radius_request{cmd = reject, attrs = []}};
		false -> {reply, #radius_request{cmd = accept, attrs = []}}
	    end;
	#{?User_Name := <<"Vendor-Dicts">>} ->
	    case AttrsMap of
		#{?IM_SSID := _, ?IM_Wifi_Connect_Type := _, ?IM_LI_Location := _} ->
		    {reply, #radius_request{cmd = accept, attrs = []}};
		_ ->
		    {reply, #radius_request{cmd = reject, attrs = []}}
	    end;
	_ ->
	    IEs = [{?Acct_Interim_Interval, 1800},
		   {?MS_Primary_DNS_Server, {8,8,8,8}},
		   {?Filter_Id, <<"test">>}],
	    {reply, #radius_request{cmd = accept, attrs = IEs}}
	end;
radius_request(#radius_request{cmd = accreq} = _Req, _Nasprop, _Args) ->
    {reply, #radius_request{cmd = accresp}};
radius_request(#radius_request{cmd = discreq} = Req, Nasprop, Args) ->
    ergw_aaa_radius_handler:radius_request(Req, Nasprop, Args);
radius_request(_Req, _Nasprop, _Args) ->
    {reply, #radius_request{cmd = accept}}.
