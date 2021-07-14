%% Copyright 2017-2020, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_avp_filter_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(HUT, ergw_aaa_ro).
-define(SERVICE, <<"diam-test">>).

-define(SET_TC_INFO(Name, Value),
	set_test_info(?FUNCTION_NAME, Name, Value)).

-define(GET_TC_INFO(Name),
	get_test_info(?FUNCTION_NAME, Name)).

-define(GET_TC_INFO(Name, Default),
	get_test_info(?FUNCTION_NAME, Name)).

-define(LIST_TC_INFO(), list_test_info(?FUNCTION_NAME)).

-define(DELETE_TC_INFO(Name),
	delete_test_info(?FUNCTION_NAME, Name)).

-define(CLEAR_TC_INFO(),
	clear_test_info(?FUNCTION_NAME)).

-define('Origin-Host', <<"127.0.0.1">>).
-define('Origin-Realm', <<"example.com">>).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'  => <<"NAS">>,
		'Framed-Protocol' => 'PPP',
		'Service-Type'    => 'Framed-User'}}).

-define(DIAMETER_TRANSPORTS,
	[#{connect_to => <<"aaa://127.0.10.10">>}]).

-define(TEST_SERVER_TRANSPORTS,
	[{{127, 0, 10, 10}, "server1.test-srv.example.com"}]).

-define(TEST_SERVER_CALLBACK_OVERRIDE,
	#{diameter_gy =>
	      [{handle_request,
		{?MODULE, test_server_request, []}}]}).

-define(DIAMETER_FUNCTION,
	#{?SERVICE =>
	      #{handler => ergw_aaa_diameter,
		'Origin-Host' => ?'Origin-Host',
		'Origin-Realm' => ?'Origin-Realm',
		transports => ?DIAMETER_TRANSPORTS}}).

-define(DIAMETER_RO_CONFIG,
	#{function => ?SERVICE,
	  'Destination-Realm' => <<"test-srv.example.com">>}).

-define(CONFIG,
	#{rate_limits =>
	      #{default => #{outstanding_requests => 50, rate => 50}},
	  functions => ?DIAMETER_FUNCTION,
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_ro => ?DIAMETER_RO_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
		<<"Ro">> =>
		    #{handler => 'ergw_aaa_ro'}},
	  apps =>
	      #{default =>
		    #{init => [#{service => <<"Default">>}],
		      authenticate => [],
		      authorize => [],
		      start => [],
		      interim => [],
		      stop => [],
		      {gy, 'CCR-Initial'} => [#{service => <<"Ro">>}],
		      {gy, 'CCR-Update'} => [#{service => <<"Ro">>}],
		      {gy, 'CCR-Terminate'} => [#{service => <<"Ro">>}]}
	       }
	 }).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [default_filter,
	  simple_root,
	  simple_path,
	  conditional_instance,
	  conditional_instance_value_match,
	  conditional_instance_ip_match,
	  ignore_single_condition_path].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),

    meck_init(Config),

    init_test_info_ets(),

    TestTransports =
	[[{transport_module, diameter_tcp},
	  {capabilities, [{'Origin-Host', Host}]},
	  {transport_config,
	   [{reuseaddr, true}, {ip, IP}]}]
	 || {IP, Host} <- ?TEST_SERVER_TRANSPORTS],

    diameter_test_server:start(?TEST_SERVER_CALLBACK_OVERRIDE, TestTransports),
    {ok, _} = application:ensure_all_started(ergw_aaa),
    ergw_aaa_test_lib:ergw_aaa_init(?CONFIG),

    case wait_for_diameter(?SERVICE, 10) of
	ok ->
	    Config;
	Other ->
	    end_per_suite(Config),
	    {skip, Other}
    end.

end_per_suite(Config) ->
    meck_unload(Config),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

init_per_testcase(Config) -> meck_reset(Config), Config.

end_per_testcase(Config) ->
    meck_validate(Config),
    ok.

%%%===================================================================
%%% Test cases
%%%
%%% The simplest way to test this is to fire 2 requests, 1 without
%%% and another with the filtering we test, and compare the result.
%%%===================================================================

%%====================================================================
%% Default filter (for ro it is ['3GPP-IMSI'])
%% override default filter with empty list and check if before we have
%% '3GPP-IMSI' filtered out by the default filter, then present after
%% empty filter is applied
%%====================================================================
default_filter(_Config) ->
    {Before, After} = do_filter_test([]),
    ?equal(undefined, maps:get('3GPP-IMSI', Before, undefined)),
    ?match([_], maps:get('3GPP-IMSI', After)),
    ok.

%%====================================================================
%% Simple filter of single AVPs 'User-Name' from the root of
%% the AVP tree as well as the '3GPP-IMSI' as the filter overrides the
%% handler default
%%====================================================================
simple_root(_Config) ->
    {Before, After} = do_filter_test([['3GPP-IMSI'], ['User-Name']]),

    ?equal(undefined, maps:get('3GPP-IMSI', Before, undefined)),
    ?match([_], maps:get('User-Name', Before, undefined)),

    ?equal(undefined, maps:get('3GPP-IMSI', After, undefined)),
    ?equal(undefined, maps:get('User-Name', After, undefined)),
    ok.

%%====================================================================
%% Simple path filter to remove
%% 'Service-Information' -> 'PS-Information' -> '3GPP-MS-TimeZone'
%% and '3GPP-IMSI'
%%====================================================================
simple_path(_Config) ->
    Filter = [
	['Service-Information', 'PS-Information', '3GPP-MS-TimeZone'], % delete an AVP with path
	['3GPP-IMSI'] % simple delete AVP from root
    ],
    {Before, After} = do_filter_test(Filter),

    ?match(#{'Service-Information' := [#{'PS-Information' := [#{'3GPP-MS-TimeZone' := _ }]}]}, Before),
    ?equal(undefined, maps:get('3GPP-IMSI', Before, undefined)),

    #{'Service-Information' := [#{'PS-Information' := [PSInfoAfter]}]} = After,
    ?equal(undefined, maps:get('3GPP-MS-TimeZone', PSInfoAfter, undefined)),
    ?equal(undefined, maps:get('3GPP-IMSI', After, undefined)),
    ok.


%%====================================================================
%% Conditionally filter multiple instance AVP : delete the IMSI
%% 'Subscription-Id' instance, based on the 'Subscription-Id-Type'
%% value = 1 and leave '3GPP-IMSI'
%%====================================================================
conditional_instance(_Config) ->
    Filter = [
	['Subscription-Id', [{'Subscription-Id-Type', <<"1">>}]]
    ],
    {Before, After} = do_filter_test(Filter),
    
    ?equal(undefined, maps:get('3GPP-IMSI', Before, undefined)),
    ?equal(2, length(maps:get('Subscription-Id', Before))),
    
    ?match(#{'3GPP-IMSI' := [_], 'Subscription-Id' := [#{'Subscription-Id-Type' := 0}]}, After),
    ok.
    
%%====================================================================
%% Conditionally filter multiple instance AVP : delete the AVP
%% '3GPP-User-Location-Info' from 'PS-Information' if the 'SGSN-Address'
%% value matches. The SGSN-Address is optional and also it is provided
%% in tuple format. This test confirms IP address match when the filter
%% is provided in binary format.
%%====================================================================
conditional_instance_ip_match(_Config) ->
    Filter = [
	[
	    'Service-Information',
	    'PS-Information',
	    [{'SGSN-Address', <<"192.168.1.1">>}],
	    '3GPP-User-Location-Info'
	]
    ],
    {Before, After} = do_filter_test(Filter),
	
    ?match(#{'Service-Information' := [#{'PS-Information' := [#{'3GPP-User-Location-Info' := _ }]}]}, Before),

    #{'Service-Information' := [#{'PS-Information' := [PSInfoAfter]}]} = After,
    ?equal(undefined, maps:get('3GPP-User-Location-Info', PSInfoAfter, undefined)),

    ok.
	
%%====================================================================
%% Conditionally filter multiple instance AVP : delete the MSCC
%% instance, based on the 'Rating-Group' value, defined in binary
%% value = <<"1000">> and being optional in the AVP tree
%%====================================================================
conditional_instance_value_match(_Config) ->
    Filter = [
	['Multiple-Services-Credit-Control', [{'Rating-Group', <<"1000">>}]]
    ],
    {Before, After} = do_filter_test(Filter),

    ?match(#{'Multiple-Services-Credit-Control' := [_]}, Before),
    ?equal(undefined, maps:get('Multiple-Services-Credit-Control', After, undefined)),
	
    ok.
	
%%====================================================================
%% Ignore single element path with matching condition list in the root
%% AVP structure. i.e. send the message with the other paths filtered
%%====================================================================
ignore_single_condition_path(_Config) ->
    Filter = [
	[{'3GPP-NSAPI', 5}],
	['Subscription-Id', [{'Subscription-Id-Type', 1}]]
    ],
    {Before, After} = do_filter_test(Filter),

    ?equal(undefined, maps:get('3GPP-IMSI', Before, undefined)),
    ?equal(2, length(maps:get('Subscription-Id', Before))),

    ?match(#{'3GPP-IMSI' := [_], 'Subscription-Id' := [#{'Subscription-Id-Type' := 0}]}, After),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

init_session(SessionMod) ->
    Defaults = #{'3GPP-GGSN-Address' => {172, 20, 16, 28},
		 '3GPP-IMEISV' => <<82, 21, 50, 96, 32, 80, 30, 0>>,
		 '3GPP-IMSI' => <<"250071234567890">>,
		 '3GPP-Charging-Id' => 3604013806,
		 '3GPP-IMSI-MCC-MNC' => <<"25999">>,
		 '3GPP-GGSN-MCC-MNC' => <<"25888">>,
		 '3GPP-MS-TimeZone' => {128, 1},
		 '3GPP-MSISDN' => <<"46702123456">>, '3GPP-NSAPI' => 5,
		 '3GPP-PDP-Type' => 'IPv4', '3GPP-RAT-Type' => 6,
		 '3GPP-SGSN-Address' => {192, 168, 1, 1},
		 '3GPP-SGSN-MCC-MNC' => <<"26201">>,
		 '3GPP-Selection-Mode' => 0,
		 '3GPP-User-Location-Info' =>
		     <<24, 98, 242, 16, 64, 163, 98, 242, 16, 1, 156, 232,
		       0>>,
		 'Called-Station-Id' => <<"some.station.gprs">>,
		 'Calling-Station-Id' => <<"543148000012345">>,
		 'Framed-IP-Address' => {10, 106, 14, 227},
		 'Framed-Protocol' => 'GPRS-PDP-Context',
		 'Multi-Session-Id' =>
		     1012552258277823040188863251876666193415858290601,
		 'Username' => <<"ergw">>, 'Password' => <<"ergw">>,
		 'Service-Type' => 'Framed-User',
		 'Node-Id' => <<"PGW-001">>,
		 'PDP-Context-Type' => primary,
		 'Charging-Rule-Base-Name' => <<"m2m0001">>,
		 '3GPP-GPRS-Negotiated-QoS-Profile' =>
		     <<11, 146, 31, 147, 150, 64, 64, 255, 255, 255, 255, 17,
		       1, 1, 64, 64>>,
		 '3GPP-Allocation-Retention-Priority' => 2,
		 '3GPP-Charging-Characteristics' => <<8, 0>>,
		 'QoS-Information' =>
		     #{'QoS-Class-Identifier' => 8,
		       'Max-Requested-Bandwidth-DL' => 0,
		       'Max-Requested-Bandwidth-UL' => 0,
		       'Guaranteed-Bitrate-DL' => 0,
		       'Guaranteed-Bitrate-UL' => 0,
		       'Allocation-Retention-Priority' =>
			   #{'Priority-Level' => 10,
			     'Pre-emption-Capability' => 1,
			     'Pre-emption-Vulnerability' => 0},
		       'APN-Aggregate-Max-Bitrate-DL' => 84000000,
		       'APN-Aggregate-Max-Bitrate-UL' => 8640000}},
    maps:merge(Defaults, SessionMod).

test_server_request(#diameter_packet{msg = [_ | Msg]} =	Packet,
		    Svc, {_, Caps} = Peer, Extra) ->
    #{'Session-Id' := SId,
      'Auth-Application-Id' := AppId,
      'CC-Request-Type' := Type,
      'CC-Request-Number' := Number} = Msg,

    Resp = case get_diameter_session_handler(SId) of
	     Fun when is_function(Fun) ->
		 Fun(Packet, Svc, Peer, Extra);
	     _ -> ok
	   end,

    case Resp of
      ok ->
	#diameter_caps{origin_host = {OH, _},
		       origin_realm = {OR, _}} = Caps,
	CCA = #{'Session-Id' => SId,
		'Auth-Application-Id' => AppId,
		'Origin-Host' => OH,
		'Origin-Realm' => OR,
		'CC-Request-Type' => Type,
		'CC-Request-Number' => Number,
		'Result-Code' => ?'DIAMETER_BASE_RESULT-CODE_SUCCESS'},
	{reply, ['CCA' | CCA]};
      _ -> Resp
    end.

do_filter_test(Filter) ->
    do_filter_test(Filter, #{}).

do_filter_test(Filter, SessionMod) ->
    DTRA = fun (#diameter_packet{msg = [_ | AVPs]}, _SVC,
		_Peer, _Extra) ->
		   ?SET_TC_INFO(received, AVPs), ok
	   end,
    MECK = fun(Sevice, Procedure, Session, Events, Opts, State) ->
		#{'Diameter-Session-Id' := SId} = Session,
		NewOpts = case get_avp_filter(SId) of
		    undefined -> Opts;
		    Filter ->    Opts#{avp_filter => Filter}
		end,
		meck:passthrough([Sevice, Procedure, Session, Events, NewOpts, State])
	   end,
    meck:expect(ergw_aaa_ro, invoke, MECK),
    Session = init_session(SessionMod),
    GyOpts = #{credits => #{1000 => empty}},
    {ok, SId1} = ergw_aaa_session_sup:new_session(self(),
						  Session),
    {ok, DiameterSId1} = ergw_aaa_session:get(SId1,
					      'Diameter-Session-Id'),
    set_diameter_session_handler(DiameterSId1, DTRA),
    {ok, _, _} = ergw_aaa_session:invoke(SId1, GyOpts,
					 {gy, 'CCR-Initial'}, []),
    Before = (?GET_TC_INFO(received)),
    {ok, SId2} = ergw_aaa_session_sup:new_session(self(),
						  Session),
    {ok, DiameterSId2} = ergw_aaa_session:get(SId2,
					      'Diameter-Session-Id'),
    set_avp_filter(DiameterSId2, Filter),
    set_diameter_session_handler(DiameterSId2, DTRA),
    {ok, _, _} = ergw_aaa_session:invoke(SId2, GyOpts,
					 {gy, 'CCR-Initial'}, []),
    After = (?GET_TC_INFO(received)),
    ?CLEAR_TC_INFO(),
    {Before, After}.

init_test_info_ets() ->
    spawn(fun () ->
		  ets:new(?MODULE,
			  [named_table, public, {write_concurrency, true}]),
		  receive stop -> ok end
	  end).

stop_test_info_ets() -> ets:info(?MODULE, owner) ! stop.

set_test_info(TC, Name, Value) ->
    ets:insert(?MODULE, {{TC, Name}, Value}).

get_test_info(TC, Name) ->
    case ets:lookup(?MODULE, {TC, Name}) of
      [] -> undefined;
      [{_, Value}] -> Value
    end.

delete_test_info(TC, Name) ->
    case ets:lookup(?MODULE, {TC, Name}) of
      [] -> undefined;
      [{_, Value}] -> ets:delete(?MODULE, {TC, Name}), Value
    end.

clear_test_info(TC) ->
    ets:match_delete(?MODULE, {{TC, '_'}, '_'}).

list_test_info(TC) ->
    [list_to_tuple(Res)
     || Res <- ets:match(?MODULE, {{TC, '$1'}, '$2'})].

set_diameter_session_handler(SId, Fun) ->
    ets:insert(?MODULE,
	       {{diameter_session_handler, SId}, Fun}).

get_diameter_session_handler(SId) ->
    case ets:lookup(?MODULE,
		    {diameter_session_handler, SId})
	of
      [] -> undefined;
      [{_, Fun}] -> Fun
    end.

delete_diameter_session_handler(SId) ->
    ets:delete(?MODULE, {diameter_session_handler, SId}).

set_avp_filter(SId, Filter) ->
    ets:insert(?MODULE,
	       {{diameter_session_avp_filter, SId}, Filter}).

get_avp_filter(SId) ->
    case ets:lookup(?MODULE,
		    {diameter_session_avp_filter, SId})
    of
      [] -> undefined;
      [{_, Filter}] -> Filter
    end.
