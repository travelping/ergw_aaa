%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(config_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(bad(Fun), ?match({'EXIT', {badarg, _}}, (catch Fun))).
-define(ok(Fun), ?match(#{}, (catch Fun))).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'        => <<"NAS-Identifier">>,
		'Acct-Interim-Interval' => 600,
		'Framed-Protocol'       => 'PPP',
		'Service-Type'          => 'Framed-User'}}).

-define(RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING,
	#{normal                => 1,
	  administrative        => 6,
	  link_broken           => 2,
	  upf_failure           => 9,
	  remote_failure        => 2,
	  cp_inactivity_timeout => 4,
	  up_inactivity_timeout => 4,
	  peer_restart          => 7,
	  'ASR'                 => 6,
	  error                 => 9,
	  req_timeout           => 4,
	  conn_error            => 10,
	  rate_limit            => 10,
	  ocs_hold_end          => 10,
	  peer_reject           => 10
	 }).

-define(RADIUS_AUTH_CONFIG,
	#{server                    => #{host => {127,0,0,1},
					 port => 1812,
					 secret => <<"secret">>},
	  retries                   => 3,
	  timeout                   => 5000,
	  vendor_dicts              => [],
	  avp_filter                => #{},
	  termination_cause_mapping => ?RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING}).
-define(RADIUS_ACCT_CONFIG,
	#{server                    => #{host => {0,0,0,0,0,0,0,1},
					 port => 1813,
					 secret => <<"secret">>},
	  retries                   => 3,
	  timeout                   => 5000,
	  vendor_dicts              => [],
	  avp_filter                => #{},
	  termination_cause_mapping => ?RADIUS_DEFAULT_TERMINATION_CAUSE_MAPPING}).
-define(RADIUS_SERVICE_OPTS, #{service => <<"RADIUS-Service">>}).

-define(DIAMETER_TRANSPORT,
	#{connect_to => <<"aaa://127.0.0.1">>}).

-define(DIAMETER_FUNCTION,
	#{handler        => ergw_aaa_diameter,
	  'Origin-Host'  => <<"127.0.0.1">>,
	  'Origin-Realm' => <<"test-clnt.example.com">>,
	  transports     => [?DIAMETER_TRANSPORT]}).
-define(DIAMETER_CONFIG,
	#{function            => <<"diam-test">>,
	  'Destination-Realm' => <<"test-srv.example.com">>}).
-define(DIAMETER_SERVICE_OPTS, #{service => <<"DIAMETER-Service">>}).

-define(RF_CONFIG,
	#{function            => <<"diam-test">>,
	  'Destination-Realm' => <<"test-srv.example.com">>}).

-define(RO_CONFIG,
	#{function            => <<"diam-test">>,
	  'Destination-Realm' => <<"test-srv.example.com">>}).

-define(CONFIG,
	#{functions => ?DIAMETER_FUNCTION,
	  handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG,
		ergw_aaa_radius => ?RADIUS_ACCT_CONFIG,
		ergw_aaa_nasreq => ?DIAMETER_CONFIG,
		ergw_aaa_rf     => ?RF_CONFIG,
		ergw_aaa_ro     => ?RF_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static'},
		<<"RADIUS-Service">> =>
		    #{handler => 'ergw_aaa_radius'},
		<<"DIAMETER-Service">> =>
		    #{handler => 'ergw_aaa_nasreq'},
		<<"RF-Service">> =>
		    maps:put(handler, 'ergw_aaa_rf', ?RF_CONFIG),
		<<"RO-Service">> =>
		    maps:put(handler, 'ergw_aaa_ro', ?RF_CONFIG)},
	  apps =>
	      #{<<"RADIUS-Application">> =>
		    #{'Origin-Host' => <<"dummy.host">>,
		      procedures =>
			  #{init => [#{service => <<"Default">>}],
		      	    authenticate =>
				[maps:merge(?RADIUS_AUTH_CONFIG, ?RADIUS_SERVICE_OPTS),
				 #{service => <<"Default">>, defaults => #{'TLS-Pre-Shared-Key' => <<"secret">>}}],
			    authorize => [],
			    start => [],
		            interim => [],
		            stop => []},
		    },
		<<"DIAMETER-Application">> =>
		    #{'Origin-Host' => <<"dummy.host">>,
		      procedures =>
			  #{init => [#{service => <<"Default">>}],
			    authenticate => [?DIAMETER_SERVICE_OPTS],
			    authorize => [],
			    start => [],
			    interim => [],
			    stop => []}
		    }
	      }
	  }).

%%%===================================================================
%%% Helpers
%%%===================================================================

match_map(Match, Map, File, Line) ->
    maps:fold(
      fun(Key, Expected, R) ->
	      case maps:is_key(Key, Map) of
		  true ->
		      Actual = maps:get(Key, Map),
		      case erlang:match_spec_test({Actual, ok}, [{{Expected, '$1'}, [], ['$1']}], table) of
			  {ok, ok, _, _} ->
			      R andalso true;
			  {ok, false, _, _} ->
			      ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
				     [File, Line, Key, Expected, Actual]),
			      false
		      end;
		  _ ->
		      ct:pal("MAP KEY MISSING(~s:~b, ~s)~n", [File, Line, Key]),
		      false
	      end
      end, true, Match) orelse error(badmatch),
    ok.

-define(match_map(Expected, Actual), match_map(Expected, Actual, ?FILE, ?LINE)).

%%%===================================================================
%%% Tests + API
%%%===================================================================

all() ->
    [toplevel,
     rate_limits,
     function,
     static_handler,
     radius_handler,
     nasreq_handler,
     rf_handler,
     ro_handler,
     service,
     app
    ].

init_per_testcase(_, Config) ->
    ergw_aaa_test_lib:clear_app_env(),
    Config.

end_per_testcase(_, _Config) ->
    ok.

toplevel() ->
    [{doc, "Test simple top level options"}].
toplevel(_Config)  ->
    Cfg = #{product_name => <<"PRODUCT">>},
    ValF = fun(Opts) ->
		   ergw_aaa_config:validate_options(
		     fun ergw_aaa_config:validate_option/2, Opts, [])
	   end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),
    ok.

rate_limits() ->
    [{doc, "Test rate limit options"}].
rate_limits(_Config)  ->
    Limit = [{outstanding_requests, 50}, {rate, 50}],
    ValF = fun ergw_aaa_config:validate_rate_limit/2,

    ?ok(ValF(default, Limit)),
    ?ok(ValF(default, ValF(default, Limit))),

    ?ok(ValF(default, [])),
    ?ok(ValF(default, [{outstanding_requests, 50}])),
    ?ok(ValF(default, [{rate, 50}])),

    ?bad(ValF(default, [{rate, 50}, {rate, 50}])),
    ?bad(ValF(default, [{outstanding_requests, invalid}])),
    ?bad(ValF(default, [{rate, invalid}])),

    ?bad(ValF(default, [{rate, 0}])),
    ?bad(ValF(default, [{rate, 100000}])),
    ok.

function() ->
    [{doc, "Test function options"}].
function(_Config)  ->
    Function = ?DIAMETER_FUNCTION,
    ValF = fun ergw_aaa_config:validate_function/2,

    ?ok(ValF(<<"test">>, Function)),
    ?ok(ValF(<<"test">>, ValF(<<"test">>, Function))),

    ?bad(ValF(<<"test">>, [])),
    ?bad(ValF(<<"test">>, set_cfg_value([invalid_option], [], Function))),
    ?bad(ValF(<<"test">>, set_cfg_value(['Origin-Host'], invalid_host, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value(['Origin-Host'], <<"undefined.example.net">>, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value(['Origin-Realm'], invalid_realm, Function))),

    ?bad(ValF(<<"test">>, set_cfg_value([transports], [], Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports], {}, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, connect_to], invalid_uri, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, connect_to], <<"http://example.com:12345">>, Function))),

    % transport options
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, fragment_timer], 200, Function))),
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, recbuf], 424242, Function))),
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, sndbuf], 424242, Function))),
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, unordered], false, Function))),
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, reuseaddr], false, Function))),
    ?ok(ValF(<<"test">>, set_cfg_value([transports, 1, nodelay], false, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, fragment_timer], invalid, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, recbuf], invalid, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, recbuf], 13, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, sndbuf], invalid, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, sndbuf], 13, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, unordered], invalid, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, reuseaddr], invalid, Function))),
    ?bad(ValF(<<"test">>, set_cfg_value([transports, 1, nodelay], invalid, Function))),
    ok.

static_handler() ->
    [{doc, "Test static handler options"}].
static_handler(_Config)  ->
    Cfg = ?STATIC_CONFIG,
    ValF = fun(Opts) -> ergw_aaa_config:validate_handler(ergw_aaa_static, Opts) end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),

    ?ok(ValF(#{})),
    ?ok(ValF([])),

    %% ?bad(ValF(set_cfg_value([invalid_option], [], Cfg))),
    %% ?bad(ValF(set_cfg_value([shared_secret], invalid_secret, Cfg))),
    %% ?ok(ValF(set_cfg_value([shared_secret], <<"secret">>, Cfg))),
    ok.

radius_handler() ->
    [{doc, "Test radius handler options"}].
radius_handler(_Config)  ->
    Cfg = ?RADIUS_ACCT_CONFIG,
    ValF = fun(Opts) -> ergw_aaa_config:validate_handler(ergw_aaa_radius, Opts) end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),

    ?bad(ValF(#{})),
    ?bad(ValF(set_cfg_value([invalid_option], [], Cfg))),
    ?bad(ValF(set_cfg_value([server], invalid_id, Cfg))),
    ?bad(ValF(set_cfg_value([server], invalid_id, Cfg))),
    ?bad(ValF(set_cfg_value([server], {"undefined.example.net",1812,<<"secret">>}, Cfg))),
    ?bad(ValF(set_cfg_value([server], {invalid_ip,1812,<<"secret">>}, Cfg))),
    ?bad(ValF(set_cfg_value([server], {{127,0,0,1},invalid_port,<<"secret">>}, Cfg))),
    ?bad(ValF(set_cfg_value([server], {{127,0,0,1},1812,invalid_secret}, Cfg))),
    ?bad(ValF(set_cfg_value([async], invalid, Cfg))),
    ?bad(ValF(set_cfg_value([retries], invalid, Cfg))),
    ?bad(ValF(set_cfg_value([timeout], invalid, Cfg))),

    ?ok(ValF(set_cfg_value([server], #{host => "localhost", port => 1812, secret => <<"secret">>}, Cfg))),
    ?ok(ValF(set_cfg_value([server], #{host => <<"localhost">>, port => 1812, secret => <<"secret">>}, Cfg))),
    ?ok(ValF(set_cfg_value([async], true, Cfg))),
    ?ok(ValF(set_cfg_value([async], false, Cfg))),
    ?ok(ValF(set_cfg_value([retries], 1, Cfg))),
    ?ok(ValF(set_cfg_value([timeout], 1000, Cfg))),

    ?ok(ValF(set_cfg_value([vendor_dicts], [], Cfg))),
    ?ok(ValF(set_cfg_value([vendor_dicts], [ituma], Cfg))),
    ?ok(ValF(set_cfg_value([vendor_dicts], [52315], Cfg))),
    ?bad(ValF(set_cfg_value([vendor_dicts], [atom], Cfg))),
    ?bad(ValF(set_cfg_value([vendor_dicts], atom, Cfg))),
    ?bad(ValF(set_cfg_value([vendor_dicts], 52315, Cfg))),
    ?bad(ValF(set_cfg_value([vendor_dicts], ituma, Cfg))),

    % termination cause mapping tests config
    ?ok(ValF(set_cfg_value([termination_cause_mapping], #{normal => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{wrong_mapped_value => some_atom}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{"improper_key" => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], invalid, Cfg))),
    ok.


nasreq_handler() ->
    [{doc, "Test nasreq handler options"}].
nasreq_handler(_Config)  ->
    Cfg = ?DIAMETER_CONFIG,
    ValF = fun(Opts) -> ergw_aaa_config:validate_handler(ergw_aaa_nasreq, Opts) end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),

    ?bad(ValF(#{})),

    ?bad(ValF(set_cfg_value([invalid_option], [], Cfg))),
    ?bad(ValF(set_cfg_value([accounting], [], Cfg))),
    ?bad(ValF(set_cfg_value([avp_filter], invalid, Cfg))),
    ?ok(ValF(set_cfg_value([accounting], split, Cfg))),
    ?ok(ValF(set_cfg_value([accounting], coupled, Cfg))),
    ?ok(ValF(set_cfg_value([avp_filter], ['3GPP-IMSI', ['3GPP-MSISDN']], Cfg))),

    % termination cause mapping tests config
    ?ok(ValF(set_cfg_value([termination_cause_mapping], #{normal => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{wrong_mapped_value => some_atom}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{"improper_key" => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], invalid, Cfg))),
    ok.

rf_handler() ->
    [{doc, "Test rf handler options"}].
rf_handler(_Config)  ->
    Cfg = ?RF_CONFIG,
    ValF = fun(Opts) -> ergw_aaa_config:validate_handler(ergw_aaa_rf, Opts) end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),

    ?bad(ValF(#{})),
    ?bad(ValF(set_cfg_value([invalid_option], #{}, Cfg))),

    % termination cause mapping tests config
    ?ok(ValF(set_cfg_value([termination_cause_mapping], #{normal => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{wrong_mapped_value => some_atom}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{"improper_key" => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], invalid, Cfg))),
    ok.

ro_handler() ->
    [{doc, "Test ro handler options"}].
ro_handler(_Config)  ->
    Cfg = ?RF_CONFIG,
    ValF = fun(Opts) -> ergw_aaa_config:validate_handler(ergw_aaa_ro, Opts) end,

    ?ok(ValF(Cfg)),
    ?ok(ValF(ValF(Cfg))),

    ?bad(ValF(#{})),
    ?bad(ValF(set_cfg_value([invalid_option], #{}, Cfg))),

    % termination cause mapping tests config
    ?ok(ValF(set_cfg_value([termination_cause_mapping], #{normal => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{wrong_mapped_value => some_atom}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], #{"improper_key" => 1}, Cfg))),
    ?bad(ValF(set_cfg_value([termination_cause_mapping], invalid, Cfg))),
    ok.

service() ->
    [{doc, "Test service options"}].
service(_Config)  ->
    %% load functions
    Functions =
	[{ergw_aaa_static, ?STATIC_CONFIG},
	 {ergw_aaa_radius, ?RADIUS_ACCT_CONFIG},
	 {ergw_aaa_nasreq, ?DIAMETER_CONFIG},
	 {ergw_aaa_rf, ?RF_CONFIG},
	 {ergw_aaa_ro, ?RF_CONFIG}],
    lists:foreach(
      fun({K, V}) ->
	      Opts = ergw_aaa_config:validate_handler(K, V),
	      ergw_aaa:add_config(handlers, K, Opts)
      end, Functions),

    ValF = fun ergw_aaa_config:validate_service/2,

    ?ok(ValF(<<"Default">>, [{handler, 'ergw_aaa_static'}])),
    ?ok(ValF(<<"RADIUS-Service">>, [{handler, 'ergw_aaa_radius'}])),
    ?ok(ValF(<<"DIAMETER-Service">>, [{handler, 'ergw_aaa_nasreq'}])),
    ?ok(ValF(<<"RF-Service">>, maps:put(handler, 'ergw_aaa_rf', ?RF_CONFIG))),
    ?ok(ValF(<<"RO-Service">>, maps:put(handler, 'ergw_aaa_ro', ?RF_CONFIG))),

    ?bad(ValF(<<"RADIUS-Service">>, [{handler, 'invalid_handler'}])),
    ?bad(ValF(<<"RADIUS-Service">>, ?RADIUS_AUTH_CONFIG)),
    ?ok(ValF(<<"RADIUS-Service">>,
	     maps:put(handler, 'ergw_aaa_radius', ?RADIUS_AUTH_CONFIG))),


    ?bad(ValF(<<"DIAMETER-Service">>, [{handler, 'invalid_handler'}])),
    ?bad(ValF(<<"DIAMETER-Service">>, ?DIAMETER_CONFIG)),
    ?ok(ValF(<<"DIAMETER-Service">>,
	     maps:put(handler, 'ergw_aaa_nasreq', ?DIAMETER_CONFIG))),

    %% make sure the handler config is passed through to the service
    ?match_map(?STATIC_CONFIG, ValF(<<"Default">>, [{handler, 'ergw_aaa_static'}])),
    ?match_map(?DIAMETER_CONFIG, ValF(<<"DIAMETER-Service">>, [{handler, 'ergw_aaa_nasreq'}])),
    ?match_map(?RADIUS_ACCT_CONFIG, ValF(<<"RADIUS-Service">>, [{handler, 'ergw_aaa_radius'}])),
    ok.

app() ->
    [{doc, "Test the app options"}].
app(_Config)  ->
    %% load functions
    Functions =
	[{ergw_aaa_static, ?STATIC_CONFIG},
	 {ergw_aaa_radius, ?RADIUS_ACCT_CONFIG},
	 {ergw_aaa_nasreq, ?DIAMETER_CONFIG},
	 {ergw_aaa_rf, ?RF_CONFIG},
	 {ergw_aaa_ro, ?RF_CONFIG}],
    lists:foreach(
      fun({K, V}) ->
	      Opts = ergw_aaa_config:validate_handler(K, V),
	      ergw_aaa:add_config(handlers, K, Opts)
      end, Functions),

    %% load services
    Services =
	[{<<"Default">>,          #{handler => 'ergw_aaa_static'}},
	 {<<"RADIUS-Service">>,   #{handler => 'ergw_aaa_radius'}},
	 {<<"DIAMETER-Service">>, #{handler => 'ergw_aaa_nasreq'}},
	 {<<"RF-Service">>,       maps:put(handler, 'ergw_aaa_rf', ?RF_CONFIG)},
	 {<<"RO-Service">>,       maps:put(handler, 'ergw_aaa_ro', ?RF_CONFIG)}],
    lists:foreach(
      fun({K, V}) ->
	      Opts = ergw_aaa_config:validate_service(K, V),
	      ergw_aaa:add_config(services, K, Opts)
      end, Services),

    RADIUS =
    [
        {'Origin-Host', <<"dummy.host">>},
        {procedures, [
	        {init, [#{service => <<"Default">>}]},
	        {authenticate, [maps:merge(?RADIUS_AUTH_CONFIG, ?RADIUS_SERVICE_OPTS),
		                    #{service => <<"Default">>, defaults => #{'TLS-Pre-Shared-Key' => <<"secret">>}}]},
	        {authorize, []},
	        {start, []},
	        {interim, []},
	        {stop, []}
        ]}
    ],
    DIAMETER =
	#{procedures =>
	    #{init => [#{service => <<"Default">>}],
	      authenticate => [?DIAMETER_SERVICE_OPTS],
	      authorize => [],
	      start => [],
	      interim => [],
	      stop => []}
	},
    ValF = fun ergw_aaa_config:validate_app/2,

    ?ok(ValF(<<"RADIUS-Application">>, RADIUS)),
    ?ok(ValF(<<"RADIUS-Application">>, ValF(<<"RADIUS-Application">>, RADIUS))),

    %% make sure the handler config is also passed through to the session
    #{procedures := RadiusOpts} = ValF(<<"RADIUS-Application">>, RADIUS),
    ?match_map(?STATIC_CONFIG, get_cfg_value([init, 1], RadiusOpts)),
    ?match_map(maps:merge(?RADIUS_SERVICE_OPTS, ?RADIUS_AUTH_CONFIG),
	       get_cfg_value([authenticate, 1], RadiusOpts)),

    ?ok(ValF(<<"DIAMETER-Application">>, DIAMETER)),
    ?ok(ValF(<<"DIAMETER-Application">>, ValF(<<"DIAMETER-Application">>, DIAMETER))),

    %% make sure the handler config is also passed through to the session
    #{procedures := DiameterOpts} = ValF(<<"DIAMETER-Application">>, DIAMETER),
    ?match_map(maps:merge(?DIAMETER_CONFIG, ?DIAMETER_SERVICE_OPTS),
	       get_cfg_value([authenticate, 1], DiameterOpts)),
    ok.
