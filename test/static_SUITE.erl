%% Copyright 2017,2018, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(static_SUITE).

%% Common Test callbacks
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include_lib("gtplib/include/gtp_packet.hrl").
-include("../include/diameter_3gpp_ts29_212.hrl").
-include("../include/diameter_3gpp_ts32_299.hrl").
-include("../include/ergw_aaa_session.hrl").
-include("ergw_aaa_test_lib.hrl").

-define(HUT, ergw_aaa_static).

-define(STATIC_CONFIG,
	#{defaults =>
	      #{'NAS-Identifier'          => <<"NAS">>,
		'Framed-Protocol'         => 'PPP',
		'Service-Type'            => 'Framed-User',
		'Node-Id'                 => <<"PGW-001">>,
		'Charging-Rule-Base-Name' => <<"m2m0001">>}}).

-define(CONFIG,
	#{handlers =>
	      #{ergw_aaa_static => ?STATIC_CONFIG},
	  services =>
	      #{<<"Default">> =>
		    #{handler => 'ergw_aaa_static',
		      answers =>
			  #{<<"Initial-Gy">> =>
				#{avps =>
				      #{'Result-Code' => 2001,
					'Multiple-Services-Credit-Control' =>
					    [#{'Envelope-Reporting' => [0],
					       'Granted-Service-Unit' =>
						   [#{'CC-Time' => [3600],
						      'CC-Total-Octets' => [102400]}],
					       'Rating-Group' => [3000],
					       'Result-Code' => [2001],
					       'Time-Quota-Threshold' => [60],
					       'Volume-Quota-Threshold' => [10240]}
					    ]
				       }
				 },
			    <<"Update-Gy">> =>
				#{avps => #{'Result-Code' => 5003}},
			    <<"Initial-Gx">> =>
				#{avps =>
				      #{'Result-Code' => 2001,
					'Charging-Rule-Install' =>
					    [#{'Charging-Rule-Definition' =>
						   [#{'Charging-Rule-Name' => <<"m2m-gx">>,
						      'Rating-Group' => [3000],
						      'Flow-Information' =>
							  [#{'Flow-Description' => [<<"permit out ip from any to assigned">>],
							     'Flow-Direction'   => [1]    %% DownLink
							    },
							   #{'Flow-Description' => [<<"permit out ip from any to assigned">>],
							     'Flow-Direction'   => [2]    %% UpLink
							    }],
						      'Metering-Method'  => [1],
						      'Precedence' => [100]
						     }],
					       'Charging-Rule-Name' =>
						   [<<"m2m-r0001">>, <<"m2m-r0001">>],
					       'Charging-Rule-Base-Name' =>
						   [<<"m2m0001">>]
					      }
					    ]
				       }
				 },
			    <<"Update-Gx">> =>
				#{avps =>
				      #{'Result-Code' => 2001,
					'Charging-Rule-Remove' =>
					    [#{'Charging-Rule-Name' =>
						   [<<"m2m-r0001">>],
					       'Charging-Rule-Base-Name' =>
						   [<<"m2m0001">>]
					      }
					    ]
				       }
				 },
			    <<"Final-Gx">> =>
				#{avps => #{'Result-Code' => 5003}}
			   }
		     }
	       },

	  apps =>
	      #{default =>
		    #{init => [#{service => <<"Default">>}],
		      authenticate => [],
		      authorize => [],
		      start => [],
		      interim => [],
		      stop => [],
		      {gx, 'CCR-Initial'}   => [#{service => <<"Default">>, answer => <<"Initial-Gx">>}],
		      {gx, 'CCR-Update'}    => [#{service => <<"Default">>, answer => <<"Update-Gx">>}],
		      {gx, 'CCR-Terminate'} => [#{service => <<"Default">>, answer => <<"Final-Gx">>}],
		      {gy, 'CCR-Initial'}   => [#{service => <<"Default">>, answer => <<"Initial-Gy">>}],
		      {gy, 'CCR-Update'}    => [#{service => <<"Default">>, answer => <<"Update-Gy">>}],
		      {gy, 'CCR-Terminate'} => []
		      }
	       }
	 }).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [gx_session, gy_session].

init_per_suite(Config0) ->
    Config = [{handler_under_test, ?HUT} | Config0],

    application:load(ergw_aaa),
    ergw_aaa_test_lib:clear_app_env(),

    meck_init(Config),

    {ok, _} = application:ensure_all_started(ergw_aaa),
    ergw_aaa_test_lib:ergw_aaa_init(?CONFIG),

    Config.

end_per_suite(Config) ->
    meck_unload(Config),
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    ok.

init_per_testcase(Config) ->
    meck_reset(Config),
    Config.

end_per_testcase(_Config) ->
    ok.

%%%===================================================================
%%% Helper
%%%===================================================================

init_session(Session, _Config) ->
    Defaults =
	#{
	  '3GPP-GGSN-Address'       => {172,20,16,28},
	  '3GPP-IMEISV'             => <<82,21,50,96,32,80,30,0>>,
	  '3GPP-IMSI'               => <<"250071234567890">>,
	  '3GPP-Charging-Id'        => 3604013806,
	  '3GPP-IMSI-MCC-MNC'       => <<"25999">>,
	  '3GPP-GGSN-MCC-MNC'       => <<"25888">>,
	  '3GPP-MS-TimeZone'        => {128,1},
	  '3GPP-MSISDN'             => <<"46702123456">>,
	  '3GPP-NSAPI'              => 5,
	  '3GPP-PDP-Type'           => 'IPv4',
	  '3GPP-RAT-Type'           => 6,
	  '3GPP-SGSN-Address'       => {192,168,1,1},
	  '3GPP-SGSN-MCC-MNC'       => <<"26201">>,
	  '3GPP-Selection-Mode'     => 0,
	  'User-Location-Info' =>
	      #{'ext-macro-eNB' =>
		    #ext_macro_enb{plmn_id = {<<"001">>, <<"001">>},
				   id = rand:uniform(16#1fffff)},
		'TAI' =>
		    #tai{plmn_id = {<<"001">>, <<"001">>},
			 tac = rand:uniform(16#ffff)}},
	  'Called-Station-Id'       => <<"some.station.gprs">>,
	  'Calling-Station-Id'      => <<"543148000012345">>,
	  'Framed-IP-Address'       => {10,106,14,227},
	  'Framed-Protocol'         => 'GPRS-PDP-Context',
	  'Multi-Session-Id'        => 1012552258277823040188863251876666193415858290601,
	  'Username'                => <<"ergw">>,
	  'Password'                => <<"ergw">>,
	  'Service-Type'            => 'Framed-User',
	  'Node-Id'                 => <<"PGW-001">>,
	  'PDP-Context-Type'        => primary,
	  'Charging-Rule-Base-Name' => <<"m2m0001">>,

	  '3GPP-GPRS-Negotiated-QoS-Profile' =>   <<11,146,31,147,150,64,64,255,
						    255,255,255,17,1,1,64,64>>,
	  '3GPP-Allocation-Retention-Priority' => 2,
	  '3GPP-Charging-Characteristics' =>  <<8,0>>,

	  'QoS-Information' =>
	      #{
		'QoS-Class-Identifier' => 8,
		'Max-Requested-Bandwidth-DL' => 0,
		'Max-Requested-Bandwidth-UL' => 0,
		'Guaranteed-Bitrate-DL' => 0,
		'Guaranteed-Bitrate-UL' => 0,
		'Allocation-Retention-Priority' =>
		    #{'Priority-Level' => 10,
		      'Pre-emption-Capability' => 1,
		      'Pre-emption-Vulnerability' => 0},
		'APN-Aggregate-Max-Bitrate-DL' => 84000000,
		'APN-Aggregate-Max-Bitrate-UL' => 8640000
	       }
	 },
    maps:merge(Defaults, Session).

%%%===================================================================
%%% Test cases
%%%===================================================================

gx_session() ->
    [{doc, "Simple Gx session"}].
gx_session(Config) ->
    Session = init_session(#{}, Config),
    GxOpts = #{'Event-Trigger' => ?'DIAMETER_GX_EVENT-TRIGGER_UE_IP_ADDRESS_ALLOCATE',
	       'Bearer-Operation' => ?'DIAMETER_GX_BEARER-OPERATION_ESTABLISHMENT'},

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, Session1, Events1} =
	ergw_aaa_session:invoke(SId, GxOpts, {gx, 'CCR-Initial'}, []),
    ?match([{pcc, install, [#{}]}], Events1),
    ?equal(false, maps:is_key('Result-Code', Session1)),

    GxUpd = #{'Event-Trigger' => ?'DIAMETER_GX_EVENT-TRIGGER_SGSN_CHANGE',
	      'Bearer-Operation' => ?'DIAMETER_GX_BEARER-OPERATION_MODIFICATION'},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GxUpd, {gx, 'CCR-Update'}, []),
    ?match([{pcc, remove, [#{}]}], Events2),
    ?equal(false, maps:is_key('Result-Code', Session2)),

    GxTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       'Event-Trigger' => ?'DIAMETER_GX_EVENT-TRIGGER_UE_IP_ADDRESS_RELEASE',
	       'Bearer-Operation' => ?'DIAMETER_GX_BEARER-OPERATION_TERMINATION'},
    {Result3, Session3, Events3} =
	ergw_aaa_session:invoke(SId, GxTerm, {gx, 'CCR-Terminate'}, []),
    ?equal({fail, 5003}, Result3),
    ?match([{stop, {gx, peer_reject}}], Events3),
    ?equal(false, maps:is_key('Result-Code', Session3)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.

gy_session() ->
    [{doc, "Simple Gy session"}].
gy_session(Config) ->
    Session = init_session(#{}, Config),
    GyOpts = #{credits => #{3000 => empty}},

    {ok, SId} = ergw_aaa_session_sup:new_session(self(), Session),
    {ok, Session1, Events1} =
	ergw_aaa_session:invoke(SId, GyOpts, {gy, 'CCR-Initial'}, []),
    ?match([{update_credits,
	     [#{'Envelope-Reporting' := [0],
		'Granted-Service-Unit' :=
		    [#{'CC-Time' := [3600],
		       'CC-Total-Octets' := [102400]}],
		'Rating-Group' := [3000],
		'Result-Code' := [2001],
		'Time-Quota-Threshold' := [60],
		'Volume-Quota-Threshold' := [10240]}]}], Events1),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session1)),

    UsedCredits =
	#{3000 => #{'CC-Input-Octets'  => [1092],
		    'CC-Output-Octets' => [0],
		    'CC-Time'          => [60],
		    'CC-Total-Octets'  => [1092],
		    'Reporting-Reason' => [?'DIAMETER_3GPP_CHARGING_REPORTING-REASON_FINAL']}
	 },
    GyTerm = #{'Termination-Cause' => ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT',
	       used_credits => maps:to_list(UsedCredits)},
    {ok, Session2, Events2} =
	ergw_aaa_session:invoke(SId, GyTerm, {gy, 'CCR-Terminate'}, []),
    ?match([], Events2),
    ?equal(false, maps:is_key('Multiple-Services-Credit-Control', Session2)),

    %% make sure nothing crashed
    meck_validate(Config),
    ok.
