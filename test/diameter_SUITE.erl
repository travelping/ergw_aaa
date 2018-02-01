%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(diameter_SUITE).

%% Common Test callbacks
-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% Test cases
-export([check_CER_CEA/1, accounting/1, acct_interim_interval/1, attrs_3gpp/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [check_CER_CEA,
     accounting,
     acct_interim_interval,
     attrs_3gpp].


init_per_suite(Config) ->
    DiameterOpts = [{nas_identifier, <<"NAS">>},
                    {host, <<"127.0.0.1">>},
                    {realm, <<"example.com">>},
                    {connect_to, <<"aaa://127.0.0.1:3868">>}
                   ],
    Opts = [ {default, {provider, ergw_aaa_diameter, DiameterOpts} } ],
    application:load(ergw_aaa),
    application:set_env(ergw_aaa, applications, Opts),

    diameter_test_server:start(),
    application:ensure_all_started(ergw_aaa),

    timer:sleep(100),
    Config.

end_per_suite(_Config) ->
    application:stop(ergw_aaa),
    application:unload(ergw_aaa),
    diameter_test_server:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

check_CER_CEA(_Config) ->
    Statistics = get_stats(),
    % check that client has sent CER
    1 = proplists:get_value({{0, 257, 1}, send}, Statistics),
    % check that client has received CEA
    1 = proplists:get_value({{0, 257, 0}, recv}, Statistics),
    ok.

accounting(_Config) ->
    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'Framed-IP-Address' => {10,10,10,10}}),
    success = ergw_aaa_session:authenticate(Session, #{}),
    ergw_aaa_session:start(Session, #{}),

    timer:sleep(100),
    Statistics = get_stats(),

    % check that client has sent ACR
    1 = proplists:get_value({{1, 271, 1}, send}, Statistics),
    % check that client has received ACA
    1 = proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, Statistics),
    ok.

% test diameter provider can reset interim interval 
% by data from ACA Acct-Interim-Interval
acct_interim_interval(_Config) ->
    Fun = fun(_, S) -> S end,
    {ok, Session} = ergw_aaa_session_sup:new_session(self(), #{'Accouting-Update-Fun' => Fun}),
    success = ergw_aaa_session:authenticate(Session, #{}),
    ergw_aaa_session:start(Session, #{}),

    timer:sleep(100),
    Count0 = proplists:get_value({{1, 271, 1}, send}, get_stats()),
    
    % wait a little to be sure that values is set in session
    timer:sleep(100),
    {state, _, _, _, _, _, _, _, SessionMap} = sys:get_state(Session),
    1000 = maps:get('Interim-Accounting', SessionMap),

    % In ACA we have Acct-Interim-Interval = 100
    % that means for 2 seconds at least 2 ACR Interim reqs will be sent.
    % We can check it via statistics counter.
    timer:sleep(2000),
    Count = proplists:get_value({{1, 271, 1}, send}, get_stats()),
    true = (2 =< Count - Count0),

    ok.

attrs_3gpp(_Config) ->
    Attrs = #{
      '3GPP-GGSN-Address'       => {199,255,4,125},
      '3GPP-IMEISV'             => <<82,21,50,96,32,80,30,0>>,
      '3GPP-IMSI'               => <<"250071234567890">>,
      '3GPP-Charging-ID'        => <<214, 208, 226, 238>>,
      '3GPP-IMSI-MCC-MNC'       => <<"25999">>,
      '3GPP-GGSN-MCC-MNC'       => <<"25888">>,
      '3GPP-MS-TimeZone'        => {128,1},
      '3GPP-NSAPI'              => 5,
      '3GPP-PDP-Type'           => 'IPv4',
      '3GPP-RAT-Type'           => 6,
      '3GPP-SGSN-Address'       => <<192,168,1,1>>,
      '3GPP-SGSN-MCC-MNC'       => <<"26201">>,
      '3GPP-SGSN-IPv6-Address'  => {100, 10, 10, 10},
      '3GPP-GGSN-IPv6-Address'  => {200, 10, 10, 10},
      '3GPP-Selection-Mode'     => 0,
      '3GPP-User-Location-Info' => <<24,98,242,16,64,163,98,242,16,1,156,232,0>>,
      'Called-Station-Id'       => <<"some.station.gprs">>,
      'Calling-Station-Id'      => <<"543148000012345">>,
      'Framed-IP-Address'       => {0,0,0,0},
      'Framed-Protocol'         => 'GPRS-PDP-Context',
      'Multi-Session-Id'        => 1012552258277823040188863251876666193415858290601,
      'Password'                => <<"ergw">>,
      'Service-Type'            => 'Framed-User',
      'Session-Id'              => 1012552258277823040188863251876666193415858290601,
      'Username'                => <<"ergw">>
     },

    Count0 = proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, get_stats()),

    {ok, Session} = ergw_aaa_session_sup:new_session(self(), Attrs),
    success = ergw_aaa_session:authenticate(Session, #{}),
    ergw_aaa_session:start(Session, #{}),

    timer:sleep(100),
    Count = proplists:get_value({{1, 271, 0}, recv, {'Result-Code',2001}}, get_stats()),
    1 = Count - Count0,

    ok.

get_stats() ->
    [Transport] = diameter:service_info(ergw_aaa_diameter, transport),
    proplists:get_value(statistics, Transport).
