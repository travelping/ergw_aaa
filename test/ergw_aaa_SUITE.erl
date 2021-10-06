%% Test ergw_aaa.erl

-module(ergw_aaa_SUITE).

%% Test cases
-export( [
	peer_info/1,
	peer_info_adjust/1
] ).

%% Common test callbacks
-export( [
	all/0,
	end_per_suite/1,
	init_per_suite/1
] ).

-include_lib("diameter/include/diameter.hrl").


all() -> [
	peer_info,
	peer_info_adjust
].

end_per_suite(_Config) ->
	application:stop(ergw_aaa),
	application:unload(ergw_aaa).

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(ergw_aaa),
	Config.

%% Test cases
peer_info( _Config ) ->
	Host = ahost,
	Peer = peer("peer_info", Host),
	ok = ergw_aaa_diameter_srv:start_request("SvcName", Peer),

	PeerInfo = ergw_aaa:peer_info(),

	#{Host := Info} = PeerInfo,
	#{capacity := _C, outstanding := _O} = Info.

peer_info_adjust(_Config) ->
	Host = ahost,
	Peer = peer("peer_info_adjust", Host),
	ok = ergw_aaa_diameter_srv:start_request("SvcName", Peer),
	#{Host := #{capacity := C, outstanding := O}} = ergw_aaa:peer_info(),
	Capacity = C + 1,
	Outstanding = O - 1,
	Expected = #{capacity => Capacity, outstanding => Outstanding},
	New = #{Host => #{capacity => Capacity, outstanding => Outstanding}},

	ergw_aaa:peer_info_adjust(New),

	#{Host := Expected} = ergw_aaa:peer_info().

%%===================================================================
%% Internal
%%===================================================================

peer(Name, Host) -> {Name, #diameter_caps{origin_host = {ignore, Host}}}.
