%% Copyright 2017, Travelping GmbH <info@travelping.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(ergw_aaa_gx).

-compile({parse_transform, cut}).

%% AAA API
-export([validate_options/1, initialize_api/1, call/1, call/3, cast/1]).

%%
%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4, pick_peer/5,
         prepare_request/3, prepare_request/4,
         prepare_retransmit/3, prepare_retransmit/4,
         handle_answer/4, handle_answer/5,
         handle_error/3,
         handle_error/4, handle_error/5,
         handle_request/3]).

-export([stop/0]).

-import(ergw_aaa_session, [attr_get/2, attr_get/3, attr_set/3,
			   attr_append/3, attr_fold/3, merge/2, to_session/1]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").
-include("include/diameter_3gpp_ts29_212.hrl").
-include("include/ergw_aaa_gx.hrl").

-include("include/ergw_aaa_profile.hrl").
-include("include/ergw_aaa_variable.hrl").

-define(APP, diameter_gx).
-define(SERVER, ?MODULE).
-define(SERVICE, ?MODULE).

-define(DIAMETER_APP_ID_GX, diameter_3gpp_ts29_212:id()).
-define(DIAMETER_DICT_GX, diameter_3gpp_ts29_212).

-define(VENDOR_ID_3GPP, 10415).
-define(VENDOR_ID_ETSI, 13019).
-define(VENDOR_ID_TP,   18681).

-define(Start, 2).
-define(Interim, 3).
-define(Stop, 4).

-define(DefaultOptions, [{host, undefined},
                         {realm, undefined},
                         {connect_to, undefined}
                        ]).

%%===================================================================
%% API
%%===================================================================

initialize_api(Opts) ->
    {OriginHost, Addr} = proplists:get_value(host, Opts),
    OriginRealm = proplists:get_value(realm, Opts),
    SvcOpts = [{'Origin-Host', OriginHost},
               {'Origin-Realm', OriginRealm},
               {'Origin-State-Id', diameter:origin_state_id()},
               {'Host-IP-Address', [Addr]},
               {'Vendor-Id', ?VENDOR_ID_TP},
               {'Product-Name', "erGW-AAA"},
               {'Supported-Vendor-Id', [?VENDOR_ID_3GPP,
                                        ?VENDOR_ID_ETSI,
                                        ?VENDOR_ID_TP]},
	       {'Auth-Application-Id', [?DIAMETER_APP_ID_GX]},
	       {'Vendor-Specific-Application-Id',
		[#'diameter_base_Vendor-Specific-Application-Id'{
		    'Vendor-Id'           = ?VENDOR_ID_3GPP,
		    'Auth-Application-Id' = [?DIAMETER_APP_ID_GX]}]},
               {string_decode, false},
	       {application, [{alias, ?APP},
			      {dictionary, ?DIAMETER_DICT_GX},
                              {module, ?MODULE}]}],
    ok = diameter:start_service(?SERVICE, SvcOpts),

    #diameter_uri{type = _AAA, % aaa | aaas
                  fqdn = Host,
                  port = Port,
                  transport = Transport,
                  protocol = _Diameter} = proplists:get_value(connect_to, Opts),
    {ok, {Raddr, Type}} = resolve_hostname(Host),
    TransportOpts = [{transport_module, transport_module(Transport)},
                     {transport_config, [{reuseaddr, true},
                                         {raddr, Raddr},
                                         {rport, Port},
                                         Type
                                        ]}],
    {ok, _} = diameter:add_transport(?SERVICE, {connect, TransportOpts}),

    ChildSpec = ergw_aaa_diameter_srv:child_spec(),
    {ok, [ChildSpec]}.

validate_options(Opts) ->
    ergw_aaa_config:validate_options(fun validate_option/2, Opts, ?DefaultOptions).

% will be used in tests
stop() ->
    diameter:stop_service(?SERVICE),
    diameter:remove_transport(?SERVICE, true).

call(Request) ->
    exec(Request, []).

call(Request, Mod, Opts) ->
    exec(Request, [detach, {extra, [{cb, Mod, Opts}]}]).

cast(Request) ->
    exec(Request, [detach]).

to_map(Msg)
  when element(1, Msg) =:= 'diameter_base_answer-message' ->
    try msg2map(Msg, 'diameter_gen_base_rfc6733')
    catch
	error:badarg ->
	    msg2map(Msg, 'diameter_gen_base_rfc3588')
    end;
to_map(Msg) ->
    msg2map(Msg, ?DIAMETER_DICT_GX).

%%===================================================================
%% DIAMETER handler callbacks
%%===================================================================

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    lager:debug("peer_up: ~p~n", [_Peer]),
    State.

%% peer_down/3

peer_down(_SvcName, {PeerRef, _} = _Peer, State) ->
    ergw_aaa_diameter_srv:peer_down(?SERVICE, PeerRef),
    lager:debug("peer_down: ~p~n", [_Peer]),
    State.

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

pick_peer(LocalCandidates, RemoteCandidates, SvcName, State, _Extra) ->
    pick_peer(LocalCandidates, RemoteCandidates, SvcName, State).

prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]}, _, {PeerRef, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR},
		   origin_state_id = {OSid, _}} = Caps,

    SF = case catch(ergw_aaa_diameter_srv:is_first_request(?SERVICE, PeerRef)) of
	     false ->
		 [];
	     _ ->
		 [[{'Vendor-Id',       diameter_3gpp_base:vendor_id()},
		   {'Feature-List-ID', 1},
		   {'Feature-List',    ?'DIAMETER_GX_FEATURE-ID-1_Rel8' bor
			               ?'DIAMETER_GX_FEATURE-ID-1_Rel9' bor
			               ?'DIAMETER_GX_FEATURE-ID-1_Rel10'}
		    ]]
	 end,
    Msg = [T, {'Origin-Host', OH},
	      {'Origin-Realm', OR},
	      {'Origin-State-Id', OSid},
              {'Destination-Host', [DH]},
              {'Destination-Realm', DR},
	      {'Supported-Features', SF}
	   | Avps],
    lager:debug("prepare_request Msg: ~p", [Msg]),
    {send, Msg};

prepare_request(#diameter_packet{msg = Rec0}, _, {PeerRef, Caps})
  when is_record(Rec0, diameter_gx_CCR) ->

    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR},
		   origin_state_id = {OSid, _}} = Caps,

    SF = case catch(ergw_aaa_diameter_srv:is_first_request(?SERVICE, PeerRef)) of
	     false ->
		 [];
	     _ ->
		 [#'diameter_gx_Supported-Features'{
		     'Vendor-Id'       = diameter_3gpp_base:vendor_id(),
		     'Feature-List-ID' = 1,
		     'Feature-List'    =
			 ?'DIAMETER_GX_FEATURE-ID-1_Rel8' bor
			 ?'DIAMETER_GX_FEATURE-ID-1_Rel9' bor
			 ?'DIAMETER_GX_FEATURE-ID-1_Rel10'
		    }]
	 end,

    Rec1 = Rec0#diameter_gx_CCR{'Origin-Host' = OH,
				'Origin-Realm' = OR,
				'Destination-Host' = [DH],
				'Destination-Realm' = DR,
				'Origin-State-Id' = OSid,
				'Supported-Features' = SF},
    lager:debug("prepare_request Rec: ~p", [lager:pr(Rec1, ?MODULE)]),
    {send, Rec1};

prepare_request(Packet, _SvcName, {PeerRef, _}) ->
    lager:debug("prepare_request to ~p: ~p", [PeerRef, lager:pr(Packet, ?MODULE)]),
    {send, Packet}.

prepare_request(Packet, SvcName, Peer, _Extra) ->
    prepare_request(Packet, SvcName, Peer).

%% prepare_retransmit/3

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

prepare_retransmit(Packet, SvcName, Peer, _Extra) ->
    prepare_retransmit(Packet, SvcName, Peer).

%% handle_answer/4

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, {PeerRef, _}) ->
    lager:debug("handle_answer to ~p: ~p", [PeerRef, lager:pr(Msg, ?MODULE)]),
    {ok, (catch to_map(Msg))}.

handle_answer(Packet, Request, _SvcName, Peer, {cb, Mod, Opts}) ->
    apply(Mod, handle_answer, [Packet, Request, ?MODULE, Peer] ++ Opts);
handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, {PeerRef, _}, _Extra) ->
    lager:debug("handle_answer to ~p: ~p", [PeerRef, lager:pr(Msg, ?MODULE)]),
    ok.

%% handle_error/3

handle_error(Reason, Request, {cb, Mod, Opts}) ->
    apply(Mod, handle_error, [Reason, Request, ?MODULE, unknown] ++ Opts);
handle_error(_Reason, _Request, _Extra) ->
    ok.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

handle_error(Reason, Request, _SvcName, Peer, {cb, Mod, Opts}) ->
    apply(Mod, handle_error, [Reason, Request, ?MODULE, Peer] ++ Opts);
handle_error(Reason, Request, SvcName, Peer, _Extra) ->
    handle_error(Reason, Request, SvcName, Peer).

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).

%%%===================================================================
%%% Options Validation
%%%===================================================================

validate_option(connect_to = Opt, Value) when is_binary(Value) ->
    try
        #diameter_uri{} = decode_diameter_uri(Value)
    catch _:_ -> validate_option_error(Opt, Value)
    end;
validate_option(host = Opt, Value) when is_binary(Value) ->
    try
        {ok, {Addr, _Type}} = resolve_hostname(Value),
        {Value, Addr}
    catch _:_ -> validate_option_error(Opt, Value)
    end;
validate_option(realm, Value) when is_binary(Value) ->
    Value;
validate_option(Opt, Value) ->
    validate_option_error(Opt, Value).

validate_option_error(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

resolve_hostname(Name) when is_binary(Name) -> resolve_hostname(binary_to_list(Name));
resolve_hostname(Name) ->
    Name1 = case inet:gethostbyname(Name, inet6) of
        {error, nxdomain} -> inet:gethostbyname(Name, inet);
        Other -> Other
    end,
    case Name1 of
        {ok, #hostent{h_addr_list = [LocalIP | _], h_addrtype = Type}} ->
            {ok, {LocalIP, Type}};
        _ -> erlang:error(badarg, Name)
    end.

decode_diameter_uri(Value) ->
    Module = diameter_types, % trick to stop xref complains about undef function
    try
        apply(Module, 'DiameterURI', [decode, Value, #{rfc => 6733}]) % OTP 20
    catch _:_ -> apply(Module, 'DiameterURI', [decode, Value])
    end.

transport_module(tcp) -> diameter_tcp;
transport_module(sctp) -> diameter_sctp;
transport_module(_) -> unknown.

%%===================================================================
%% internal helpers
%%===================================================================

exec(Request, Opt) ->
    case diameter:call(?SERVICE, ?APP, Request, Opt) of
        ok ->
            ok;
        {ok, _} = Answer ->
            Answer;
        {error, Reason} = Error ->
            case proplists:get_bool(detach, Opt) of
                true ->
                    handle_error(Reason, Request, proplists:get_value(extra, Opt, [])),
                    ok;
                _ ->
                    Error
            end
    end.

to_map({_K, []}, M, _Dict) ->
    M;
to_map({K, V}, M, Dict)
  when is_tuple(V), is_atom(element(1, V)) ->
    M#{K => to_map(V, Dict)};
to_map({K, [V]}, M, Dict) ->
    M#{K => to_map(V, Dict)};
to_map({K, [_|_] = V}, M, Dict) ->
    M#{K => lists:map(fun (Item) -> to_map(Item, Dict) end, V)};
to_map({K, V}, M, _Dict) ->
    M#{K => V}.

to_map(Rec, Dict)
  when is_tuple(Rec) ->
    L = Dict:'#get-'(Rec),
    lists:foldl(to_map(_, _, Dict), #{}, tl(L));
to_map(Other, _Dict) ->
    Other.

msg2map(Rec, Dict) ->
    {Dict:rec2msg(element(1, Rec)), to_map(Rec, Dict)}.
