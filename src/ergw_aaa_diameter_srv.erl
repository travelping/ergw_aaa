%% Copyright 2017-2019, Travelping GmbH <info@travelping.com>
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

-module(ergw_aaa_diameter_srv).

-compile({parse_transform, cut}).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([peer_down/3, is_first_request/3,
	 register_service/2, get_service_opts/1]).
-export([pick_peer/2, pick_peer/3,
	 start_request/2,
	 start_request/3,
	 finish_request/2,
	 send_request/4, retry_request/4, await_response/1, await_response/2,
	 prepare_request/4, get_peers_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(TEST).
-export([peers/0]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("include/ergw_aaa_session.hrl").
-include("ergw_aaa_internal.hrl").

-define(SERVER, ?MODULE).
-define(REQ_LIMIT_TAB, ergw_aaa_diameter_limiter).

-record(state, {
		handlers,
		peers = #{}
	       }).

-define(LoadBuckets, 20).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

peer_down(API, SvcName, {PeerRef, _} = Peer) ->
    (catch ets:delete(?MODULE, {API, PeerRef})),
    gen_server:cast(?SERVER, {peer_down, SvcName, Peer}),
    ok.

is_first_request(API, _SvcName, {PeerRef, _}) ->
    case ets:lookup(?MODULE, {API, PeerRef}) of
	[] ->
	    gen_server:call(?SERVER, {is_first_request, API, PeerRef});
	[_] ->
	    false
    end.

register_service(Service, SvcOpts) ->
    gen_server:call(?SERVER, {register_service, Service, SvcOpts}).

get_service_opts(Service) ->
    gen_server:call(?SERVER, {get_services, Service}).

pick_peer(Candidates, SvcName) ->
    gen_server:call(?SERVER, {pick_peer, Candidates, SvcName}).

pick_peer(Candidates, SvcName, #diam_call{peers_tried = ExceptPeers}) ->
    gen_server:call(?SERVER, {pick_peer, Candidates -- ExceptPeers, SvcName}).

%% start_request/2
start_request(SvcName, Peer) ->
    gen_server:call(?SERVER, {start_request, SvcName, Peer, self()}).

%% start_request/3
start_request(Msg, SvcName, Peer) ->
    case start_request(SvcName, Peer) of
	ok    -> {send, Msg};
	Other -> Other
    end.

finish_request(SvcName, Peer) ->
    gen_server:call(?SERVER, {finish_request, SvcName, Peer, self()}).

get_peers_info() ->
    gen_server:call(?SERVER, peers_info).

-ifdef(TEST).
peers() ->
    gen_server:call(?SERVER, '$peers').
-endif.

%%%===================================================================
%%% diameter:call wrapper
%%%===================================================================

%% send_request/4
send_request(Handler, App, Request, Config) ->
    Promise = make_ref(),
    CallOpts0 =
	#diam_call{
	   seqno = diameter_session:sequence(),
	   tries = maps:get(max_retries, Config, 0) + 1,
	   opts = Config},
    From = self(),
    proc_lib:spawn(
      fun() ->
	      Result = (catch call(App, Request, Config, CallOpts0)),
	      From ! {'$reply', Promise, Handler, Result, Config}
      end),
    Promise.

await_response(Promise) ->
    await_response(Promise, infinity).

await_response(Promise, Timeout) ->
    receive
	{'$reply', Promise, _, Result, _} -> Result
    after Timeout -> timeout
    end.


handle_plain_error(Module, Error, Request, SvcName, CallOpts)
  when is_atom(Module) ->
    ?LOG(debug, "~p: DIAMETER error in ~0p with: ~0p. Request: ~0p, CallOpts: ~0p",
	 [self(), Module, Error, Request, CallOpts]),
    erlang:apply(Module, handle_error, [Error, Request, SvcName, undefined, CallOpts]);
handle_plain_error([Module | Extra], Error, Request, SvcName, CallOpts)
  when is_atom(Module) ->
    ?LOG(debug, "~p: DIAMETER error in ~0p with: ~0p. Request: ~0p, Extra: ~0p, CallOpts: ~0p",
	 [self(), Module, Error, Request, Extra, CallOpts]),
    erlang:apply(Module, handle_error,
		 [Error, Request, SvcName, undefined] ++ Extra ++ [CallOpts]).
%% we don't currently use #diameter_callback{}

handle_plain_error(Error, _Request, _SvcName, _App, _CallOpts, []) ->
    {error, Error};
handle_plain_error(Error, Request, SvcName, App, CallOpts, [SI|T]) ->
    case lists:member({alias, App}, SI) of
	true ->
	    Module = proplists:get_value(module, SI),
	    handle_plain_error(Module, Error, Request, SvcName, CallOpts);
	_ ->
	    handle_plain_error(Error, Request, SvcName, App, CallOpts, T)
    end.

%% call/4
call(App, Request, Config, #diam_call{tries = 0} = CallOpts) ->
    %% should not happen, but lets crash if it does to avoid endless recursion
    erlang:error(badarg, [App, Request, Config, CallOpts]);

call(App, Request, #{function := Function} = Config, CallOpts) ->
    Timeout = maps:get(tx_timeout, Config, 5000),
    Opts = [{timeout, Timeout}, {extra, [CallOpts]}],
    case diameter:call(Function, App, Request, Opts) of
	{retry, NextCallOpts} ->
	    call(App, Request, Config, NextCallOpts);
	{error, Error} = Result ->
	    case Error of
		encode ->
		    ?LOG(critical, "failed to encode DIAMETER requests: ~0p", [Request]);
		Other ->
		    ?LOG(notice, "non-critical diameter API return: ~p", [Other])
	    end,
	    SI = diameter:service_info(Function, applications),
	    handle_plain_error(Result, Request, Function, App, CallOpts, SI);
	Result ->
	    Result
    end.

%% retry_request/4
retry_request(Reason, _SvcName, _Peer, #diam_call{tries = 1}) ->
    {error, Reason};
retry_request(_, _, Peer, #diam_call{tries = Tries, peers_tried = PeersTried} = CallOpts) ->
    {retry, CallOpts#diam_call{tries = Tries - 1,
			       peers_tried = [Peer | PeersTried]}}.

%% prepare_request/4
prepare_request(#diameter_packet{header = Hdr0, msg = [Message | AVPs0]} = Packet, _SvcName, _Peer,
		#diam_call{peers_tried = PeersTried, seqno = SeqNo, opts = Config}) ->
    Filter = maps:get(avp_filter, Config, []),
    AVPs = filter_avps(AVPs0, Filter),
    Hdr = Hdr0#diameter_header{
	    is_retransmitted =
		Hdr0#diameter_header.is_retransmitted =:= true orelse PeersTried /= [],
	    end_to_end_id = SeqNo},
    Packet#diameter_packet{header = Hdr, msg = [Message | AVPs]}.

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [ordered_set, public, named_table, {keypos, 1}, {read_concurrency, true}]),
    {ok, #state{handlers = dict:new(), peers = #{}}}.

handle_call({is_first_request, API, PeerId}, _From, State) ->
    case ets:lookup(?MODULE, {API, PeerId}) of
	[] ->
	    MRef = erlang:monitor(process, PeerId),
	    ets:insert(?MODULE, {{API, PeerId}, MRef}),
	    {reply, true, State};
	_ ->
	    {reply, false, State}
    end;

handle_call({register_service, Service, SvcOpts}, _From, #state{handlers = H} = State) ->
    {reply, ok, State#state{handlers = dict:update(Service, [SvcOpts | _], [SvcOpts], H)}};

handle_call({get_services, Service}, _From, #state{handlers = H} = State) ->
    Reply = case dict:find(Service, H) of
		{ok, V} -> V;
		_       -> []
	    end,
    {reply, Reply, State};

handle_call('$peers', _From, #state{peers = Peers} = State) ->
    Reply = maps:fold(
	      fun(K, #peer{} = P, A) -> [setelement(1, P, K)|A];
		 (_, _, A)           -> A
	      end, [], Peers),
    {reply, Reply, State};

handle_call({pick_peer, Candidates, SvcName}, _From, #state{peers = Peers} = State) ->
    Reply = pick_peer_h(Candidates, SvcName, Peers),
    {reply, Reply, State};

handle_call({start_request, SvcName, Peer, RPid}, _From, #state{peers = Peers0} = State) ->
    {Reply, Peers} = start_request_h(SvcName, Peer, RPid, Peers0),
    {reply, Reply, State#state{peers = Peers}};

handle_call({finish_request, SvcName, Peer, RPid}, _From, #state{peers = Peers0} = State) ->
    Peers = finish_request_h(SvcName, Peer, RPid, Peers0),
    {reply, ok, State#state{peers = Peers}};

handle_call(peers_info, _, #state{peers = Peers} = State) ->
    {reply, Peers, State}.

handle_cast({peer_down, _SvcName, {PeerRef, _}}, #state{peers = Peers} = State) ->
    {noreply, State#state{peers = maps:remove(PeerRef, Peers)}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'DOWN', MRef, _Type, Pid, Info}, #state{peers = Peers0} = State)
  when is_map_key(MRef, Peers0) ->
    ?LOG(alert, "Diameter request process terminated unexpected with ~0tp (req: ~0tp, pid: ~0p)",
	 [Info, maps:get(MRef, Peers0), Pid]),

    {_, _, PeerRef, Caps} = maps:get(MRef, Peers0),
    Peers = release_peer(PeerRef, Caps, maps:without([Pid, MRef], Peers0)),
    ets:match_delete(?MODULE, {{'_', Pid}, MRef}),
    {noreply, State#state{peers = Peers}};

handle_info({'DOWN', MRef, _Type, Pid, _Info}, State) ->
    ets:match_delete(?MODULE, {{'_', Pid}, MRef}),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(alert, "unexpected info message, something important might have been missed: ~p",
         [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% https://medium.com/smyte/rate-limiter-df3408325846
update_bucket(#peer{last_ts = undefined, rate = Rate} = Peer) ->
    Peer#peer{
      last_ts = erlang:monotonic_time(microsecond),
      interval = floor(1000000 / Rate),
      tokens = Rate
     };
update_bucket(#peer{last_ts = Last, interval = Interval,
		    rate = Rate, tokens = Tokens} = Peer) ->
    Now = erlang:monotonic_time(microsecond),
    Refill = floor((Now - Last) / Interval),
    Peer#peer{
      last_ts = min(Now, Last + Refill * Interval),
      tokens = min(Rate, Tokens + Refill)
     }.

get_peer(Host, Peers) ->
    Peer =
	case maps:is_key(Host, Peers) of
	    true ->
		maps:get(Host, Peers);
	    _ ->
		#{outstanding_requests := Capacity, rate := Rate} =
		    case application:get_env(ergw_aaa, rate_limits, undefined) of
			Limits when is_map_key(Host, Limits) ->
			    maps:get(Host, Limits);
			#{default := Default} ->
			    Default;
			_ ->
			    #{outstanding_requests => 50, rate => 50}
		    end,
		#peer{capacity = Capacity, rate = Rate}
	end,
    update_bucket(Peer).

%% relative_connection_load/3
connection_load({PeerRef, _} = Peer, CM, Peers) ->
    Cnt = maps:get(PeerRef, Peers, 0),
    maps:update_with(-Cnt, [Peer|_], [Peer], CM).

%% pick_connection/2
pick_connection(Connections, Peers) ->
    CMap =
	lists:foldl(
	  connection_load(_, _, Peers), #{}, Connections),
    {_, Cands} = hd(lists:keysort(1, maps:to_list(CMap))),
    N = rand:uniform(length(Cands)),
    lists:nth(N, Cands).

%% relative_peer_load/4
relative_peer_load(Host, _, LM, Peers) ->
    Peer = get_peer(Host, Peers),
    #peer{outstanding = Cnt,
	  capacity    = Cap,
	  rate        = Rate,
	  tokens      = Tokens} = Peer,
    Load =
	erlang:floor((Cnt / Cap) * ?LoadBuckets) +
	erlang:floor(((1.0 - Tokens / Rate) * ?LoadBuckets)),
    maps:update_with(Load, [Host|_], [Host], LM).

%% pick_peer_h/3
pick_peer_h([], _SvcName, _Peers) ->
    false;
pick_peer_h(Candidates, _SvcName, Peers) ->
    CandMap = lists:foldl(
		fun({_, #diameter_caps{origin_host = {_, OH}}} = Peer, CM) ->
			maps:update_with(OH, [Peer|_], [Peer], CM)
		end, #{}, Candidates),
    LoadMap = maps:fold(relative_peer_load(_, _, _, Peers), #{}, CandMap),
    [{_, Cands} | _] = lists:keysort(1, maps:to_list(LoadMap)),
    N = rand:uniform(length(Cands)),
    Connection = pick_connection(maps:get(lists:nth(N, Cands), CandMap), Peers),
    {ok, Connection}.

inc_element(Element, Stats) ->
    %% inc with wrap around to 0 at 2^64
    setelement(Element, Stats, (element(Element, Stats) + 1) rem (1 bsl 64)).

inc_stats(no_tokens, #peer{stats = Stats} = Peer) ->
    Peer#peer{stats = inc_element(#peer_stats.no_tokens, Stats)};
inc_stats(no_capacity, #peer{stats = Stats} = Peer) ->
    Peer#peer{stats = inc_element(#peer_stats.no_capacity, Stats)}.

start_request_h(SvcName, {PeerRef,  #diameter_caps{origin_host = {_, OH}} = Caps}, RPid, Peers0) ->
    Peer = get_peer(OH, Peers0),
    ?LOG(debug, "start_request_h: ~p", [PeerRef]),
    #peer{outstanding = Cnt,
	  capacity    = Cap,
	  tokens      = Tokens} = Peer,
    if Cnt >= Cap ->
	    ?LOG(notice,
		 "Diameter peer ~0p traffic is over the configured outstanding request capacity (~p > ~p)",
		 [OH, Cnt, Cap]),
	    {{discard, rate_limit}, Peers0#{OH => inc_stats(no_capacity, Peer)}};
       Tokens == 0 ->
	    ?LOG(notice, "Diameter peer ~0p traffic is over the configured rate", [OH]),
	    {{discard, rate_limit}, Peers0#{OH => inc_stats(no_tokens, Peer)}};
       true ->
	    ?LOG(debug, "outstanding inc: ~p", [Cnt + 1]),
	    Peers1 =
		Peers0#{OH => Peer#peer{outstanding = Cnt + 1, tokens = Tokens - 1}},
	    Peers2 = maps:update_with(PeerRef, _ + 1, 1, Peers1),
	    MRef = monitor(process, RPid),
	    Peers3 = maps:put(MRef, {RPid, SvcName, PeerRef, Caps}, Peers2),
	    Peers4 = maps:put(RPid, MRef, Peers3),
	    {ok, Peers4}
    end.

release_peer_ref(PeerRef, Peers) when is_map_key(PeerRef, Peers) ->
    maps:update_with(PeerRef, _ - 1, Peers);
release_peer_ref(_PeerRef, Peers) ->
    Peers.

release_peer_oh(#diameter_caps{origin_host = {_, OH}}, Peers) ->
    Peer = get_peer(OH, Peers),
    case Peer of
	#peer{outstanding = Cnt} when Cnt > 0 ->
	    ?LOG(debug, "outstanding dec #1: ~p", [Cnt - 1]),
	    Peers#{OH => Peer#peer{outstanding = Cnt - 1}};
	_ ->
	    ?LOG(emergency, "reference counting underflow for Diameter peer ~0tp", [OH]),
	    Peers#{OH => Peer}
    end.

release_peer(PeerRef, Caps, Peers) ->
    release_peer_oh(Caps, release_peer_ref(PeerRef, Peers)).

finish_request_h(_SvcName, {PeerRef, Caps}, RPid, Peers0) ->
    ?LOG(debug, "finish_request_h: ~p", [PeerRef]),
    Peers =
	case Peers0 of
	    #{RPid := MRef} ->
		demonitor(MRef),
		maps:without([RPid, MRef], Peers0);
	    _ ->
		Peers0
	end,
    release_peer(PeerRef, Caps, Peers).

%% AVP filtering
%% See /docs/diameter_avp_filter.md in the source tree for functionality
filter_avps(AVPs, [AVPPath | Rest]) when is_list(AVPPath) ->
    filter_avps(filter_avp(AVPs, AVPPath), Rest);
filter_avps(AVPs, []) ->
    AVPs.

%% When conditions are the last element of the path, and we're filtering a
%% list of AVP structures. Delete all structures matching the conditions
filter_avp([#{}| _] = AVPs, [Conditions]) when is_list(Conditions) ->
    [M || M <- AVPs, not check_avp_conditions(M, Conditions)];
%% When conditions are not the last element of the path and we're filtering
%% a list of AVP structures. Return all non-matching and continue filtering
%% the matches.
filter_avp([#{}| _] = AVPs, AVPPath) ->
    [filter_avp(M, AVPPath) || M <- AVPs];
%% When AVP name is the last element of the path, and we're filtering a
%% single AVP structure, delete the key if present
filter_avp(#{} = AVPs, [Target]) when is_map_key(Target, AVPs) ->
    maps:without([Target], AVPs);
%% When the path element is an AVP name, there is more in the path and we
%% are filtering a single AVP structure, go a level down to continue filtering
filter_avp(#{} = AVPs, [Next | Rest]) when is_map_key(Next, AVPs) ->
    AVPs#{Next => filter_avp(maps:get(Next, AVPs), Rest)};
%% When conditions are not the last element of the path and we're filtering
%% a sigle AVP structure. Follow the path to filter if the conditions are
%% met, otherwise return the structure
filter_avp(#{} = AVPs, [Conditions, Rest]) when is_list(Conditions) ->
    case check_avp_conditions(AVPs, Conditions) of
	true ->
	    filter_avp(AVPs, Rest);
	false ->
	    AVPs
    end;
filter_avp(AVPs, _) ->
    AVPs.

check_avp_conditions(AVPs, [{AVP, Value} | Rest]) when is_map_key(AVP, AVPs) ->
    case maps:get(AVP, AVPs) of
	Value -> check_avp_conditions(AVPs, Rest);
	_ -> false
    end;
check_avp_conditions(_, []) ->
    true;
check_avp_conditions(_, _) ->
    false.
