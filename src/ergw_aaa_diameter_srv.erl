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
	 call/5, retry/5,
	 prepare_request/4,
	 handle_answer/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("diameter/include/diameter.hrl").

-define(SERVER, ?MODULE).
-define(REQ_LIMIT_TAB, ergw_aaa_diameter_limiter).

-record(state, {
		handlers,
		peers = #{}
	       }).
-record(peer, {
	       outstanding = 0,
	       capacity    = 50,
	       last_ts     = undefined,
	       rate        = 10,                    %% requests per second
	       interval    = 0,                     %% refill interval in milliseconds
	       tokens      = 0
	      }).
-record(diam_call,{
		   session = #{},
		   events = [],
		   seqno,
		   tries,
		   peers_tried = [],
		   opts,
		   last_failure
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
    gen_server:call(?SERVER, {start_request, SvcName, Peer}).

%% start_request/3
start_request(Msg, SvcName, Peer) ->
    case start_request(SvcName, Peer) of
	ok    -> {send, Msg};
	Other -> Other
    end.

finish_request(SvcName, Peer) ->
    gen_server:call(?SERVER, {finish_request, SvcName, Peer}).

%%%===================================================================
%%% diameter:call wrapper
%%%===================================================================

%% call/5
call(App, Request, Session, Events, Config) ->
    CallOpts0 =
	#diam_call{
	   session = Session,
	   seqno = diameter_session:sequence(),
	   tries = maps:get(max_retries, Config, 0) + 1,
	   opts = Config},
    case maps:get(async, Config, false) of
	false ->
	    CallOpts = CallOpts0#diam_call{events = Events},
	    call(App, Request, Config, CallOpts);
	true ->
	    From = self(),
	    proc_lib:spawn(
	      fun() ->
		      Result = call(App, Request, Config, CallOpts0),
		      ergw_aaa_session:handle_answer(From, Result)
	      end),
	    {ok, Session}
    end.

handle_plain_error(Module, Error, Request, SvcName, CallOpts)
  when is_atom(Module) ->
    erlang:apply(Module, handle_error, [Error, Request, SvcName, undefined, CallOpts]);
handle_plain_error([Module | Extra], Error, Request, SvcName, CallOpts)
  when is_atom(Module) ->
    erlang:apply(Module, handle_error,
		 [Error, Request, SvcName, undefined] ++ Extra ++[CallOpts]).
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
	{error, _} = Result ->
	    SI = diameter:service_info(Function, applications),
	    handle_plain_error(Result, Request, Function, App, CallOpts, SI);
	Result ->
	    Result
    end.

%% retry/5
retry(Fun, Reason, _SvcName, _Peer,
      #diam_call{session = Session, events = Events, opts = Opts, tries = 1}) ->
    Fun({error, Reason}, Session, Events, Opts);
retry(_, _, _, Peer, #diam_call{tries = Tries, peers_tried = PeersTried} = CallOpts) ->
    {retry, CallOpts#diam_call{tries = Tries - 1,
			       peers_tried = [Peer | PeersTried]}}.

%% prepare_request/4
prepare_request(#diameter_packet{header = Hdr0} = Packet, _SvcName, _Peer,
		#diam_call{peers_tried = PeersTried, seqno = SeqNo}) ->
    Hdr = Hdr0#diameter_header{
	    is_retransmitted =
		Hdr0#diameter_header.is_retransmitted =:= true orelse PeersTried /= [],
	    end_to_end_id = SeqNo},
    Packet#diameter_packet{header = Hdr}.

%% handle_answer/3
handle_answer(Fun, Msg, #diam_call{session = Session, events = Events, opts = Opts}) ->
    Fun(Msg, Session, Events, Opts).

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

handle_call({pick_peer, Candidates, SvcName}, _From, #state{peers = Peers} = State) ->
    Reply = pick_peer_h(Candidates, SvcName, Peers),
    {reply, Reply, State};

handle_call({start_request, SvcName, Peer}, _From, #state{peers = Peers0} = State) ->
    {Reply, Peers} = start_request_h(SvcName, Peer, Peers0),
    {reply, Reply, State#state{peers = Peers}};

handle_call({finish_request, SvcName, Peer}, _From, #state{peers = Peers0} = State) ->
    {Reply, Peers} = finish_request_h(SvcName, Peer, Peers0),
    {reply, Reply, State#state{peers = Peers}}.

handle_cast({peer_down, _SvcName, {PeerRef, _}}, #state{peers = Peers} = State) ->
    {noreply, State#state{peers = maps:remove(PeerRef, Peers)}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'DOWN', MRef, _Type, Pid, _Info}, State) ->
    ets:match_delete(?MODULE, {{'_', Pid}, MRef}),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("handle_info: ~p", [Info]),
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
      last_ts = erlang:monotonic_time(millisecond),
      interval = floor(1000 / Rate),
      tokens = Rate
     };
update_bucket(#peer{last_ts = Last, interval = Interval,
		    rate = Rate, tokens = Tokens} = Peer) ->
    Now = erlang:monotonic_time(millisecond),
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
		RateLimits = application:get_env(ergw_aaa, rate_limits, #{}),
		#{outstanding_requests := Capacity, rate := Rate} =
		    if is_map_key(Host, RateLimits) ->
			    maps:get(Host, RateLimits);
		       is_map_key(default, RateLimits) ->
			    maps:get(default, RateLimits);
		       true ->
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
	erlang:floor(((Rate - Tokens / Rate) * ?LoadBuckets)),
    maps:update_with(Load, [Host|_], [Host], LM).

%% pick_peer_h/3
pick_peer_h([], _SvcName, _Peers) ->
    false;
pick_peer_h(Candidates, _SvcName, Peers) ->
    CandMap = lists:foldl(
		fun({_, #diameter_caps{origin_host = {OH, _}}} = Peer, CM) ->
			maps:update_with(OH, [Peer|_], [Peer], CM)
		end, #{}, Candidates),
    LoadMap = maps:fold(relative_peer_load(_, _, _, Peers), #{}, CandMap),
    [{_, Cands} | _] = lists:keysort(1, maps:to_list(LoadMap)),
    N = rand:uniform(length(Cands)),
    Connection = pick_connection(maps:get(lists:nth(N, Cands), CandMap), Peers),
    {ok, Connection}.

start_request_h(_SvcName, {PeerRef,  #diameter_caps{origin_host = {OH, _}}}, Peers0) ->
    Peer0 = maps:get(OH, Peers0, #peer{}),
    Peer = update_bucket(Peer0),
    #peer{outstanding = Cnt,
	  capacity    = Cap,
	  tokens      = Tokens} = Peer,
    if (Cnt >= Cap orelse Tokens == 0) ->
	    {{discard, rate_limit}, Peers0#{OH => Peer}};
       true ->
	    Peers1 =
		Peers0#{OH => Peer#peer{outstanding = Cnt + 1, tokens = Tokens - 1}},
	    Peers2 = maps:update_with(PeerRef, _ + 1, 1, Peers1),
	    {ok, Peers2}
    end.

finish_request_h(_SvcName, {PeerRef,  #diameter_caps{origin_host = {OH, _}}}, Peers0) ->
    Peers1 = case Peers0 of
		 #{PeerRef := _} -> maps:update_with(PeerRef, _ - 1, Peers0);
		 _               -> Peers0
	     end,
    Peer = get_peer(OH, Peers1),
    case Peer of
	#peer{outstanding = Cnt} when Cnt > 0 ->
	    {ok, Peers1#{OH => Peer#peer{outstanding = Cnt - 1}}};
	_ ->
	    {{error, underflow}, Peers1#{OH => Peer}}
    end.
