-module(ctld_session_seq).

-behaviour(gen_server).

%% API
-export([start_link/0, new/1, inc/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Id) ->
    gen_server:call(?SERVER, {new, Id}).

inc(Id) ->
    ets:update_counter(?MODULE, Id, 1).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [public, set, named_table, {write_concurrency, true}]),
    new_id(default),
    {ok, #state{}}.

handle_call({new, Id}, _From, State) ->
    new_id(Id),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_id(Id) ->
    Start = (crypto:rand_uniform(0, 4294967296)) bsl 128,
    ets:insert(?MODULE, {Id, Start}).
