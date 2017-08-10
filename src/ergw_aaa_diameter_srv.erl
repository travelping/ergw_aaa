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

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([peer_down/2, is_first_request/2]).
-export([child_spec/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

peer_down(API, PeerId) ->
    (catch ets:delete(?MODULE, {API, PeerId})),
    ok.

is_first_request(API, PeerId) ->
    case ets:lookup(?MODULE, {API, PeerId}) of
	[] ->
	    gen_server:call(?SERVER, {is_first_request, API, PeerId});
	[_] ->
	    false
    end.

child_spec() ->
    {?MODULE, {?MODULE, start_link, []}, permanent, 5000, worker, [?MODULE]}.

%%%===================================================================%
%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?MODULE, [ordered_set, public, named_table, {keypos, 1}, {read_concurrency, true}]),

    {ok, #state{}}.

handle_call({is_first_request, API, PeerId}, _From, State) ->
    case ets:lookup(?MODULE, {API, PeerId}) of
	[] ->
	    MRef = erlang:monitor(process, PeerId),
	    ets:insert(?MODULE, {{API, PeerId}, MRef}),
	    {reply, true, State};
	_ ->
	    {reply, false, State}
    end.

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
