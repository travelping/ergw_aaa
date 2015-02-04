-module(ctld_aaa).

-type session() :: list({atom(), term()}) | map().
-type a3state() :: term().
-export_type([session/0, a3state/0]).

-callback init(proplists:proplist()) -> {ok, a3state()} | {error, term()}.
-callback start_authentication(From :: term(), Session :: session(), State :: a3state()) ->
    {reply, a3state()}.
-callback authorize(From :: term(), Session :: session(), State :: a3state()) ->
    {noreply, term()} | {reply, term(), a3state()} | {reply, term(), session(), a3state()}.
-callback start_accounting(From :: term(), Type :: term(), Session :: session(), State :: a3state()) ->
    {ok, a3state()}.
