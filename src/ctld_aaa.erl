-module(ctld_aaa).

-type session() :: [{atom(), term()}].
-type a3state() :: term().
-export_type([session/0, a3state/0]).

-callback init(proplists:proplist()) -> {ok, a3state()} | {error, term()}.
-callback authenticate(From :: term(), Session :: session(), State :: a3state()) ->
    {noreply, term()} | {reply, term(), a3state()} | {reply, term(), session(), a3state()}.
-callback authorize(From :: term(), Session :: session(), State :: a3state()) ->
    {noreply, term()} | {reply, term(), a3state()} | {reply, term(), session(), a3state()}.
-callback start(Session :: session(), State :: a3state()) ->
    a3state() | {session(), a3state()}.
-callback interim(Session :: session(), State :: a3state()) ->
    a3state() | {session(), a3state()}.
-callback stop(Session :: session(), State :: a3state()) ->
    a3state() | {session(), a3state()}.

