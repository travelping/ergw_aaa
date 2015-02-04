-module(ctld_behavior).

-export([register/2]).

-export([start/3, start/4,
         start_link/3, start_link/4,
         send_event/2, wake_hib/6,
	 call/2, call/3]).

%% Internal exports
-export([init_it/6,
         system_continue/3,
         system_terminate/4,
         system_code_change/4,
         format_status/2]).

-import(error_logger, [format/2]).

%%% The user module should export:
%%%
%%%   init(Args)
%%%     ==> {ok, StateModule, StateData}
%%%         {ok, StateModule, StateData, Timeout}
%%%         ignore
%%%         {stop, Reason}

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------

-callback init(Args :: term()) ->
    {ok, StateModule :: atom(), StateData :: term()} |
    {stop, Reason :: term()} | ignore.

-callback terminate(Reason :: normal | shutdown | {shutdown, term()}
                    | term(), StateModule :: atom(), StateData :: term()) ->
    term().
-callback code_change(OldVsn :: term() | {down, term()}, StateModule :: atom(),
                      StateData :: term(), Extra :: term()) ->
    {ok, NextStateModule :: atom(), NewStateData :: term()}.

%%% ---------------------------------------------------
%%% Starts a behavior state machine.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' fsm
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% ---------------------------------------------------
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).

send_event(Name, Event) ->
    Name ! Event,
    ok.

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% ----------------------------------------------------------------- 
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
        {ok,Res} ->
            Res;
        {'EXIT',Reason} ->
            exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
        {ok,Res} ->
            Res;
        {'EXIT',Reason} ->
            exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = gen:debug_options(Options),
    case catch Mod:init(Args) of
        {ok, StateModule, StateData} ->
            proc_lib:init_ack(Starter, {ok, self()}),
	    next_state(Parent, Name, StateModule, StateData, Mod, infinity, Debug, false);
        {stop, Reason} ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        ignore ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, ignore),
            exit(normal);
        {'EXIT', Reason} ->
            unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, Reason}),
            exit(Reason);
        Else ->
            Error = {bad_return_value, Else},
            proc_lib:init_ack(Starter, {error, Error}),
            exit(Error)
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    _ = (catch unregister(Name));
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name);
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.

%%-----------------------------------------------------------------
%% The MAIN loop
%%-----------------------------------------------------------------
loop(Parent, Name, StateModule, StateData, Mod, hibernate, Debug) ->
    proc_lib:hibernate(?MODULE, wake_hib,
                       [Parent, Name, StateModule, StateData, Mod,
                        Debug]);
loop(Parent, Name, StateModule, StateData, Mod, TimeOut, Debug) ->
    Reply = StateModule:handle_event(TimeOut, StateData),
    handle_reply(Reply, Parent, Name, StateModule, StateData, Mod, TimeOut, Debug, false).

wake_hib(Parent, Name, StateModule, StateData, Mod, Debug) ->
    Reply = StateModule:handle_event(infinite, StateData),
    handle_reply(Reply, Parent, Name, StateModule, StateData, Mod, hibernate, Debug, true).

handle_call(From, Msg, Parent, Name, StateModule, StateData, Mod) ->
    case catch Mod:handle_call(Msg, From, StateModule, StateData) of
        {reply, Reply, NStateModule, NStateData} ->
            reply(From, Reply),
            loop(Parent, Name, NStateModule, NStateData, Mod, infinity, []);
        {reply, Reply, NStateModule, NStateData, Time1} ->
            reply(From, Reply),
            loop(Parent, Name, NStateModule, NStateData, Mod, Time1, []);
        {noreply, NStateModule, NStateData} ->
            loop(Parent, Name, NStateModule, NStateData, Mod, infinity, []);
        {noreply, NStateModule, NStateData, Time1} ->
            loop(Parent, Name, NStateModule, NStateData, Mod, Time1, []);
        {stop, Reason, Reply, NStateModule, NStateData} ->
            {'EXIT', R} =
                (catch terminate(Reason, Name, Msg, Mod, NStateModule, NStateData, [])),
            reply(From, Reply),
            exit(R);
        {'EXIT', What} ->
            terminate(What, Name, Msg, Mod, StateModule, StateData, []);
        Other ->
            terminate({bad_return_value, Other}, Name, Msg, Mod, StateModule, StateData, [])
    end.

handle_reply(Reply, Parent, Name, StateModule, StateData, Mod, Time, Debug, Hib) ->
    case Reply of
        {system, From, get_state} ->
            Misc = [Name, StateModule, StateData, Mod, Time],
            sys:handle_system_msg(get_state, From, Parent, ?MODULE, Debug,
                                  {{StateModule, StateData}, Misc}, Hib);
        {system, From, {replace_state, StateFun}} ->
            State = {StateModule, StateData},
            NState = {NStateModule, NStateData} = try StateFun(State)
                                                catch _:_ -> State end,
            NMisc = [Name, NStateModule, NStateData, Mod, Time],
            sys:handle_system_msg(replace_state, From, Parent, ?MODULE, Debug,
                                  {NState, NMisc}, Hib);
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                                  [Name, StateModule, StateData, Mod, Time], Hib);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, Name, Reply, Mod, StateModule, StateData, Debug);

	{{'$gen_call', From, Msg}, NStateData} ->
		handle_call(From, Msg, Parent, Name, StateModule, NStateData, Mod);

        {ok, NStateData} ->
            loop(Parent, Name, StateModule, NStateData, Mod, infinity, []);
        {ok, NStateData, Time1} ->
            loop(Parent, Name, StateModule, NStateData, Mod, Time1, []);

        {next_profile, stop, NStateData} ->
            terminate(normal, Name, Reply, Mod, StateModule, NStateData, []);
        {next_profile, stop, NStateData, _Time1} ->
            terminate(normal, Name, Reply, Mod, StateModule, NStateData, []);

        {next_profile, NStateModule, NStateData} ->
            next_state(Parent, Name, NStateModule, NStateData, Mod, infinity, [], Hib);
        {next_profile, NStateModule, NStateData, Time1} ->
            next_state(Parent, Name, NStateModule, NStateData, Mod, Time1, [], Hib);

        {stop, Reason, NStateData} ->
            terminate(Reason, Name, Reply, Mod, StateModule, NStateData, []);

        Reply ->
            terminate({bad_return_value, Reply},
                      Name, Reply, Mod, StateModule, StateData, [])
    end.

next_state(Parent, Name, NStateModule, StateData, Mod, TimeOut, Debug, Hib) ->
    lager:debug("Calling ~p:enter(~p)", [NStateModule, StateData]),
    Reply = (catch NStateModule:enter(StateData)),
    lager:debug("Reply: ~p", [Reply]),
    handle_reply(Reply, Parent, Name, NStateModule, StateData, Mod, TimeOut, Debug, Hib).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, StateModule, StateData, Mod, Time]) ->
    loop(Parent, Name, StateModule, StateData, Mod, Time, Debug).

-spec system_terminate(term(), _, _, [term(),...]) -> no_return().

system_terminate(Reason, _Parent, Debug,
                 [Name, StateModule, StateData, Mod, _Time]) ->
    terminate(Reason, Name, [], Mod, StateModule, StateData, Debug).

system_code_change([Name, StateModule, StateData, Mod, Time],
                   _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, StateModule, StateData, Extra) of
        {ok, NewStateModule, NewStateData} ->
            {ok, [Name, NewStateModule, NewStateData, Mod, Time]};
        Else -> Else
    end.

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

-spec terminate(term(), _, _, atom(), _, _, _) -> no_return().

terminate(Reason, Name, Reply, Mod, StateModule, StateData, Debug) ->
    case catch Mod:terminate(Reason, StateModule, StateData) of
        {'EXIT', R} ->
            error_info(R, Name, Reply, StateModule, StateData, Debug),
            exit(R);
        _ ->
            case Reason of
                normal ->
                    exit(normal);
                shutdown ->
                    exit(shutdown);
                {shutdown,_}=Shutdown ->
                    exit(Shutdown);
                _ ->
                    FmtStateData =
                        case erlang:function_exported(Mod, format_status, 2) of
                            true ->
                                Args = [get(), StateData],
                                case catch Mod:format_status(terminate, Args) of
                                    {'EXIT', _} -> StateData;
                                    Else -> Else
                                end;
                            _ ->
                                StateData
                        end,
                    error_info(Reason,Name,Reply,StateModule,FmtStateData,Debug),
                    exit(Reason)
            end
    end.

error_info(Reason, Name, _Reply, StateModule, StateData, Debug) ->
    Reason1 =
        case Reason of
            {undef,[{M,F,A,L}|MFAs]} ->
                case code:is_loaded(M) of
                    false ->
                        {'module could not be loaded',[{M,F,A,L}|MFAs]};
                    _ ->
                        case erlang:function_exported(M, F, length(A)) of
                            true ->
                                Reason;
                            false ->
                                {'function not exported',[{M,F,A,L}|MFAs]}
                        end
                end;
            _ ->
                Reason
        end,
    Str = "** Behavior ~p terminating \n" ++
        "** When State == ~p~n"
        "**      Data  == ~p~n"
        "** Reason for termination = ~n** ~p~n",
    format(Str, [Name, StateModule, StateData, Reason1]),
    sys:print_log(Debug),
    ok.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, StateModule, StateData, Mod, _Time]] =
        StatusData,
    Header = gen:format_status_header("Status for behavior",
                                      Name),
    Log = sys:get_debug(log, Debug, []),
    DefaultStatus = [{data, [{"StateData", StateData}]}],
    Specfic =
        case erlang:function_exported(Mod, format_status, 2) of
            true ->
                case catch Mod:format_status(Opt,[PDict,StateData]) of
                    {'EXIT', _} -> DefaultStatus;
                    StatusList when is_list(StatusList) -> StatusList;
                    Else -> [Else]
                end;
            _ ->
                DefaultStatus
        end,
    [{header, Header},
     {data, [{"Status", SysState},
             {"Parent", Parent},
             {"Logged events", Log},
             {"StateModule", StateModule}]} |
     Specfic].

%%-----------------------------------------------------------------
%%
%%-----------------------------------------------------------------
register(Module, ProfileName) ->
    lager:warning("TODO: implement register(~p, ~p)", [Module, ProfileName]),
    ok.
