-module(genlib_pmap).

-export([map/2]).
-export([map/3]).
-export([safemap/2]).
-export([safemap/3]).

-type result(T) :: {ok, T} | {error, _} | timeout.
-type opts() :: #{
    % Soft limit for duration of _whole operation_, not of a single functor call.
    timeout    => timeout(),
    % Hard limit for a number of processes to spin.
    proc_limit => pos_integer()
}.

-spec map(fun((A) -> B), [A]) ->
    [B].
-spec map(fun((A) -> B), [A], opts()) ->
    [B] | no_return().

map(F, L) ->
    map(F, L, #{}).

map(F, L, Opts0) ->
    Opts = maps:merge(#{timeout => infinity}, Opts0),
    lists:map(fun replicate/1, safemap(F, L, Opts)).

-spec safemap(fun((A) -> B), [A]) ->
    [result(B)].
-spec safemap(fun((A) -> B), [A], opts()) ->
    [result(B)].

safemap(F, L) ->
    safemap(F, L, #{}).

safemap(F, L, Opts) ->
    Fuse = spawn_fuse(Opts),
    Results = case maps:get(proc_limit, Opts, undefined) of
        undefined ->
            collect(fun collect_one/3, lists:map(executor_one(F, Fuse), L));
        Limit ->
            Size = erlang:length(L),
            if
                Limit >= Size ->
                    collect(fun collect_one/3, lists:map(executor_one(F, Fuse), L));
                Limit < Size ->
                    collect(fun collect_many/3, distribute(F, Fuse, L, Size, Limit, 0))
            end
    end,
    _ = erlang:exit(Fuse, normal),
    Results.

spawn_fuse(Opts) ->
    Pid = erlang:self(),
    Timeout = maps:get(timeout, Opts, 5000),
    erlang:spawn(fuse(Pid, Timeout)).

-spec fuse(pid(), timeout()) ->
    fun(() -> no_return()).
fuse(Pid, Timeout) ->
    fun () ->
        _Was = erlang:process_flag(trap_exit, true),
        MRef = erlang:monitor(process, Pid),
        receive
            {'EXIT', Pid, Reason} ->
                exit(Reason);
            {'DOWN', MRef, process, Pid, Reason} ->
                exit(Reason)
        after Timeout ->
            exit(timeout)
        end
    end.

distribute(_, _, [], _Size, Limit, Limit) ->
    [];
distribute(F, Fuse, List, Size, Limit, I) when I < Limit ->
    Ns = compute_nth_slice(I, Size, Limit),
    {Slice, Rest} = lists:split(Ns, List),
    Executor = spawn_executor(fun execute_many/2, F, Fuse, Slice, Ns),
    [Executor | distribute(F, Fuse, Rest, Size, Limit, I + 1)].

compute_nth_slice(N, Size, Limit) ->
    (Size * (N + 1) div Limit) - (Size * N div Limit).

executor_one(F, Fuse) ->
    fun (E) ->
        spawn_executor(fun execute_one/2, F, Fuse, E, 1)
    end.

spawn_executor(Executor, F, Fuse, E, Extra) ->
    Pid = erlang:self(),
    Ref = erlang:make_ref(),
    WorkerPidRef = erlang:spawn_monitor(fun () ->
        % NOTE
        % If the fuse process is already dead at the time of linking (when the
        % timeout value was very small for example) worker will die right away
        % and the ERTS will emit appropriate error report. This will make some
        % noise and I can't seem to find a way to silence it.
        _ = erlang:link(Fuse),
        _ = Pid ! {Ref, Executor(F, E)},
        ok
    end),
    {{WorkerPidRef, Ref}, Extra}.

-spec execute_one(fun((A) -> B), A) -> result(B).

execute_one(F, E) ->
    try
        {ok, F(E)}
    catch C:R:Stacktrace ->
        {error, {C, R, Stacktrace}}
    end.

-spec execute_many(fun((A) -> B), [A]) -> {many, [result(B)]}.

execute_many(F, L) ->
    {many, lists:foldl(fun (E, Acc) -> [execute_one(F, E) | Acc] end, [], L)}.

collect(Collector, Workers) ->
    Results = await(Collector, Workers, []),
    lists:reverse(Results).

await(Collector, [{{{WorkerPid, MRef}, Ref}, Extra} | WorkersLeft], Results) ->
    receive
        {Ref, Result} ->
            _ = erlang:demonitor(MRef, [flush]),
            await(Collector, WorkersLeft, Collector(Result, Extra, Results));
        {'DOWN', MRef, process, WorkerPid, Reason} ->
            Result = case Reason of
                {noproc, _} -> timeout;
                _           -> Reason
            end,
            await(Collector, WorkersLeft, Collector(Result, Extra, Results))
    end;
await(_Collector, [], Results) ->
    Results.

collect_one(Result = {ok, _}, _, Acc) ->
    [Result | Acc];
collect_one(Result = {error, _}, _, Acc) ->
    [Result | Acc];
collect_one(timeout, _, Acc) ->
    [timeout | Acc];
collect_one(Result, _, Acc) ->
    [{exit, Result, []} | Acc].

collect_many({many, Results}, N, Acc) when length(Results) == N ->
    Results ++ Acc;
collect_many(timeout, N, Acc) ->
    lists:duplicate(N, timeout) ++ Acc;
collect_many(Result, N, Acc) ->
    lists:duplicate(N, {exit, Result, []}) ++ Acc.

replicate({ok, Result}) ->
    Result;
replicate({error, {C, R, ST}}) ->
    erlang:raise(C, R, ST);
replicate(timeout) ->
    error(timeout).
