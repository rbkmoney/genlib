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
    case maps:get(proc_limit, Opts, undefined) of
        undefined ->
            collect(fun collect_one/3, lists:map(executor_one(F), L), Opts);
        Limit ->
            Size = erlang:length(L),
            if
                Limit >= Size ->
                    collect(fun collect_one/3, lists:map(executor_one(F), L), Opts);
                Limit < Size ->
                    collect(fun collect_many/3, distribute(F, L, Size, Limit, 0), Opts)
            end
    end.

distribute(_, [], _Size, Limit, Limit) ->
    [];
distribute(F, List, Size, Limit, I) when I < Limit ->
    Ns = compute_nth_slice(I, Size, Limit),
    {Slice, Rest} = lists:split(Ns, List),
    [spawn_executor(fun execute_many/2, F, Slice, Ns) | distribute(F, Rest, Size, Limit, I + 1)].

compute_nth_slice(N, Size, Limit) ->
    (Size * (N + 1) div Limit) - (Size * N div Limit).

executor_one(F) ->
    fun (E) ->
        spawn_executor(fun execute_one/2, F, E, 1)
    end.

spawn_executor(Executor, F, E, Extra) ->
    Pid = erlang:self(),
    Ref = erlang:make_ref(),
    % NOTE
    % It should be safe _unless_ user supplied function decides to kill `self()`.
    WorkerPid = erlang:spawn_link(fun () ->
        _ = Pid ! {Ref, Executor(F, E)},
        ok
    end),
    {{WorkerPid, Ref}, Extra}.

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

collect(Collector, Workers, Opts) ->
    Timeout = maps:get(timeout, Opts, 5000),
    Deadline = shift_timeout(Timeout, nowms()),
    Results = case Deadline of
        infinity ->
            await(Collector, Workers, []);
        Deadline ->
            await_deadline(Deadline, Collector, Workers, [], [])
    end,
    lists:reverse(Results).

await(Collector, [{{_, Ref}, Extra} | WorkersLeft], Results) ->
    receive
        {Ref, Result} ->
            await(Collector, WorkersLeft, Collector(Result, Extra, Results))
    end;
await(_Collector, [], Results) ->
    Results.

await_deadline(Deadline, Collector, [{{PID, Ref}, Extra} | WorkersLeft], Results, Stuck) ->
    receive
        {Ref, Result} ->
            await_deadline(Deadline, Collector, WorkersLeft, Collector(Result, Extra, Results), Stuck)
    after max(0, shift_timeout(Deadline, -nowms())) ->
        await_deadline(Deadline, Collector, WorkersLeft, Collector(timeout, Extra, Results), [PID | Stuck])
    end;
await_deadline(_Deadline, _Collector, [], Results, Stuck) ->
    ok = discharge(Stuck),
    Results.

discharge(Stuck = [_ | _]) ->
    FlagWas = erlang:process_flag(trap_exit, true),
    ok = lists:foreach(fun (PID) -> erlang:exit(PID, kill) end, Stuck),
    ok = lists:foreach(fun (PID) -> receive {'EXIT', PID, _Reason} -> ok end end, Stuck),
    _ = erlang:process_flag(trap_exit, FlagWas),
    case FlagWas of
        false -> handle_trapped_exits();
        true  -> ok
    end;
discharge([]) ->
    ok.

handle_trapped_exits() ->
    receive
        {'EXIT', _PID, normal} -> handle_trapped_exits();
        {'EXIT', _PID, Reason} -> erlang:exit(Reason)
    after 0 ->
        ok
    end.

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

shift_timeout(infinity, _) ->
    infinity;
shift_timeout(Timeout, V) ->
    Timeout + V.

replicate({ok, Result}) ->
    Result;
replicate({error, {C, R, ST}}) ->
    erlang:raise(C, R, ST);
replicate(timeout) ->
    error(timeout).

nowms() ->
    erlang:monotonic_time(millisecond).
