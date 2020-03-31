-module(genlib_pmap).

-export([map/2]).
-export([map/3]).
-export([safemap/2]).
-export([safemap/3]).

-type result(T) :: {ok, T} | {error, _} | timeout.
-type opts() :: #{
    timeout    => timeout(),
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
    {erlang:spawn_opt(
        fun () -> erlang:exit(Executor(F, E)) end,
        [monitor]
    ), Extra}.

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
    Results = lists:foldl(fun ({W, Extra}, Acc) -> await(W, Extra, Deadline, Collector, Acc) end, [], Workers),
    lists:reverse(Results).

await({PID, MRef}, Extra, infinity, Collector, Acc) ->
    receive
        {'DOWN', MRef, process, PID, Result} ->
            Collector(Result, Extra, Acc)
    end;
await({PID, MRef}, Extra, Deadline, Collector, Acc) ->
    receive
        {'DOWN', MRef, process, PID, Result} ->
            Collector(Result, Extra, Acc)
    after max(0, shift_timeout(Deadline, -nowms())) ->
        _ = erlang:demonitor(MRef, [flush]),
        _ = erlang:exit(PID, kill),
        Collector(timeout, Extra, Acc)
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
