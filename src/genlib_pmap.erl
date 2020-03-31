-module(genlib_pmap).

-export([map/2]).
-export([safemap/2]).
-export([safemap/3]).

-type result(T) :: {ok, T} | {error, _} | timeout.
-type opts() :: #{
    timeout => timeout()
}.

-spec map(fun((A) -> B), [A]) ->
    [B].

-spec safemap(fun((A) -> B), [A]) ->
    [result(B)].
-spec safemap(fun((A) -> B), [A], opts()) ->
    [result(B)].

map(F, L) ->
    lists:map(fun replicate/1, safemap(F, L, #{timeout => infinity})).

safemap(F, L) ->
    safemap(F, L, #{}).

safemap(F, L, Opts) ->
    Executor = spawner(F),
    collect(lists:map(Executor, L), Opts).

spawner(F) ->
    fun (E) ->
        {process, erlang:spawn_opt(executor(F, E), [monitor])}
    end.

-spec executor(fun((A) -> _), A) -> fun(() -> no_return()).

executor(F, E) ->
    fun () ->
        erlang:exit(
            try
                {ok, F(E)}
            catch C:R:Stacktrace ->
                {error, {C, R, Stacktrace}}
            end
        )
    end.

collect(Workers, Opts) ->
    Timeout = maps:get(timeout, Opts, 5000),
    Deadline = shift_timeout(Timeout, nowms()),
    lists:map(fun (E) -> await(E, Deadline) end, Workers).

await({process, {PID, MRef}}, infinity) ->
    receive
        {'DOWN', MRef, process, PID, Result} ->
            map_result(Result)
    end;
await({process, {PID, MRef}}, Deadline) ->
    receive
        {'DOWN', MRef, process, PID, Result} ->
            map_result(Result)
    after max(0, shift_timeout(Deadline, -nowms())) ->
        _ = erlang:demonitor(MRef, [flush]),
        _ = erlang:exit(PID, kill),
        timeout
    end.

map_result(Result = {ok, _}) ->
    Result;
map_result(Result = {error, _}) ->
    Result;
map_result(Result) ->
    {exit, Result, []}.

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
