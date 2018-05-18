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
    Executor = executor(F),
    collect(lists:map(Executor, L), fun replicate/1, #{timeout => infinity}).

safemap(F, L) ->
    safemap(F, L, #{}).

safemap(F, L, Opts) ->
    Executor = executor(F),
    collect(lists:map(Executor, L), fun wrap/1, Opts).

executor(F) ->
    fun (E) ->
        {process, erlang:spawn_opt(
            fun () -> execute(F, E) end,
            [monitor]
        )}
    end.

execute(F, E) ->
    exit(
        try {result, F(E)} catch
            C:R ->
                {error, {C, R, erlang:get_stacktrace()}}
        end
    ).

collect(Ws, With, Opts) ->
    To = maps:get(timeout, Opts, 5000),
    Te = shift_timeout(To, nowms()),
    lists:map(
        fun ({process, {PID, MRef}}) ->
            receive
                {'DOWN', MRef, process, PID, {result, Result}} ->
                    With({result, Result});
                {'DOWN', MRef, process, PID, {error, Error}} ->
                    With({error, Error});
                {'DOWN', MRef, process, PID, Result} ->
                    With({error, {exit, Result, []}})
            after max(0, shift_timeout(Te, -nowms())) ->
                _ = exit(PID, kill),
                With(timeout)
            end
        end,
        Ws
    ).

shift_timeout(infinity, _) ->
    infinity;
shift_timeout(To, V) ->
    To + V.

replicate({result, Result}) ->
    Result;
replicate({error, {C, R, ST}}) ->
    erlang:raise(C, R, ST);
replicate(timeout) ->
    error(timeout).

wrap({result, Result}) ->
    {ok, Result};
wrap({error, Error}) ->
    {error, Error};
wrap(timeout) ->
    timeout.

nowms() ->
    {Ms, S, Mcs} = os:timestamp(),
    (Ms * 1000000 + S) * 1000 + Mcs div 1000.
