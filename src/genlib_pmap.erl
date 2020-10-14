-module(genlib_pmap).

-export([map/2]).
-export([map/3]).
-export([safemap/2]).
-export([safemap/3]).

-type result(T) :: {ok, T} | {error, _} | timeout.
-type opts() :: #{
    % Soft limit for duration of _whole operation_, not of a single functor call.
    timeout => timeout(),
    % Hard limit for a number of processes to spin.
    proc_limit => pos_integer()
}.

-define(DEFAULT_SAFEMAP_TIMEOUT, 5000).

-spec map(fun((A) -> B), [A]) -> [B].
-spec map(fun((A) -> B), [A], opts()) -> [B] | no_return().

map(F, L) ->
    map(F, L, #{}).

map(F, L, Opts0) ->
    Opts = maps:merge(#{timeout => infinity}, Opts0),
    lists:map(fun replicate/1, safemap(F, L, Opts)).

-spec safemap(fun((A) -> B), [A]) -> [result(B)].
-spec safemap(fun((A) -> B), [A], opts()) -> [result(B)].

safemap(F, L) ->
    safemap(F, L, #{}).

safemap(F, L, Opts) ->
    % First of all we spawn a _fuse process_ responsible for killing itself when:
    % * either timeout specified in `Opts` passes,
    % * or current process terminates for some reason.
    % Later on we link each worker process with this fuse process so that no worker would outlive
    % neither specified timeout nor current process.
    Fuse = spawn_fuse(Opts),
    Results =
        case maps:get(proc_limit, Opts, undefined) of
            undefined ->
                % Number of workers is unbounded.
                % Spawn one worker per each unit of work.
                collect(fun collect_one/3, lists:map(executor_one(F, Fuse), L));
            Limit ->
                Size = erlang:length(L),
                if
                    Limit >= Size ->
                        % Number of workers is bounded yet greater than the size of workload.
                        % Again, spawn one worker per each unit of work.
                        collect(fun collect_one/3, lists:map(executor_one(F, Fuse), L));
                    Limit < Size ->
                        % Number of workers is bounded.
                        % Distribute workload between this number of workers so that each
                        % would get their fair share right at the start.
                        collect(fun collect_many/3, distribute(F, Fuse, L, Size, Limit, 0))
                end
        end,
    _ = erlang:exit(Fuse, normal),
    Results.

spawn_fuse(Opts) ->
    Pid = erlang:self(),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_SAFEMAP_TIMEOUT),
    erlang:spawn(fuse(Pid, Timeout)).

-spec fuse(pid(), timeout()) -> fun(() -> no_return()).
fuse(Pid, Timeout) ->
    fun() ->
        % NOTE
        % We need to trap exits here to be unaffected by worker processes terminating upon
        % completion. This in turn makes the process message queue grow in size because for
        % simplicity we _do not_ consume these messages. However memory requirements are already
        % roughly O(n) so this should not make much difference.
        _Was = erlang:process_flag(trap_exit, true),
        MRef = erlang:monitor(process, Pid),
        receive
            {'EXIT', Pid, Reason} ->
                % Coordinating process ordered us to terminate.
                exit(Reason);
            {'DOWN', MRef, process, Pid, Reason} ->
                % Coordinating process terminated for some external reason.
                exit(Reason)
        after Timeout ->
            % Timeout passed. It's time to send exit signals to all workers still alive.
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
    fun(E) ->
        spawn_executor(fun execute_one/2, F, Fuse, E, 1)
    end.

spawn_executor(Executor, F, Fuse, E, Extra) ->
    Pid = erlang:self(),
    Ref = erlang:make_ref(),
    WorkerPidRef = erlang:spawn_monitor(fun() ->
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
    catch
        C:R:Stacktrace ->
            {error, {C, R, Stacktrace}}
    end.

-spec execute_many(fun((A) -> B), [A]) -> {many, [result(B)]}.
execute_many(F, L) ->
    {many, lists:foldl(fun(E, Acc) -> [execute_one(F, E) | Acc] end, [], L)}.

collect(Collector, Workers) ->
    Results = await(Collector, Workers, []),
    lists:reverse(Results).

await(Collector, [{{{WorkerPid, MRef}, Ref}, Extra} | WorkersLeft], Results) ->
    receive
        {Ref, Result} ->
            _ = erlang:demonitor(MRef, [flush]),
            await(Collector, WorkersLeft, Collector(Result, Extra, Results));
        {'DOWN', MRef, process, WorkerPid, Reason} ->
            Result =
                case Reason of
                    % NOTE
                    % Getting `noproc` here means that the fuse process was already dead at the time of
                    % linking. We interpret it as a timeout condition  which appears to be the only
                    % valid interpretation here.
                    {noproc, _} -> timeout;
                    _ -> Reason
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
