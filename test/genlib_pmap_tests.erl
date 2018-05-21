%%

-module(genlib_pmap_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec empty_test_() -> [testcase()].

empty_test_() ->
    [
        ?_assertEqual([], genlib_pmap:map(fun (V) -> V              end, [])),
        ?_assertEqual([], genlib_pmap:map(fun (_) -> exit(welwalla) end, []))
    ].

-spec map_test_() -> [testcase()].

map_test_() ->
    [
        ?_assertMatch([_, _, _], genlib_pmap:map(fun (_) -> self() end, [1, 2, 3])),
        ?_assertEqual([3, 2, 1], genlib_pmap:map(fun (V) -> 4 - V end, [1, 2, 3]))
    ].

-spec errorneous_map_test_() -> [testcase()].

errorneous_map_test_() ->
    [
        ?_assertError(
            badarg,
            genlib_pmap:map(
                fun (V) -> integer_to_list(V) end,
                [1, haha, 3]
            )
        ),
        ?_assertError(
            function_clause,
            genlib_pmap:map(
                fun (V) when is_integer(V) -> integer_to_list(V) end,
                [1, haha, 3]
            )
        ),
        ?_assertThrow(
            even,
            genlib_pmap:map(
                fun (V) -> case V rem 2 of 0 -> throw(even); _ -> odd end end,
                [1, 2, 3]
            )
        ),
        ?_assertExit(
            {bad, 2},
            genlib_pmap:map(
                fun (V) -> case V rem 2 of 0 -> exit({bad, V}); _ -> ok end end,
                [1, 2, 3]
            )
        )
    ].

-spec safemap_test_() -> [testcase()].

safemap_test_() ->
    [
        ?_assertEqual(
            [{ok, 1}, {ok, 2}, {ok, 3}],
            genlib_pmap:safemap(
                fun (V) -> V end,
                [1, 2, 3]
            )
        ),
        ?_assertMatch(
            [{ok, "1"}, {error, {error, badarg, _}}, {ok, "3"}],
            genlib_pmap:safemap(
                fun (V) -> integer_to_list(V) end,
                [1, haha, 3]
            )
        )
    ].

-spec timeout_test_() -> [testcase()].

timeout_test_() ->
    ?_assertEqual(
        [{ok, ok}, {ok, ok}, timeout, timeout, timeout],
        genlib_pmap:safemap(
            fun (V) -> timer:sleep(V * 100) end,
            [1, 2, 3, 4, 5],
            #{timeout => 250}
        )
    ).

-spec speedup_test_() -> [testcase()].

speedup_test_() ->
    N = 100,
    T = 100,
    ConstOverhead = 100,
    ?_assert(
        % NOTE
        % We assume here that pmap overall spent no more than `T` time (which is the time a single worker
        % spends sleeping) plus some constant (which is essentially synchronisation overhead and non-ideal
        % SMP as we run tests on real hardware after all)
        T + ConstOverhead >
        measure(genlib_pmap, map, [
            fun (_) -> timer:sleep(T) end,
            lists:seq(1, N)
        ])
    ).

measure(M, F, A) ->
    {Time, _Result} = timer:tc(M, F, A),
    Time div 1000.
