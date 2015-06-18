%%

-module(genlib_opts_tests).

-include_lib("eunit/include/eunit.hrl").

take_test_() ->
    Opts = [{hey, "oh"}, {listen, "what"}, {i, "say oh"}, {come, "back and..."}],
    [
        ?_assertEqual(
            {"oh", [{listen, "what"}, {i, "say oh"}, {come, "back and..."}]},
            genlib_opts:take(hey, Opts)
        ),
        ?_assertEqual(
            {undefined, [{hey, "oh"}, {listen, "what"}, {i, "say oh"}, {come, "back and..."}]},
            genlib_opts:take(hell, Opts)
        ),
        ?_assertEqual(
            {42, [{hey, "oh"}, {listen, "what"}, {i, "say oh"}, {come, "back and..."}]},
            genlib_opts:take(hell, Opts, 42)
        ),
        ?_assertEqual(
            {["oh", undefined, "what", "say oh", "back and..."], []},
            lists:foldl(
                fun (K, {A, O1}) -> {V, O2} = genlib_opts:take(K, O1), {A ++ [V], O2} end,
                {[], Opts},
                [hey, honey, listen, i, come]
            )
        )
    ].
