%%

-module(genlib_format_tests).

-include_lib("eunit/include/eunit.hrl").

hex_test_() ->
    [
        ?_assertEqual(<<"DEADBEEF">>, genlib_format:binary_to_hex(genlib_format:hex_to_binary(<<"deadbeef">>))),
        ?_assertError(badarg, genlib_format:hex_to_binary(<<"FEZZ">>)),
        ?_assertError(badarg, genlib_format:hex_to_binary(<<"DEF">>))
    ].

datetime_test_() ->
    [
        ?_assertEqual(<<"17.01.1989">>, genlib_format:format_date([dd, $., mm, $., yyyy], {1989, 1, 17})),
        ?_assertEqual(<<"17/01/89">>, genlib_format:format_date([dd, $/, mm, $/, yy], {1989, 1, 17}))
    ].

stacktrace_test_() ->
    [
        ?_assertMatch(
            <<
                "in call to proplists:get_value(dink,drance,[]) at proplists.erl:", _:3/binary, $\n,
                $\t, "called from genlib_format_tests:", _/binary
            >>,
            genlib_format:format_stacktrace(
                try proplists:get_value(dink, drance, []) catch _:_ -> erlang:get_stacktrace() end,
                [newlines]
            )
        ),
        ?_assertMatch(
            <<
                "in call to ordsets:add_element(1,{4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,..) at ordsets.erl:", _:3/binary, ", "
                "called from genlib_format_tests:", _/binary
            >>,
            genlib_format:format_stacktrace(
                try ordsets:add_element(1, {4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,23,42,4,8,15,16,23,42}) catch
                    _:_ -> erlang:get_stacktrace()
                end
            )
        )
    ].
