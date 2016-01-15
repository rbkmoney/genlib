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

iso8601_test_() ->
    [
        ?_assertEqual(<<"2012-12-22T00:04:33Z">>, genlib_format:format_datetime_iso8601({{2012, 12, 22}, {0, 4, 33}})),
        ?_assertEqual(<<"2012-12-22T04:34:33+04:30">>, genlib_format:format_datetime_iso8601_tz({{2012, 12, 22}, {4, 34, 33}}, {'+', 4, 30}))
    ].

timestamp_test_() ->
    [
        ?_assertEqual(<<"2012-12-22T00:04:33Z">>, genlib_format:format_timestamp_iso8601(1356134673)),
        ?_assertEqual(<<"2012-12-22T04:34:33+04:30">>, genlib_format:format_timestamp_iso8601_tz(1356150873, {'+', 4, 30}))
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

decimal_test_() ->
    [
        ?_assertEqual(<<"4242">>, genlib_format:format_decimal(4242, 0)),
        ?_assertEqual(<<"424.2">>, genlib_format:format_decimal(4242, 1)),
        ?_assertEqual(<<"0.00000007">>, genlib_format:format_decimal(7, 8)),
        ?_assertError(badarg, genlib_format:format_decimal(42, -1)),
        ?_assertError(badarg, genlib_format:format_decimal("42", 42))
    ].
