%%

-module(genlib_format_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec hex_test_() -> [testcase()].
hex_test_() ->
    [
        ?_assertEqual(<<"DEADBEEF">>, genlib_format:binary_to_hex(genlib_format:hex_to_binary(<<"deadbeef">>))),
        ?_assertError(badarg, genlib_format:hex_to_binary(<<"FEZZ">>)),
        ?_assertError(badarg, genlib_format:hex_to_binary(<<"DEF">>))
    ].

-spec format_int_base_test_() -> [testcase()].
format_int_base_test_() ->
    [
        ?_assertError(badarg, genlib_format:format_int_base(12345, 99)),
        ?_assertError(badarg, genlib_format:format_int_base(-12345, 63)),
        ?_assertError(badarg, genlib_format:format_int_base(5, 1)),
        ?_assertEqual(<<"0">>, genlib_format:format_int_base(0, 42)),
        ?_assertEqual(<<"HW5">>, genlib_format:format_int_base(31337, 42)),
        ?_assertEqual(<<"-HW5">>, genlib_format:format_int_base(-31337, 42))
    ].

-spec datetime_test_() -> [testcase()].
datetime_test_() ->
    [
        ?_assertEqual(<<"17.01.1989">>, genlib_format:format_date([dd, $., mm, $., yyyy], {1989, 1, 17})),
        ?_assertEqual(<<"17/01/89">>, genlib_format:format_date([dd, $/, mm, $/, yy], {1989, 1, 17}))
    ].

-spec stacktrace_test_() -> [testcase()].
stacktrace_test_() ->
    [
        ?_assertMatch(
            <<
                "in proplists:get_value(dink,drance,[]) at line ",
                _:3/binary,
                $\n,
                $\t,
                "in genlib_format_tests:-stacktrace_test_/0",
                _:7/binary,
                "/0 at line ",
                _:2/binary,
                $\n,
                _/binary
            >>,
            genlib_format:format_stacktrace(
                try
                    proplists:get_value(dink, drance, [])
                catch
                    _:_:Stacktrace ->
                        Stacktrace
                end,
                [newlines]
            )
        ),
        ?_assertMatch(
            <<
                "in ordsets:add_element(1,{4,8,15,16,23,...}) at line ",
                _:3/binary,
                ", "
                "in genlib_format_tests:-stacktrace_test_/0",
                _:7/binary,
                "/0 at line ",
                _:2/binary,
                _/binary
            >>,
            genlib_format:format_stacktrace(
                try
                    ordsets:add_element(
                        1,
                        {4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8,
                            15, 16, 23, 42, 4, 8, 15, 16, 23, 42}
                    )
                catch
                    _:_:Stacktrace ->
                        Stacktrace
                end,
                [{arglist_limit, 20}]
            )
        ),
        ?_assertMatch(
            <<
                "in ordsets:add_element(1,{4,8,...}) at line ",
                _:3/binary,
                ", "
                "in genlib_format_tests:-stacktrace_test_/0",
                _:7/binary,
                "/0 at line ",
                _:2/binary,
                _/binary
            >>,
            genlib_format:format_stacktrace(
                try
                    ordsets:add_element(
                        1,
                        {4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8, 15, 16, 23, 42, 4, 8,
                            15, 16, 23, 42, 4, 8, 15, 16, 23, 42}
                    )
                catch
                    _:_:Stacktrace ->
                        Stacktrace
                end,
                [{arglist_depth, 5}]
            )
        )
    ].

-spec decimal_test_() -> [testcase()].
decimal_test_() ->
    [
        ?_assertEqual(<<"4242">>, genlib_format:format_decimal(4242, 0)),
        ?_assertEqual(<<"424.2">>, genlib_format:format_decimal(4242, 1)),
        ?_assertEqual(<<"0.00000007">>, genlib_format:format_decimal(7, 8)),
        ?_assertError(badarg, genlib_format:format_decimal(42, -1)),
        ?_assertError(badarg, genlib_format:format_decimal("42", 42))
    ].

-spec uuid_to_bstring_test_() -> [testcase()].
uuid_to_bstring_test_() ->
    [
        ?_assertEqual(
            <<"952af85f-06e1-4986-b55e-bbe3951a2f4a">>,
            genlib_format:uuid_to_bstring(<<149, 42, 248, 95, 6, 225, 73, 134, 181, 94, 187, 227, 149, 26, 47, 74>>)
        )
    ].

-spec bstring_to_uuid_test_() -> [testcase()].
bstring_to_uuid_test_() ->
    [
        ?_assertEqual(
            <<149, 42, 248, 95, 6, 225, 73, 134, 181, 94, 187, 227, 149, 26, 47, 74>>,
            genlib_format:bstring_to_uuid(<<"952af85f-06e1-4986-b55e-bbe3951a2f4a">>)
        )
    ].

-spec parse_timespan_test_() -> [testcase()].
parse_timespan_test_() ->
    [
        ?_assertEqual(15 * 1000, genlib_format:parse_timespan(<<"15s">>)),
        ?_assertEqual(15 * 60 * 1000, genlib_format:parse_timespan(<<"15m">>)),
        ?_assertEqual(erlang:round(1.5 * 60 * 1000), genlib_format:parse_timespan(<<"1.5m">>)),
        ?_assertError({badarg, {invalid_time_unit, <<"h">>}}, genlib_format:parse_timespan(<<"15h">>))
    ].
