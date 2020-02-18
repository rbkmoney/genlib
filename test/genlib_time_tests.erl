-module(genlib_time_tests).
-include_lib("eunit/include/eunit.hrl").

-type testcase() :: {_, fun()}.

-spec test() -> _.

-spec consistent_tz_test() -> _.
consistent_tz_test() ->
    Tz = genlib_time:get_timezone(),
    T0 = os:timestamp(),
    Tzs = get_timezone_for(1200000, T0),
    [?assertEqual(Tz, Tz0) || Tz0 <- Tzs].

get_timezone_for(Mcs, T0) ->
    T1 = os:timestamp(),
    case timer:now_diff(T1, T0) of
        Diff when Diff > Mcs -> [];
        _Diff -> [genlib_time:get_timezone() | get_timezone_for(Mcs, T0)]
    end.

-spec shift_date_test_() -> [testcase()].

shift_date_test_() ->
    [
        ?_assertEqual(
            {2018, 2, 28},
            genlib_time:shift_date({2017, 1, 31}, {1, 1, 0})
        ),
        ?_assertEqual(
            {2018, 3, 1},
            genlib_time:shift_date({2017, 1, 31}, {1, 1, 1})
        ),
        ?_assertEqual(
            {2016, 2, 29},
            genlib_time:shift_date({2017, 1, 31}, {-2, 13, 0})
        ),
        ?_assertEqual(
            {2016, 3, 1},
            genlib_time:shift_date({2017, 1, 31}, {-2, 13, 1})
        ),
        ?_assertEqual(
            {2018, 1, 31},
            genlib_time:shift_date({2017, 1, 31}, {0, 0, 365})
        ),
        ?_assertEqual(
            {2017, 2, 28},
            genlib_time:shift_date({2016, 2, 29}, {0, 0, 365})
        )
    ].

-spec rfc3339_utc_test_() -> [testcase()].

rfc3339_utc_test_() ->
    [
        ?_assertEqual(
            false,
            genlib_time:is_utc(<<"2020-02-18T15:32:15+03:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_time:is_utc(<<"2020-02-18T15:32:15-00:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_time:is_utc(<<"2020-02-18T15:32:15+00:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_time:is_utc(<<"2020-02-18T15:32:15Z">>)
        )
    ].

-spec rfc3339_parse_test_() -> [testcase()].

rfc3339_parse_test_() ->
    [
        ?_assertEqual(
            503640306,
            genlib_time:parse(<<"1985-12-17T04:05:06.123Z">>, second)
        ),
        ?_assertEqual(
            503640306123,
            genlib_time:parse(<<"1985-12-17T04:05:06.123456Z">>, millisecond)
        ),
        ?_assertEqual(
            503640306123456,
            genlib_time:parse(<<"1985-12-17T04:05:06.123456Z">>, microsecond)
        ),
        ?_assertEqual(
            503640306123456789,
            genlib_time:parse(<<"1985-12-17T04:05:06.123456789Z">>, nanosecond)
        )
    ].

-spec rfc3339_format_test_() -> [testcase()].

rfc3339_format_test_() ->
    [
        ?_assertEqual(
            <<"1985-12-17T04:05:06Z">>,
            genlib_time:format(503640306, second)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123Z">>,
            genlib_time:format(503640306123, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456Z">>,
            genlib_time:format(503640306123456, microsecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456789Z">>,
            genlib_time:format(503640306123456789, nanosecond)
        )
    ].
