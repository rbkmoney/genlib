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
