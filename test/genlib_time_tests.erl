-module(genlib_time_tests).
-include_lib("eunit/include/eunit.hrl").

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
