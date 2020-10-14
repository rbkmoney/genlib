%%

-module(genlib_bitset_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec bitset_test() -> _.

bitset_test() ->
    ?assertEqual(<<"C0L0R">>, genlib_bitset:to_binary([0, 5, 6, 9, 10, 16, 19, 20, 25, 26, 32, 34, 37], 5 * 8, 1)),
    ?assertEqual(<<127>>, genlib_bitset:to_binary([1, 2, 3, 4, 5, 6, 7], 8)),
    RN = fun(N) -> rand:uniform(N) - 1 end,
    RL = lists:usort([RN(64) || _ <- lists:seq(1, RN(40))]),
    ?assertEqual(RL, genlib_bitset:from_binary(genlib_bitset:to_binary(RL, 64))).
