%%

-module(genlib_rational_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([no_auto_import]).

-spec test() -> _.

-type testcase() :: {_, fun()}.

%%

-spec general_test_() -> [testcase()].
-spec compare_test_() -> [testcase()].
-spec round_test_() -> [testcase()].
-spec round_half_away_from_zero_test_() -> [testcase()].
-spec round_half_towards_zero_test_() -> [testcase()].

general_test_() ->
    [
        ?_assertError(badarg, new(3, 0)),
        ?_assertError(badarg, inv(new(0, -3))),

        ?_assertEqual(-3, round(neg(new(-3, -1)))),
        ?_assertEqual(1, round(add(new(15, 31), new(16, 31)))),
        ?_assertEqual(2, round(add(new(5, 3), new(3, 7)))),
        ?_assertEqual(0, round(sub(new(17, 3), new(17, 3)))),
        ?_assertEqual(3, round(inv(new(-1, -3)))),
        ?_assertEqual(1, round(mul(new(1, 3), new(3)))),
        ?_assertEqual(1, round(mul(new(-1, -3), new(3)))),
        ?_assertEqual(58, round(mul(new(333, 1000), new(1755, 10)))),
        ?_assertEqual(1, round(dvd(new(5, 7), new(2, 3)))),
        ?_assertEqual(2, round(dvd(new(4, 7), new(2, 7))))
    ].

compare_test_() ->
    [
        ?_assertEqual(eq, cmp(new(17, 3), new(17, 3))),
        ?_assertEqual(eq, cmp(new(1, 4), new(25, 100))),
        ?_assertEqual(gt, cmp(new(1, 3), new(1, 4))),
        ?_assertEqual(lt, cmp(new(1, 5), new(1, 4)))
    ].

round_test_() ->
    [
        ?_assertEqual(2, round(new(5, 3))),
        ?_assertEqual(-2, round(new(-5, 3))),
        ?_assertEqual(1, round(new(7, 5))),
        ?_assertEqual(-1, round(new(-7, 5))),
        ?_assertEqual(1, round(new(7, 5))),
        ?_assertEqual(2, round(new(15, 10))),
        ?_assertEqual(-2, round(new(-15, 10)))
    ].

round_half_away_from_zero_test_() ->
    [
        ?_assertEqual(2, round(new(5, 3), round_half_away_from_zero)),
        ?_assertEqual(-2, round(new(-5, 3), round_half_away_from_zero)),
        ?_assertEqual(1, round(new(7, 5), round_half_away_from_zero)),
        ?_assertEqual(-1, round(new(-7, 5), round_half_away_from_zero)),
        ?_assertEqual(1, round(new(7, 5), round_half_away_from_zero)),
        ?_assertEqual(2, round(new(15, 10), round_half_away_from_zero)),
        ?_assertEqual(-2, round(new(-15, 10), round_half_away_from_zero))
    ].

round_half_towards_zero_test_() ->
    [
        ?_assertEqual(2, round(new(5, 3), round_half_towards_zero)),
        ?_assertEqual(-2, round(new(-5, 3), round_half_towards_zero)),
        ?_assertEqual(1, round(new(7, 5), round_half_towards_zero)),
        ?_assertEqual(-1, round(new(-7, 5), round_half_towards_zero)),
        ?_assertEqual(1, round(new(7, 5), round_half_towards_zero)),
        ?_assertEqual(1, round(new(15, 10), round_half_towards_zero)),
        ?_assertEqual(-1, round(new(-15, 10), round_half_towards_zero))
    ].

%%

new(P) -> genlib_rational:new(P).

new(P, Q) -> genlib_rational:new(P, Q).

cmp(P, Q) -> genlib_rational:cmp(P, Q).

neg(R1) -> genlib_rational:neg(R1).

inv(R1) -> genlib_rational:inv(R1).

add(R1, R2) -> genlib_rational:add(R1, R2).

sub(R1, R2) -> genlib_rational:sub(R1, R2).

mul(R1, R2) -> genlib_rational:mul(R1, R2).

dvd(R1, R2) -> genlib_rational:dvd(R1, R2).

round(R1) -> genlib_rational:round(R1).

round(R1, Method) -> genlib_rational:round(R1, Method).
