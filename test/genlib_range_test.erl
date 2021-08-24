-module(genlib_range_test).

-export([range_ops_fail_with_zero_step/0]).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec range_ops_fail_with_zero_step() -> _.

range_ops_fail_with_zero_step() ->
    ?assertError(
        badarg,
        genlib_range:map(fun(_) -> throw(blow) end, {0, 0, 0})
    ).
