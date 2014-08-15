%%%

-module(genlib_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%%

linear_test() ->
    R1 = genlib_retry:linear(3, 64),
    {wait, 64, R2} = genlib_retry:next_step(R1),
    {wait, 64, R3} = genlib_retry:next_step(R2),
    {wait, 64, R4} = genlib_retry:next_step(R3),
    finish = genlib_retry:next_step(R4).

exponential_test() ->
    R1 = genlib_retry:exponential(3, 2, 64),
    {wait, 64 , R2} = genlib_retry:next_step(R1),
    {wait, 128, R3} = genlib_retry:next_step(R2),
    {wait, 256, R4} = genlib_retry:next_step(R3),
    finish = genlib_retry:next_step(R4).

linear_compute_retries_test() ->
    Fixture = [
        {{max_total_timeout, 1909}, 10},
        {{max_total_timeout, 3449}, 104},
        {{max_total_timeout, 0}, 1000}
    ],
    lists:foreach(
        fun({Retries = {max_total_timeout, MaxTotalTimeout}, Timeout}) ->
            assert_max_retry(genlib_retry:linear(Retries, Timeout), MaxTotalTimeout)
        end,
        Fixture
    ).

exponential_compute_retries_test() ->
    Fixture = [
        {{max_total_timeout, 130}, 2, 10,  1300},
        {{max_total_timeout, 130}, 21, 11, 1300},
        {{max_total_timeout, 200}, 1, 10, 27000},
        {{max_total_timeout, 2000}, 1.2, 100, 1000}
    ],
    lists:foreach(
        fun({Retries = {max_total_timeout, MaxTotalTimeout}, Factor, Timeout, MaxTimeout}) ->
            assert_max_retry(genlib_retry:exponential(Retries, Factor, Timeout, MaxTimeout), MaxTotalTimeout)
        end,
        Fixture
    ).

assert_max_retry(Retry = {linear, _Retries, Timeout}, MaxTotalTimeout) ->
    {_, SumTimeout} = process_retry(Retry),
    ?assertMatch(true, SumTimeout =< MaxTotalTimeout),
    ?assertMatch(true, SumTimeout + Timeout >= MaxTotalTimeout);

assert_max_retry(Retry = {exponential, _Retries, _Factor, _Timeout, _MaxTimeout}, MaxTotalTimeout) ->
    {{exponential, _, _, NextTimeout, _}, SumTimeout} = process_retry(Retry),
    ?assertMatch(true, SumTimeout =< MaxTotalTimeout),
    ?assertMatch(true, (SumTimeout + NextTimeout) >= MaxTotalTimeout).

process_retry(Retry) ->
    process_retry(Retry, 0).

process_retry(Retry, SumTimeout) ->
    case genlib_retry:next_step(Retry) of
        {wait, Timeout, NextRetry} ->
            process_retry(NextRetry, SumTimeout + Timeout);
        finish ->
            {Retry, SumTimeout}
    end.