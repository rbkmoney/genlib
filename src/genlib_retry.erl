%%%
%%% Genlib

-module(genlib_retry).

-export([linear/2]).
-export([exponential/3]).
-export([exponential/4]).
-export([intervals/1]).

-export([timecap/2]).

-export([next_step/1]).

-export_type([strategy/0]).

-type retries_num() :: pos_integer() | infinity.

-opaque strategy() ::
      {linear     , Retries::retries_num(), Timeout::pos_integer()}
    | {exponential, Retries::retries_num(), Factor::number(), Timeout::pos_integer(), MaxTimeout::timeout()}
    | {array      , Array::list(pos_integer())}.

%%

-define(is_posint(V), (is_integer(V) andalso V > 0)).
-define(is_retries(V), (V =:= infinity orelse ?is_posint(V))).
-define(is_max_total_timeout(V), (is_integer(V) andalso V >= 0)).

-spec linear(retries_num() | {max_total_timeout, pos_integer()}, pos_integer()) -> strategy().

linear(Retries, Timeout) when
    ?is_retries(Retries) andalso
    ?is_posint(Timeout)
->
    {linear, Retries, Timeout};

linear(Retries = {max_total_timeout, MaxTotalTimeout}, Timeout) when
    ?is_max_total_timeout(MaxTotalTimeout) andalso
    ?is_posint(Timeout) ->
    {linear, compute_retries(linear, Retries, Timeout), Timeout}.

-spec exponential(retries_num() | {max_total_timeout, pos_integer()}, number(), pos_integer()) -> strategy().

exponential(Retries, Factor, Timeout) when
    ?is_posint(Timeout) andalso
    Factor > 0
->
    exponential(Retries, Factor, Timeout, infinity).

-spec exponential(retries_num() | {max_total_timeout, pos_integer()}, number(), pos_integer(), timeout()) -> strategy().

exponential(Retries, Factor, Timeout, MaxTimeout) when
    ?is_retries(Retries) andalso
    ?is_posint(Timeout) andalso
    Factor > 0 andalso
    (MaxTimeout =:= infinity orelse ?is_posint(MaxTimeout))
->
    {exponential, Retries, Factor, Timeout, MaxTimeout};

exponential(Retries = {max_total_timeout, MaxTotalTimeout}, Factor, Timeout, MaxTimeout) when
    ?is_max_total_timeout(MaxTotalTimeout) andalso
    ?is_posint(Timeout) andalso
    Factor > 0 andalso
    (MaxTimeout =:= infinity orelse ?is_posint(MaxTimeout))
->
    {exponential, compute_retries(exponential, Retries, {Factor, Timeout, MaxTimeout}), Factor, Timeout, MaxTimeout}.

-spec intervals([pos_integer(), ...]) -> strategy().

intervals(Array = [Timeout | _]) when ?is_posint(Timeout) ->
    {array, Array}.

-spec timecap(MaxTimeToSpend :: timeout(), strategy()) -> strategy().

timecap(infinity, Strategy) ->
    Strategy;
timecap(MaxTimeToSpend, Strategy) when ?is_posint(MaxTimeToSpend) ->
    Now = now_ms(),
    {timecap, Now, Now + MaxTimeToSpend, Strategy}.

%%

-spec next_step(strategy()) -> {wait, Timeout::pos_integer(), strategy()} | finish.

next_step({linear, Retries, Timeout}) when Retries > 0 ->
    {wait, Timeout, {linear, release_retry(Retries), Timeout}};
next_step({linear, _, _}) ->
    finish;

next_step({exponential, Retries, Factor, Timeout, MaxTimeout}) when Retries > 0 ->
    NewTimeout = min(round(Timeout * Factor), MaxTimeout),
    {wait, Timeout, {exponential, release_retry(Retries), Factor, NewTimeout, MaxTimeout}};
next_step({exponential, _, _, _, _}) ->
    finish;

next_step({array, []}) ->
    finish;
next_step({array, [Timeout|Remain]}) ->
    {wait, Timeout, {array, Remain}};

next_step({timecap, Last, Deadline, Strategy}) ->
    Now = now_ms(),
    case next_step(Strategy) of
        {wait, Cooldown, NextStrategy} ->
            case max(0, Cooldown - (Now - Last)) of
                Timeout when Now + Timeout > Deadline ->
                    finish;
                Timeout ->
                    {wait, Timeout, {timecap, Now + Timeout, Deadline, NextStrategy}}
            end;
        finish ->
            finish
    end;

next_step(Strategy) ->
    error(badarg, [Strategy]).

-spec compute_retries
    (
        linear,
        {max_total_timeout, non_neg_integer()},
        Timeout::pos_integer()
    ) -> non_neg_integer();
    (
        exponential,
        {max_total_timeout, non_neg_integer()},
        {Factor::number(), Timeout::pos_integer(), MaxTimeout::timeout()}
    ) -> non_neg_integer().

compute_retries(linear, {max_total_timeout, MaxTotalTimeout}, Timeout) ->
    trunc(MaxTotalTimeout/Timeout);

compute_retries(exponential, {max_total_timeout, MaxTotalTimeout}, {Factor, Timeout, MaxTimeout}) when MaxTimeout =< Timeout; Factor =:= 1->
    trunc(MaxTotalTimeout/min(Timeout, MaxTimeout));

compute_retries(exponential, {max_total_timeout, MaxTotalTimeout}, {Factor, Timeout, MaxTimeout}) ->
    B1 = Timeout, %First element
    Q = Factor, %Common ratio
    M = case MaxTimeout of
        infinity ->
            infinity; % Bi can't be bigger than MaxTimeout
        _ ->
            trunc(math:log(MaxTimeout / B1) / math:log(Q) + 1) % A threshold after which Bi changes to MaxTimeout
    end,
    N = trunc(math:log( MaxTotalTimeout * (Q - 1) / B1 + 1 ) /  math:log(Q)), % A number of iteration we would need to achieve MaxTotalTimeout
    case N < M of
        true ->
            N;
        false ->
            trunc((MaxTotalTimeout - B1 * (math:pow(Q, M - 1) - 1) / (Q - 1) + (M - 1) * MaxTimeout) / MaxTimeout)
    end.

release_retry(infinity) ->
    infinity;
release_retry(N) ->
    N - 1.

now_ms() ->
    genlib_time:ticks() div 1000.
