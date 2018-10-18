%%%
%%% Genlib
%%% Rational number arithmetic subset
%%%
%%% # Rounding methods
%%%
%%% ## [round_half_away_from_zero][1]
%%% Round up iff the remainder is not less than the half of the denominator in terms of absolute value:
%%%
%%%     round(  5 /  3) =  2
%%%     round( -5 /  3) = -2
%%%     round(  7 /  5) =  1
%%%     round( -7 /  5) = -1
%%%     round( 15 / 10) =  2
%%%     round(-15 / 10) = -2
%%%
%%% ## [round_half_towards_zero][2]
%%% Round up iff the remainder is more than the half of the denominator in terms of absolute value:
%%%
%%%     round(  5 /  3) =  2
%%%     round( -5 /  3) = -2
%%%     round(  7 /  5) =  1
%%%     round( -7 /  5) = -1
%%%     round( 15 / 10) =  1
%%%     round(-15 / 10) = -1
%%%
%%% [1]: https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
%%% [2]: https://en.wikipedia.org/wiki/Rounding#Round_half_towards_zero
%%%
%%% By default round_half_away_from_zero method is used.

-module(genlib_rational).

%%

-opaque t() :: {integer(), pos_integer()}.
-type rounding_method() :: round_half_towards_zero | round_half_away_from_zero.

-export_type([t/0]).
-export_type([rounding_method/0]).

-export([new/1]).
-export([new/2]).
-export([num/1]).
-export([denom/1]).
-export([round/1]).
-export([round/2]).

-export([cmp/2]).

-export([neg/1]).
-export([add/2]).
-export([sub/2]).
-export([inv/1]).
-export([mul/2]).
-export([dvd/2]).

-compile([no_auto_import]).

%%

-spec new(integer()) -> t().

new(I) ->
    {I, 1}.

-spec new(integer(), neg_integer() | pos_integer()) -> t().

new(P, Q) when Q > 0 ->
    {P, Q};
new(P, Q) when Q < 0 ->
    {-P, -Q};
new(_, 0) ->
    erlang:error(badarg).

-spec num(t()) -> integer().

num({P, _}) ->
    P.

-spec denom(t()) -> neg_integer() | pos_integer().

denom({_, Q}) ->
    Q.

-spec round(t()) -> integer().

round(V) ->
    round(V, round_half_away_from_zero).

-spec round(t(), rounding_method()) -> integer().

round({0, _}, _) ->
    0;
round({P, Q}, round_half_towards_zero) when P > 0 ->
    P div Q + case 2 * (P rem Q) > Q of true -> 1; false -> 0 end;
round({P, Q}, round_half_away_from_zero) when P > 0 ->
    P div Q + case 2 * (P rem Q) < Q of true -> 0; false -> 1 end;
round({P, Q}, Method) ->
    -round({-P, Q}, Method).

-spec cmp(t(), t()) -> eq | gt | lt.

cmp(R1, R2) ->
    case num(sub(R1, R2)) of
        P when P > 0 -> gt;
        P when P < 0 -> lt;
        0 -> eq
    end.

-spec neg(t()) -> t().

neg({P, Q}) ->
    new(-P, Q).

-spec add(t(), t()) -> t().

add({P1, Q1}, {P2, Q2}) ->
    {P1 * Q2 + P2 * Q1, Q1 * Q2}.

-spec sub(t(), t()) -> t().

sub(R1, R2) ->
    add(R1, neg(R2)).

-spec inv(t()) -> t().

inv({P, Q}) ->
    new(Q, P).

-spec mul(t(), t()) -> t().

mul({P1, Q1}, {P2, Q2}) ->
    {P1 * P2, Q1 * Q2}.

-spec dvd(t(), t()) -> t().

dvd(R1, R2) ->
    mul(R1, inv(R2)).
