%%%
%%% Genlib
%%% Rational number arithmetic subset
%%%
%%% # Rounding rule
%%%
%%% Commercial rounding [1], round up iff the remainder is not less than the
%%% half of the denomonator in terms of absolute value:
%%%
%%%     round(  5 /  3) =  2
%%%     round( -5 /  3) = -2
%%%     round(  7 /  5) =  1
%%%     round( -7 /  5) = -1
%%%     round( 15 / 10) =  2
%%%     round(-15 / 10) = -2
%%%
%%% [1]: https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
%%%

-module(genlib_rational).

%%

-opaque t() :: {integer(), pos_integer()}.
-export_type([t/0]).

-export([new/1]).
-export([new/2]).
-export([round/1]).

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

-spec round(t()) -> integer().

round({0, _}) ->
    0;
round({P, Q}) when P > 0 ->
    P div Q + case 2 * (P rem Q) < Q of true -> 0; false -> 1 end;
round({P, Q}) ->
    -round({-P, Q}).

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
