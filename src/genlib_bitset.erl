%%
%% Bitset conversions

-module(genlib_bitset).

-export([to_binary/2]).
-export([to_binary/3]).

-export([from_binary/1]).
-export([from_binary/2]).

%%

-type bit_position() :: integer().
-type bitset()       :: [bit_position()].

%%

-spec to_binary(bitset(), pos_integer()) -> binary().

to_binary(Bs, Bitsize) ->
    to_binary(Bs, Bitsize, 0).

-spec to_binary(bitset(), pos_integer(), integer()) -> binary().

to_binary(Bs, Bitsize, Skew) when Skew < 0 ->
    N = abs(Skew),
    R0 = to_bits(Bs, 0, <<>>),
    <<_:N, R/bits>> = R0,
    S = Bitsize - bit_size(R),
    <<R/bits, 0:S>>;

to_binary(Bs, Bitsize, Skew) ->
    R = to_bits(Bs, 0, <<0:Skew>>),
    S = Bitsize - bit_size(R),
    <<R/bits, 0:S>>.

to_bits([], _, Bits)           -> Bits;
to_bits([Bi | Rest], Bi, Bits) -> to_bits(Rest, Bi + 1, <<Bits/bits, 1:1>>);
to_bits(Bs, Bj, Bits)          -> to_bits(Bs, Bj + 1, <<Bits/bits, 0:1>>).

-spec from_binary(binary()) -> bitset().

from_binary(Binary) ->
    from_binary(Binary, 0).

-spec from_binary(binary(), integer()) -> bitset().

from_binary(Binary, 0) ->
    from_binary_(Binary, 0);

from_binary(Binary, Skew) ->
    [V + Skew || V <- from_binary_(Binary, 0)].

from_binary_(<<>>, _)                -> [];
from_binary_(<<0:1, Rest/bits>>, N)  -> from_binary_(Rest, N + 1);
from_binary_(<<1:1, Rest/bits>>, N)  -> [N | from_binary_(Rest, N + 1)].
