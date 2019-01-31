%%
%% Binary strings common manipulations

-module(genlib_string).

-export([pad_string/2]).
-export([pad_numeric/2]).
-export([pad_right/3]).
-export([pad_left/3]).

-export([trim/1]).
-export([trim_left/1]).
-export([trim_right/1]).

-export([cat/2]).
-export([cat/1]).

-export([join/1]).
-export([join/2]).

-export([to_lower/1]).
-export([to_upper/1]).
-export([to_snakecase/1]).
-export([is_equal_icase/2]).

-export([redact/2]).

%%

-spec pad_string(binary(), pos_integer()) -> binary().

pad_string(S, Length) ->
    pad_right(S, $\s, Length).

-spec pad_numeric(integer() | binary(), pos_integer()) -> binary().

pad_numeric(I, Length) when is_integer(I) andalso I >= 0 ->
    pad_numeric(integer_to_binary(I), Length);

pad_numeric(S, Length) ->
    pad_left(S, $0, Length).

-spec pad_right(binary(), 0..255, pos_integer()) -> binary().

pad_right(S, C, Length) ->
    padr(S, C, Length - byte_size(S)).

-spec pad_left(binary(), 0..255, pos_integer()) -> binary().

pad_left(S, C, Length) ->
    padl(S, C, Length - byte_size(S), <<>>).

padr(S, C, N) when N > 0      -> padr(<<S/binary, C>>, C, N - 1);
padr(S, _, 0)                 -> S;
padr(_, _, _)                 -> error(badarg).

padl(S, C, N, Buf) when N > 0 -> padl(S, C, N - 1, <<Buf/binary, C>>);
padl(S, _, 0, Buf)            -> <<Buf/binary, S/binary>>;
padl(_, _, _, _)              -> error(badarg).

-spec trim(binary()) -> binary().

trim(S) ->
    trim_left(trim_right(S)).

-spec trim_left(binary()) -> binary().

trim_left(<<C, S/binary>>) when C =:= $\s orelse C =:= $\t ->
    trim_left(S);

trim_left(S) ->
    S.

-spec trim_right(binary()) -> binary().

trim_right(S) ->
    trim_right(S, byte_size(S) - 1).

trim_right(S, N) ->
    case S of
        <<S0:N/binary, C>> when C =:= $\s orelse C =:= $\t ->
            trim_right(S0, N - 1);
        S ->
            S
    end.

-spec cat(iodata(), iodata()) -> binary().

cat(S1, S2) when is_binary(S1), is_binary(S2) ->
    <<S1/binary, S2/binary>>;

cat(S1, S2) ->
    cat(iolist_to_binary(S1), iolist_to_binary(S2)).

-spec cat([iodata(), ...]) -> binary().

cat(Ss = [_ | _]) ->
    iolist_to_binary(Ss);

cat(Badarg) ->
    error(badarg, [Badarg]).

-spec join([iodata(), ...]) -> binary().

join(List) ->
    join($\s, List).

-spec join(char() | iodata(), [iodata(), ...]) -> binary().

join(Delim, List) ->
    iolist_to_binary(join_(Delim, List)).

join_(_, [H]) ->
    [H];

join_(Delim, [H | T]) ->
    [H, Delim | join_(Delim, T)].

-spec to_lower(binary()) -> binary().

to_lower(S) ->
    to_case(lower, S, <<>>).

-spec to_upper(binary()) -> binary().

to_upper(S) ->
    to_case(upper, S, <<>>).

to_case(_Case, <<>>, Acc) ->
    Acc;

to_case(_Case, <<C, _/binary>>, _Acc) when C > 127 ->
    error(badarg);

to_case(Case = lower, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_lower_char(C))>>);

to_case(Case = upper, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_upper_char(C))>>).

-spec to_snakecase(binary()) -> binary().

to_snakecase(S) ->
    snake_case(l, S, <<>>).

-define(is_lower(C), (C >= $a andalso C =< $z)).
-define(is_upper(C), (C >= $A andalso C =< $Z)).

snake_case(l, <<L, U, Rest/binary>>, A) when ?is_lower(L), ?is_upper(U) ->
    snake_case(u, Rest, <<A/binary, L, $_, (to_lower_char(U))>>);

snake_case(u, <<U, L, Rest/binary>>, A) when ?is_upper(U), ?is_lower(L) ->
    snake_case(l, Rest, <<A/binary, $_, (to_lower_char(U)), L>>);

snake_case(M, <<C, Rest/binary>>, A) when C < 128 ->
    snake_case(M, Rest, <<A/binary, (to_lower_char(C))>>);

snake_case(_, <<>>, A) ->
    A;

snake_case(_, _, _) ->
    error(badarg).

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    C + 32;
to_lower_char(C) ->
    C.

to_upper_char(C) when is_integer(C), $a =< C, C =< $z ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE ->
    C - 32;
to_upper_char(C) ->
    C.

-spec is_equal_icase(binary(), binary()) -> boolean().

is_equal_icase(S1, S2) ->
    to_lower(S1) =:= to_lower(S2).

%%

-spec redact(Subject :: binary(), Pattern :: binary()) -> Redacted :: binary().
redact(Subject, Pattern) ->
    case re:run(Subject, Pattern, [global, {capture, all_but_first, index}]) of
        {match, Captures} ->
            lists:foldl(fun redact_match/2, Subject, Captures);
        nomatch ->
            Subject
    end.

redact_match({S, Len}, Subject) ->
    <<Pre:S/binary, _:Len/binary, Rest/binary>> = Subject,
    <<Pre/binary, (binary:copy(<<"*">>, Len))/binary, Rest/binary>>;
redact_match([Capture], Message) ->
    redact_match(Capture, Message).
