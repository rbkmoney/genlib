%%
%% Genlib

-module(genlib).

%%

-export([to_binary/1]).
-export([to_list/1]).
-export([to_float/1]).

-export([format/1]).
-export([format/2]).

-export([print/2]).

-export([unique/0]).

-export([dirty_fields/3]).

-export([dice_roll/1]).

-export([round/2]).

%%

-type weighted_data() :: [{term(), pos_integer()}].

-spec to_binary(iodata() | atom() | number()) -> binary().

to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V).

-spec to_list(iodata() | atom() | number()) -> string().

to_list(V) when is_list(V)      -> V;
to_list(V)                      -> binary_to_list(to_binary(V)).

-spec to_float(iodata()) -> number().

to_float(V) ->
    Data = iolist_to_binary([V]),
    case binary:split(Data, <<$.>>) of
        [Data] ->
            binary_to_integer(Data);
        [<<>>, _] ->
            binary_to_float(<<$0, Data/binary>>);
        _ ->
            binary_to_float(Data)
    end.

%%

-spec format(term()) -> binary().

format(Arg) ->
    format("~64000p", [Arg]).

-spec format(string(), list()) -> binary().

format(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).

-spec print(term(), pos_integer()) -> binary().

print(Arg, Limit) when is_binary(Arg), byte_size(Arg) > Limit ->
    <<Part:Limit/binary, _/binary>> = Arg,
    Part;

print(Arg, _Limit) when is_binary(Arg) ->
    Arg;

print(Arg, Limit) ->
    print(unicode:characters_to_binary(genlib_trunc_io:fprint(Arg, Limit)), Limit).

%%

-spec unique() -> binary().

unique() ->
    <<I:160/integer>> = crypto:hash(sha, term_to_binary({make_ref(), now()})),
    genlib_format:format_int_base(I, 61).

%%

-spec dirty_fields(Record, Record, [Field]) -> [{Field, any()}] when
    Record :: tuple(),
    Field :: atom().

dirty_fields(R1, R2, Fields) when tuple_size(R1) =:= tuple_size(R2) andalso element(1, R1) =:= element(1, R2) ->
    L = length(Fields),
    D = tuple_size(R1) - L,
    dirty_fields(R1, R2, D + 1, length(Fields) + D + 1, Fields).

dirty_fields(_Tx0, _Tx, N, N, _Fields) ->
    [];

dirty_fields(Tx0, Tx, N, M, [F | Fs]) when element(N, Tx0) =/= element(N, Tx) ->
    [{F, element(N, Tx)} | dirty_fields(Tx0, Tx, N + 1, M, Fs)];

dirty_fields(Tx0, Tx, N, M, [_ | Fs]) ->
    dirty_fields(Tx0, Tx, N + 1, M, Fs).

-spec dice_roll(weighted_data()) -> term().

dice_roll(Data) ->
    WeightSum = lists:foldl(fun({_D, Weight}, Acc) -> Acc + Weight end, 0, Data),
    dice_roll(WeightSum, Data).

dice_roll(0, _Data) ->
    error(badarg);
dice_roll(WeightSum, Data) ->
    RandomWeight = random:uniform(WeightSum), %% random number in [1..N], i guess
    dice_roll_(RandomWeight, Data).

dice_roll_(RandomWeight, [{Data, Weight} | _]) when RandomWeight - Weight =< 0 ->
    Data;
dice_roll_(RandomWeight, [{_, Weight} | T]) ->
    dice_roll_(RandomWeight - Weight, T).

-spec round(number(), non_neg_integer()) -> float().

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.
