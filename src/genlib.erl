%%
%% Genlib

-module(genlib).

%%

-export([to_binary/1]).
-export([to_list/1]).
-export([to_float/1]).
-export([to_int/1]).

-export([format/1]).
-export([format/2]).

-export([print/2]).

-export([unique/0]).

-export([uuid/0]).
-export([bsuuid/0]).

-export([dirty_fields/3]).

-export([dice_roll/1]).

-export([round/2]).

-export([unwrap/1]).
-export([unwrap/2]).

-export_type([exception/0]).
-export([raise/1]).

%%

-type uuid() :: <<_:128>>.

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

-spec to_int(integer() | binary() | list()) -> integer().

to_int(Data) when is_integer(Data) ->
    Data;
to_int(Data) when is_binary(Data) ->
    binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
    list_to_integer(Data).

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
    <<I:160/integer>> = crypto:hash(sha, term_to_binary({make_ref(), os:timestamp()})),
    genlib_format:format_int_base(I, 61).

%%

% Gen uuid version 4
-spec uuid() -> uuid().

uuid() ->
    <<R1:48, _:4, R2:12, _:2, R3:62>> = crypto:strong_rand_bytes(16),
    <<R1:48,
      0:1, 1:1, 0:1, 0:1,  % version 4 bits
      R2:12,
      1:1, 0:1,            % RFC 4122 variant bits
      R3:62>>.

-spec bsuuid() -> binary().
bsuuid() ->
    genlib_format:uuid_to_bstring(uuid()).

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
    RandomWeight = rand:uniform(WeightSum), %% random number in [1..N], i guess
    dice_roll_(RandomWeight, Data).

dice_roll_(RandomWeight, [{Data, Weight} | _]) when RandomWeight - Weight =< 0 ->
    Data;
dice_roll_(RandomWeight, [{_, Weight} | T]) ->
    dice_roll_(RandomWeight - Weight, T).

-spec round(number(), non_neg_integer()) -> float().

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

%%

-spec unwrap
    (ok             ) -> ok;
    ({ok   , Result}) -> Result;
    ({error, _Error}) -> no_return().
unwrap(ok) ->
    ok;
unwrap({ok, R}) ->
    R;
unwrap({error, Error}) ->
    erlang:error(Error).

-spec unwrap
    (ok             , _Tag) -> ok;
    ({ok   , Result}, _Tag) -> Result;
    ( error         , _Tag) -> no_return();
    ({error, _Error}, _Tag) -> no_return().
unwrap(ok, _) ->
    ok;
unwrap({ok, R}, _) ->
    R;
unwrap(error, Tag) ->
    erlang:error(Tag);
unwrap({error, Error}, Tag) ->
    erlang:error({Tag, Error}).

%%

-type exception() :: {exit | error | throw, term(), list()}.

-spec raise(exception()) ->
    no_return().
raise({Class, Reason, Stacktrace}) ->
    erlang:raise(Class, Reason, Stacktrace).
