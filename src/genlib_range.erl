-module(genlib_range).

%% @doc Module for working with number sequences (like lists:seq/2,3),
%% but more efficiently (i.e. without generating a list of numbers)
%%
%% Supports both forward- and backward-ranges (increasing and decreasing respectively)

-export([map/2]).
-export([foldl/3]).

-type bound() :: integer().
-type step() :: neg_integer() | pos_integer().
-type t() :: {bound(), bound()} | {bound(), bound(), step()}.

-define(IS_RANGE(R),
    ((is_integer(element(1, R))) andalso
        (is_integer(element(2, R))) andalso
        ((tuple_size(R) == 2) orelse
            (tuple_size(R) == 3 andalso
                is_integer(element(3, R)) andalso
                element(3, R) /= 0)))
).

%% @doc Map over range
-spec map(fun((integer()) -> T), t()) -> [T].
map(Fun, Range) when is_function(Fun, 1), ?IS_RANGE(Range) ->
    {From, To, Step} = to_extended_range(Range),
    lists:reverse(
        do_foldl(
            fun(Idx, Acc) ->
                [Fun(Idx) | Acc]
            end,
            [],
            From,
            To,
            Step
        )
    );
map(_, _) ->
    error(badarg).

%% @doc Fold over range from starting from the first boundary
-spec foldl(fun((integer(), T) -> T), T, t()) -> T.
foldl(Fun, Acc, Range) when is_function(Fun, 2), ?IS_RANGE(Range) ->
    {From, To, Step} = to_extended_range(Range),
    do_foldl(Fun, Acc, From, To, Step);
foldl(_, _, _) ->
    error(badarg).

%%
%% Internals
%%

do_foldl(_Fun, Acc, From, To, Step) when (From > To andalso Step > 0) -> Acc;
do_foldl(_Fun, Acc, From, To, Step) when (From < To andalso Step < 0) -> Acc;
do_foldl(Fun, Acc, From, To, Step) -> do_foldl(Fun, Fun(From, Acc), From + Step, To, Step).

to_extended_range({From, To}) ->
    {From, To, 1};
to_extended_range({_From, _To, _Step} = Range) ->
    Range.
