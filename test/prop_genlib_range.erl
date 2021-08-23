-module(prop_genlib_range).

-include_lib("proper/include/proper.hrl").

-spec prop_map() -> proper:test().
prop_map() ->
    ?FORALL(
        Range = {From, To, Step},
        range(),
        lists_seq(From, To, Step) =:= genlib_range:map(fun identity/1, Range)
    ).

-spec prop_foldl() -> proper:test().
prop_foldl() ->
    ?FORALL(
        Range = {From, To, Step},
        range(),
        lists:foldl(fun sum/2, 0, lists_seq(From, To, Step)) =:= genlib_range:foldl(fun sum/2, 0, Range)
    ).

identity(X) ->
    X.

sum(A, B) ->
    A + B.

%% Workaround missing if statements in implementation
lists_seq(From, To, Step) when From < To, Step < 0 ->
    [];
lists_seq(From, To, Step) when From > To, Step > 0 ->
    [];
lists_seq(From, To, Step) ->
    lists:seq(From, To, Step).

range() ->
    {integer(), integer(), non_zero_integer()}.

non_zero_integer() ->
    oneof([
        neg_integer(),
        pos_integer()
    ]).
