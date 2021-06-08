-module(prop_genlib_list).

-include_lib("proper/include/proper.hrl").

-spec prop_wrap() -> proper:test().
prop_wrap() ->
    ?FORALL(
        Term,
        term(),
        begin
            Result = genlib_list:wrap(Term),
            %% List must stay the same, anything else (except `undefined`) ­
            (is_list(Term) and (Result == Term)) or is_list(Result)
        end
    ).

-spec prop_foldl_while() -> proper:test().
prop_foldl_while() ->
    ?FORALL(
        [Int1, Int2],
        [pos_integer(), pos_integer()],
        begin
            SeqLength = max(Int1, Int2),
            Limit = min(Int1, Int2),

            Seq = lists:seq(1, SeqLength),
            SimpleResult = length(lists:filter(fun(E) -> E =< Limit end, Seq)),
            FoldlWhileResult =
                genlib_list:foldl_while(
                    fun(E, Acc) ->
                        NewAcc = Acc + 1,
                        case E < Limit of
                            true -> {cont, NewAcc};
                            false -> {halt, NewAcc}
                        end
                    end,
                    0,
                    Seq
                ),
            SimpleResult =:= FoldlWhileResult
        end
    ).

-spec prop_find() -> proper:test().
prop_find() ->
    ?FORALL(
        List,
        ?SUCHTHAT(List, list(), List /= []),
        begin
            Length = length(List),
            Index = rand:uniform(Length),
            Value = lists:nth(Index, List),
            IndexedList = lists:zip(lists:seq(1, Length), List),

            Pred = fun({_Idx, X}) -> X =:= Value end,

            Actual = genlib_list:find(Pred, IndexedList),
            Expected = genlib_list:foldl_while(
                fun(E, Acc) ->
                    case Pred(E) of
                        true -> {halt, {ok, E}};
                        false -> {cont, Acc}
                    end
                end,
                error,
                IndexedList
            ),

            Actual =:= Expected
        end
    ).

-spec prop_orderless_equal() -> proper:test().
prop_orderless_equal() ->
    ?FORALL(
        [List1, List2],
        [list(), list()],
        genlib_list:orderless_equal(List1, List2) == (lists:sort(List1) == lists:sort(List2))
    ).

-spec prop_group_by_has_same_values() -> proper:test().
prop_group_by_has_same_values() ->
    ?FORALL(
        List,
        list(),
        begin
            Result = genlib_list:group_by(fun(X) -> X end, List),
            AllValues = lists:flatmap(fun(X) -> X end, maps:values(Result)),

            lists:sort(List) =:= lists:sort(AllValues)
        end
    ).

-spec prop_group_by_groups_correctly() -> proper:test().
prop_group_by_groups_correctly() ->
    ?FORALL(
        List,
        list(),
        begin
            Result = genlib_list:group_by(fun(X) -> X end, List),

            maps:fold(
                fun
                    (_, _, false) ->
                        false;
                    (Key, Values, true) ->
                        Filtered = lists:filter(fun(K) -> K =:= Key end, List),
                        genlib_list:orderless_equal(Values, Filtered)
                end,
                true,
                Result
            )
        end
    ).
