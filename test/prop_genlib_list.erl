-module(prop_genlib_list).

-include_lib("proper/include/proper.hrl").

-spec prop_wrap() -> proper:test().
prop_wrap() ->
    ?FORALL(
        Term,
        term(),
        case Term of
            undefined ->
                [] =:= genlib_list:wrap(Term);
            List when is_list(List) ->
                List =:= genlib_list:wrap(Term);
            _ ->
                [Term] =:= genlib_list:wrap(Term)
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

-spec prop_orderless_equal() -> proper:test().
prop_orderless_equal() ->
    ?FORALL(
        [List1, List2],
        [list(), list()],
        genlib_list:orderless_equal(List1, List2) == (lists:sort(List1) == lists:sort(List2))
    ).

-spec prop_group_by() -> proper:test().
prop_group_by() ->
    ?FORALL(
        List,
        list(),
        begin
            Result = genlib_list:group_by(fun(X) -> X end, List),
            AllValues = lists:flatmap(fun(X) -> X end, maps:values(Result)),
            SameValues = lists:sort(List) =:= lists:sort(AllValues),

            CorrectlyGrouped =
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
                ),

            SameValues and CorrectlyGrouped
        end
    ).
