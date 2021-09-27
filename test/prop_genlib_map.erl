-module(prop_genlib_map).

-include_lib("proper/include/proper.hrl").

-spec prop_fold_while() -> proper:test().
prop_fold_while() ->
    ?FORALL(
        Map,
        non_empty_map(),
        begin
            {RandomKey, RandomValue} = random_map_pair(Map),
            Result =
                genlib_map:fold_while(
                    fun
                        (K, V, _Acc) when K =:= RandomKey -> {halt, V};
                        (_K, _V, Acc) -> {cont, Acc}
                    end,
                    make_ref(),
                    Map
                ),

            Result =:= RandomValue
        end
    ).

-spec prop_search_found() -> proper:test().
prop_search_found() ->
    ?FORALL(
        Map,
        non_empty_map(),
        begin
            {RandomKey, RandomValue} = random_map_pair(Map),
            Result =
                genlib_map:search(
                    fun(K, _V) -> K =:= RandomKey end,
                    Map
                ),

            {RandomKey, RandomValue} =:= Result
        end
    ).

-spec prop_search_not_found() -> proper:test().
prop_search_not_found() ->
    ?FORALL(
        Map,
        map(),
        begin
            NonExistentKey = make_ref(),
            Result =
                genlib_map:search(
                    fun(K, _V) -> K =:= NonExistentKey end,
                    Map
                ),

            false =:= Result
        end
    ).

-spec prop_zipfold() -> proper:test().
prop_zipfold() ->
    ?FORALL(
        [Left, Right],
        [map(), map()],
        begin
            CommonKeys = maps_common_keys(Left, Right),

            Actual =
                genlib_map:zipfold(
                    fun(Key, LValue, RValue, Acc) -> Acc#{Key => {LValue, RValue}} end,
                    #{},
                    Left,
                    Right
                ),

            Expected =
                maps_merge_with(
                    fun(_Key, LValue, RValue) -> {LValue, RValue} end,
                    Left,
                    Right
                ),

            Actual =:= maps:with(CommonKeys, Expected)
        end
    ).

-if(?OTP_RELEASE >= 24).

maps_merge_with(Combiner, Left, Right) ->
    maps:merge_with(Combiner, Left, Right).

-else.

maps_merge_with(Combiner, Left, Right) ->
    CommonMerged =
        lists:foldl(
            fun(Key, Acc) ->
                Acc#{Key => Combiner(Key, maps:get(Key, Left), maps:get(Key, Right))}
            end,
            #{},
            maps_common_keys(Left, Right)
        ),

    maps:merge(maps:merge(Left, Right), CommonMerged).

%% END -if(?OTP_RELEASE >= 24).
-endif.

maps_common_keys(LeftMap, RightMap) ->
    sets:to_list(
        sets:intersection(
            sets:from_list(maps:keys(LeftMap)),
            sets:from_list(maps:keys(RightMap))
        )
    ).

map() ->
    ?LET(KVList, list({term(), term()}), maps:from_list(KVList)).

non_empty_map() ->
    ?LET(KVList, non_empty(list({term(), term()})), maps:from_list(KVList)).

random_map_pair(Map) ->
    RandomId = rand:uniform(map_size(Map)),
    lists:nth(RandomId, maps:to_list(Map)).
