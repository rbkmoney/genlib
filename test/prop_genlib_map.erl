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

map() ->
    ?LET(KVList, list({term(), term()}), maps:from_list(KVList)).

non_empty_map() ->
    ?LET(KVList, non_empty(list({term(), term()})), maps:from_list(KVList)).

random_map_pair(Map) ->
    RandomId = rand:uniform(map_size(Map)),
    lists:nth(RandomId, maps:to_list(Map)).
