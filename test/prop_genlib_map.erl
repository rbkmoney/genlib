-module(prop_genlib_map).

-include_lib("proper/include/proper.hrl").

-spec prop_fold_while() -> proper:test().
prop_fold_while() ->
    ?FORALL(
        Map,
        ?LET(KVList, list({term(), term()}), maps:from_list(KVList)),
        begin
            case map_size(Map) of
                0 ->
                    genlib_map:fold_while(
                        fun(_, _, _) -> throw(blow_up) end,
                        true,
                        Map
                    );
                Size ->
                    RandomId = rand:uniform(Size),
                    {RandomKey, RandomValue} = lists:nth(RandomId, maps:to_list(Map)),

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
        end
    ).
