%%

-module(prop_genlib_pmap).

-include_lib("proper/include/proper.hrl").

-spec prop_map_semantics() ->
    proper:test().

prop_map_semantics() ->
    MapWith = fun (T) -> io_lib:format("~p", [T]) end,
    ?FORALL({List, ProcLimit}, {list(), pos_integer()},
        lists:map(MapWith, List) =:= genlib_pmap:map(MapWith, List, #{proc_limit => ProcLimit})
    ).
