%%

-module(genlib_map_tests).

-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    [
        ?_assertEqual("oh", genlib_map:get(hey, Map)),
        ?_assertEqual(undefined, genlib_map:get(what_nigga, Map)),
        ?_assertEqual(babe, genlib_map:get(who, Map, babe)),
        ?_assertEqual(["say oh", "what", undefined, 42], genlib_map:mget([i, listen, dont, {know, 42}], Map))
    ].

truemap_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    InvMap = #{"oh" => hey, "what" => listen, "say oh" => i, "back and..." => come},
    [
        ?_assertEqual(InvMap, genlib_map:truemap(fun (K, V) -> {V, K} end, Map))
    ].
