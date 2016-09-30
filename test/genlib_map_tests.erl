%%

-module(genlib_map_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec get_test_() -> [testcase()].
get_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    [
        ?_assertEqual("oh", genlib_map:get(hey, Map)),
        ?_assertEqual(undefined, genlib_map:get(what_nigga, Map)),
        ?_assertEqual(babe, genlib_map:get(who, Map, babe)),
        ?_assertEqual(["say oh", "what", undefined, 42], genlib_map:mget([i, listen, dont, {know, 42}], Map))
    ].

-spec truemap_test_() -> [testcase()].
truemap_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    InvMap = #{"oh" => hey, "what" => listen, "say oh" => i, "back and..." => come},
    [
        ?_assertEqual(InvMap, genlib_map:truemap(fun (K, V) -> {V, K} end, Map))
    ].

-spec deepput_test_() -> [testcase()].
deepput_test_() ->
    [
        ?_assertError(_, genlib_map:deepput(answ, 42, #{})),
        ?_assertEqual(#{answ => 42, foo => fighters}, genlib_map:deepput([answ], 42, #{foo => fighters})),
        ?_assertEqual(
            #{$b => #{$a => #{$n => #{$j => #{$o => 42}}}}, ba => zinga},
            genlib_map:deepput("banjo", 42, #{ba => zinga})
        )
    ].

-spec diff_test_() -> [testcase()].
diff_test_() ->
    Map = #{hey => "oh", listen => "what", i => "say oh", come => "back and..."},
    [
        ?_assertEqual(#{}, genlib_map:diff(Map, Map)),
        ?_assertEqual(Map, genlib_map:diff(Map, #{})),
        ?_assertEqual(#{poo => hoo}, genlib_map:diff(Map#{poo => hoo}, Map)),
        ?_assertEqual(#{hey => hoo}, genlib_map:diff(Map#{hey => hoo}, Map)),
        ?_assertEqual(#{this_is => 'undefined'}, genlib_map:diff(#{this_is => 'undefined'}, #{this_is => 'not'})),
        ?_assertEqual(#{this_is => 'not'      }, genlib_map:diff(#{this_is => 'not'}, #{this_is => 'undefined'}))
    ].
