-module(genlib_list_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec wrap_empty_list_for_undefined_test() -> _.

wrap_empty_list_for_undefined_test() ->
    ?assertEqual(genlib_list:wrap(undefined), []).

-spec find_empty_list_test() -> _.
find_empty_list_test() ->
    ?assertEqual(error, genlib_list:find(fun(_) -> true end, [])).

-spec find_missing_element_test() -> _.
find_missing_element_test() ->
    ?assertEqual(error, genlib_list:find(fun(X) -> X =:= 4 end, [1, 2, 3])).
