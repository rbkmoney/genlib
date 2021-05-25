-module(genlib_list_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec wrap_empty_list_for_undefined_test() -> _.

wrap_empty_list_for_undefined_test() ->
    ?assertEqual(genlib_list:wrap(undefined), []).
