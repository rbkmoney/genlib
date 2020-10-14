%
% genlib_wildcard tests

-module(genlib_wildcard_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec match_test_() -> [testcase()].
match_test_() ->
    lists:map(
        fun({{Pattern, Text}, Result}) ->
            ?_assertEqual(Result, genlib_wildcard:match(Pattern, Text))
        end,
        [
            {{<<"*">>, <<"\\*">>}, true},
            {{<<"?">>, <<"\\?">>}, true},
            {{<<"\\1">>, <<"\\\\?">>}, true},
            {{<<"\\1">>, <<"\\\\\\1">>}, true},
            {{<<"1231312312312">>, <<"*">>}, true},
            {{<<"abc">>, <<"abc">>}, true},
            {{<<"abc">>, <<"*abc*****">>}, true},
            {{<<"abc">>, <<"*ab?*****">>}, true},
            {{<<"abc12123">>, <<"abc*">>}, true},
            {{<<"123123abc123123">>, <<"*abc*">>}, true},
            {{<<"abc">>, <<"abc*">>}, true},
            {{<<"abc">>, <<"*abc*">>}, true},
            {{<<"abc1">>, <<"abc?">>}, true},
            {{<<"1abc">>, <<"?abc">>}, true},
            {{<<"abc11121ty">>, <<"abc*t?">>}, true},
            {{<<"abc1111def">>, <<"abc*def">>}, true},
            {{<<"abc111de11def">>, <<"abc*def">>}, true},
            {{<<"a1b1c1">>, <<"a?b?c?">>}, true},
            {{<<"def">>, <<"abc">>}, false},
            {{<<"ab">>, <<"abc*">>}, false},
            {{<<"11231313ab1231ab">>, <<"*abc">>}, false},
            {{<<"abc">>, <<"abc?">>}, false},
            {{<<"abcd">>, <<"abc??">>}, false},
            {{<<"abc">>, <<"ab?c">>}, false},
            {{<<"123123ab*">>, <<"*abc*">>}, false},
            {{<<"123123abc">>, <<"*abc*?">>}, false},
            {{<<"abc11">>, <<"abc\\*">>}, false}
        ]
    ).

-spec unicode_test_() -> [testcase()].
unicode_test_() ->
    [
        ?_assertEqual(true, genlib_wildcard:match(<<"Казалось бы, но нет"/utf8>>, <<"*">>)),
        ?_assertEqual(true, genlib_wildcard:match(<<"Казалось бы, но нет"/utf8>>, <<"Казалось б?, *"/utf8>>)),
        ?_assertEqual(false, genlib_wildcard:match(<<"Казалось бы, но нет"/utf8>>, <<"Казалось б??, *"/utf8>>)),
        ?_assertEqual(true, genlib_wildcard:match(<<"Казалось бы? Nope"/utf8>>, <<"Казалось б?\\?*"/utf8>>))
    ].
