%%

-module(genlib_format_tests).

-include_lib("eunit/include/eunit.hrl").

hex_test() ->
    ?assertEqual(<<"DEADBEEF">>, genlib_format:binary_to_hex(genlib_format:hex_to_binary(<<"deadbeef">>))),
    ?assertError(badarg, genlib_format:hex_to_binary(<<"FEZZ">>)),
    ?assertError(badarg, genlib_format:hex_to_binary(<<"DEF">>)).

datetime_test() ->
    ?assertEqual(<<"17.01.1989">>, genlib_format:format_date([dd, $., mm, $., yyyy], {1989, 1, 17})),
    ?assertEqual(<<"17/01/89">>, genlib_format:format_date([dd, $/, mm, $/, yy], {1989, 1, 17})).
