%%

-module(genlib_string_tests).

-include_lib("eunit/include/eunit.hrl").

pad_numeric_test_() ->
    [
        ?_assertEqual(<<"00042">>, genlib_string:pad_numeric(42, 5)),
        ?_assertEqual(<<"43">>, genlib_string:pad_numeric(43, 2)),
        ?_assertEqual(<<"007">>, genlib_string:pad_numeric(<<"7">>, 3)),
        ?_assertError(badarg, genlib_string:pad_numeric(-1, 2)),
        ?_assertError(badarg, genlib_string:pad_numeric(<<"WAARG">>, 2)),
        ?_assertError(badarg, genlib_string:pad_numeric(<<"LOST AND LOST AGAIN">>, -146))
    ].

pad_right_test_() ->
    [
        ?_assertEqual(<<"007    ">>, genlib_string:pad_string(<<"007">>, 7)),
        ?_assertEqual(<<"42">>, genlib_string:pad_string(<<"42">>, 2)),
        ?_assertError(badarg, genlib_string:pad_string(<<"WAARG">>, 2)),
        ?_assertError(badarg, genlib_string:pad_string(<<"LOST AND LOST FOREVER">>, -1492))
    ].

snakecase_test_() ->
    [
        ?_assertEqual(<<"payer_mobile_commerce">> , genlib_string:to_snakecase(<<"PayerMobileCommerce">>)),
        ?_assertEqual(<<"payer_bank_card">>       , genlib_string:to_snakecase(<<"PayerBankCard">>)),
        ?_assertEqual(<<"payer_cpa">>             , genlib_string:to_snakecase(<<"PayerCPA">>)),
        ?_assertEqual(<<"payer_e_wallet">>        , genlib_string:to_snakecase(<<"PayerEWallet">>)),
        ?_assertEqual(<<"payer_cash">>            , genlib_string:to_snakecase(<<"PayerCash">>))
    ].

join_test_() ->
    [
        ?_assertEqual(<<"foo!bar!z">>             , genlib_string:join($!, ["foo", <<"bar">>, $z])),
        ?_assertEqual(<<"foo bar z">>             , genlib_string:join(["foo", <<"bar">>, $z])),
        ?_assertEqual(<<"a:=:b:=:c:=:d:=:e:=:f">> , genlib_string:join(<<":=:">>, lists:seq($a, $f))),
        ?_assertError(_                           , genlib_string:join($:, []))
    ].
