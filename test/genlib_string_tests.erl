%%

-module(genlib_string_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec pad_numeric_test_() -> [testcase()].
pad_numeric_test_() ->
    [
        ?_assertEqual(<<"00042">>, genlib_string:pad_numeric(42, 5)),
        ?_assertEqual(<<"43">>, genlib_string:pad_numeric(43, 2)),
        ?_assertEqual(<<"007">>, genlib_string:pad_numeric(<<"7">>, 3)),
        ?_assertError(badarg, genlib_string:pad_numeric(-1, 2)),
        ?_assertError(badarg, genlib_string:pad_numeric(<<"WAARG">>, 2)),
        ?_assertError(badarg, genlib_string:pad_numeric(<<"LOST AND LOST AGAIN">>, -146))
    ].

-spec pad_right_test_() -> [testcase()].
pad_right_test_() ->
    [
        ?_assertEqual(<<"007    ">>, genlib_string:pad_string(<<"007">>, 7)),
        ?_assertEqual(<<"42">>, genlib_string:pad_string(<<"42">>, 2)),
        ?_assertError(badarg, genlib_string:pad_string(<<"WAARG">>, 2)),
        ?_assertError(badarg, genlib_string:pad_string(<<"LOST AND LOST FOREVER">>, -1492))
    ].

-spec snakecase_test_() -> [testcase()].
snakecase_test_() ->
    [
        ?_assertEqual(<<"payer_mobile_commerce">>, genlib_string:to_snakecase(<<"PayerMobileCommerce">>)),
        ?_assertEqual(<<"payer_bank_card">>, genlib_string:to_snakecase(<<"PayerBankCard">>)),
        ?_assertEqual(<<"payer_cpa">>, genlib_string:to_snakecase(<<"PayerCPA">>)),
        ?_assertEqual(<<"payer_e_wallet">>, genlib_string:to_snakecase(<<"PayerEWallet">>)),
        ?_assertEqual(<<"payer_cash">>, genlib_string:to_snakecase(<<"PayerCash">>))
    ].

-spec join_test_() -> [testcase()].
join_test_() ->
    [
        ?_assertEqual(<<"foo!bar!z">>, genlib_string:join($!, ["foo", <<"bar">>, $z])),
        ?_assertEqual(<<"foo bar z">>, genlib_string:join(["foo", <<"bar">>, $z])),
        ?_assertEqual(<<"a:=:b:=:c:=:d:=:e:=:f">>, genlib_string:join(<<":=:">>, lists:seq($a, $f))),
        ?_assertError(_, genlib_string:join($:, []))
    ].

-spec redact_test_() -> [testcase()].
redact_test_() ->
    [
        ?_assertEqual(<<>>, genlib_string:redact(<<>>, <<"">>)),
        ?_assertEqual(<<>>, genlib_string:redact(<<>>, <<"([0-9]{2})">>)),
        ?_assertEqual(
            <<"****-**/7">>,
            genlib_string:redact(<<"2017-01/7">>, <<"([0-9]{2})">>)
        ),
        ?_assertEqual(
            <<"2**7-01/7">>,
            genlib_string:redact(<<"2017-01/7">>, <<"[0-9]([0-9]{2})[0-9]">>)
        ),
        ?_assertEqual(
            <<"Это ко****о кл****а"/utf8>>,
            genlib_string:redact(<<"Это конечно клиника"/utf8>>, <<"\\w\\w(\\w{1,10})\\w">>)
        )
    ].
