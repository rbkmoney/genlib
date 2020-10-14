%%%
%%% Copyright 2020 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(genlib_rfc3339_tests).

-include_lib("eunit/include/eunit.hrl").

-type testcase() :: {_, fun()}.

-spec test() -> _.

-spec utc_test_() -> [testcase()].

utc_test_() ->
    [
        ?_assertEqual(
            false,
            genlib_rfc3339:is_utc(<<"2020-02-18T15:32:15+03:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_rfc3339:is_utc(<<"2020-02-18T15:32:15-00:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_rfc3339:is_utc(<<"2020-02-18T15:32:15+00:00">>)
        ),
        ?_assertEqual(
            true,
            genlib_rfc3339:is_utc(<<"2020-02-18T15:32:15Z">>)
        )
    ].

-spec parse_test_() -> [testcase()].
parse_test_() ->
    [
        ?_assertEqual(
            503640306,
            genlib_rfc3339:parse(<<"1985-12-17T04:05:06.123Z">>, second)
        ),
        ?_assertEqual(
            503640306123,
            genlib_rfc3339:parse(<<"1985-12-17T04:05:06.123456Z">>, millisecond)
        ),
        ?_assertEqual(
            503640306123456,
            genlib_rfc3339:parse(<<"1985-12-17T04:05:06.123456Z">>, microsecond)
        ),
        ?_assertEqual(
            503640306123456789,
            genlib_rfc3339:parse(<<"1985-12-17T04:05:06.123456789Z">>, nanosecond)
        )
    ].

-spec format_test_() -> [testcase()].
format_test_() ->
    [
        ?_assertEqual(
            <<"1985-12-17T04:05:06Z">>,
            genlib_rfc3339:format(503640306, second)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123Z">>,
            genlib_rfc3339:format(503640306123, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456Z">>,
            genlib_rfc3339:format(503640306123456, microsecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456789Z">>,
            genlib_rfc3339:format(503640306123456789, nanosecond)
        )
    ].

-spec format_relaxed_test_() -> [testcase()].
format_relaxed_test_() ->
    [
        ?_assertEqual(
            <<"1985-12-17T04:05:06Z">>,
            genlib_rfc3339:format_relaxed(503640306, second)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123Z">>,
            genlib_rfc3339:format_relaxed(503640306123, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456Z">>,
            genlib_rfc3339:format_relaxed(503640306123456, microsecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456789Z">>,
            genlib_rfc3339:format_relaxed(503640306123456789, nanosecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123456Z">>,
            genlib_rfc3339:format_relaxed(503640306123456000, nanosecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123Z">>,
            genlib_rfc3339:format_relaxed(503640306123000, microsecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06Z">>,
            genlib_rfc3339:format_relaxed(503640306000, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.200Z">>,
            genlib_rfc3339:format_relaxed(503640306200, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.210Z">>,
            genlib_rfc3339:format_relaxed(503640306210, millisecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06Z">>,
            genlib_rfc3339:format_relaxed(503640306000000000, nanosecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.123Z">>,
            genlib_rfc3339:format_relaxed(503640306123000000, nanosecond)
        ),
        ?_assertEqual(
            <<"1985-12-17T04:05:06.100Z">>,
            genlib_rfc3339:format_relaxed(503640306100000000, nanosecond)
        )
    ].
