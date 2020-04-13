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

-spec rfc3339_utc_test_() -> [testcase()].

rfc3339_utc_test_() ->
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

-spec rfc3339_parse_test_() -> [testcase()].

rfc3339_parse_test_() ->
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

-spec rfc3339_format_test_() -> [testcase()].

rfc3339_format_test_() ->
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
