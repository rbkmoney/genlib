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

-module(genlib_rfc3339).

%% API
-export([
    is_utc/1,
    format/2,
    format_relaxed/2,
    parse/2
]).

-type datetime() :: binary().
% unfortunately not exported from calendar module
-type time_unit() ::
    microsecond
    | millisecond
    | nanosecond
    | second.

-export_type([datetime/0]).
-export_type([time_unit/0]).

-spec is_utc(datetime()) -> boolean().
is_utc(Rfc3339) when is_binary(Rfc3339) ->
    Size0 = erlang:byte_size(Rfc3339),
    Size1 = Size0 - 1,
    Size6 = Size0 - 6,
    case Rfc3339 of
        <<_:Size1/bytes, "Z">> ->
            true;
        <<_:Size6/bytes, "+00:00">> ->
            true;
        <<_:Size6/bytes, "-00:00">> ->
            true;
        _ ->
            false
    end.

-spec format_relaxed(integer(), time_unit()) -> datetime().
format_relaxed(Value, Unit) when is_integer(Value) andalso is_atom(Unit) ->
    {AdjTime, AdjUnit} = adjust_time_unit(Value, Unit),
    format(AdjTime, AdjUnit).

-spec format(integer(), time_unit()) -> datetime().
format(Value, Unit) when is_integer(Value) andalso is_atom(Unit) ->
    Str = calendar:system_time_to_rfc3339(Value, [{unit, Unit}, {offset, "Z"}]),
    erlang:list_to_binary(Str).

-spec parse(datetime(), time_unit()) -> integer().
parse(Rfc3339, Unit) when is_binary(Rfc3339) andalso is_atom(Unit) ->
    Str = erlang:binary_to_list(Rfc3339),
    calendar:rfc3339_to_system_time(Str, [{unit, Unit}]).

adjust_time_unit(Value, second) ->
    {Value, second};
adjust_time_unit(Value, Unit) ->
    case Value rem 1000 of
        0 ->
            NextFactor = next_time_unit(Unit),
            NextValue = erlang:convert_time_unit(Value, Unit, NextFactor),
            adjust_time_unit(NextValue, NextFactor);
        _ ->
            {Value, Unit}
    end.

next_time_unit(nanosecond) -> microsecond;
next_time_unit(microsecond) -> millisecond;
next_time_unit(millisecond) -> second.
