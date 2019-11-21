%%
%% Datetime facilities

-module(genlib_time).

-export([
    now/0,
    unow/0,
    ticks/0,
    to_unixtime/1,
    to_gregorian/1,
    daytime_to_unixtime/1,
    unixtime_to_daytime/1
]).

-export([sanitize_date/1]).
-export([days_between/2]).
-export([shift_date/2]).

-export([
    add_days/2,
    add_hours/2,
    add_minutes/2,
    add_seconds/2,
    add_duration/2,
    sub_days/2,
    sub_hours/2,
    sub_minutes/2,
    sub_seconds/2
]).

-export([get_timezone/0]).

%%

%% There is 62167219200 seconds between Jan 1, 0 and Jan 1, 1970.
-define(EPOCH_DIFF, 62167219200).

-type ts() :: pos_integer().
-type tzoffset() :: {'-' | '+', 0..12, 0..59}.
-type timespan() :: pos_integer(). % ms

-export_type([ts/0]).
-export_type([tzoffset/0]).
-export_type([timespan/0]).

%%

-spec to_unixtime(pos_integer()) -> ts().

to_unixtime(Time) when is_integer(Time) ->
    Time - ?EPOCH_DIFF.

-spec to_gregorian(ts()) -> pos_integer().

to_gregorian(Time) when is_integer(Time) ->
    Time + ?EPOCH_DIFF.

-spec daytime_to_unixtime(calendar:datetime()) -> ts().

daytime_to_unixtime(Daytime) ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(Daytime)).

-spec unixtime_to_daytime(ts()) -> calendar:datetime().

unixtime_to_daytime(Unixtime) ->
    calendar:gregorian_seconds_to_datetime(to_gregorian(Unixtime)).

-spec local_time() -> ts().

local_time() ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(calendar:local_time())).

-spec universal_time() -> ts().

universal_time() ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

-spec now() -> ts().

now() ->
    local_time().

-spec unow() -> ts().

unow() ->
    universal_time().

-spec ticks() -> pos_integer().

ticks() ->
    {Ms, S, Mcs} = os:timestamp(),
    (Ms * 1000000 + S) * 1000000 + Mcs.

%%

-spec sanitize_date(ImproperDate) -> calendar:date() when
    ImproperDate :: calendar:date() | {0, 1..12, 1..31}.

sanitize_date({0, M, D}) ->
    {Today = {Y, _, _}, _} = calendar:local_time(),
    Candidates = [{Y + N, M, D} || N <- [-1, 0, 1]],
    [Date | _] = lists:sort(fun (D1, D2) -> abs(days_between(Today, D1)) < abs(days_between(Today, D2)) end, Candidates),
    Date;

sanitize_date(Date) ->
    Date.

-spec days_between(calendar:date(), calendar:date()) -> integer().

days_between(D1, D2) ->
    calendar:date_to_gregorian_days(D2) - calendar:date_to_gregorian_days(D1).

-type date_interval() :: {integer(), integer(), integer()}.

-spec shift_date(calendar:date(), integer() | date_interval()) -> calendar:date().

shift_date({Y, M, D}, {Ys, Ms, Ds}) ->
    Date1 = normalize_month_days(normalize_year({Y + Ys, M + Ms, D})),
    shift_date(Date1, Ds);
shift_date(Date, Ds) when is_integer(Ds) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Ds).

normalize_year({Y, M, D}) when M > 12 orelse M < 1 ->
    Ms = (Y * 12 + (M - 1)),
    {Ms div 12, Ms rem 12 + 1, D};
normalize_year(Date) ->
    Date.

normalize_month_days({Y, M, D}) ->
    {Y, M, erlang:min(D, calendar:last_day_of_the_month(Y, M))}.

-type duration() :: {0..24, 0..59, 0..59} | {Days :: non_neg_integer(), 0..24, 0..59, 0..59}.

-spec add_days(ts(), non_neg_integer()) -> ts().
add_days(Point, Days) -> add_duration(Point, {Days, 0, 0, 0}).

-spec add_hours(ts(), non_neg_integer()) -> ts().
add_hours(Point, Hours) -> add_duration(Point, {Hours, 0, 0}).

-spec add_minutes(ts(), non_neg_integer()) -> ts().
add_minutes(Point, Minutes) -> add_duration(Point, {0, Minutes, 0}).

-spec add_seconds(ts(), non_neg_integer()) -> ts().
add_seconds(Point, Seconds) -> Point + Seconds.

-spec add_duration(ts(), duration()) -> ts().
add_duration(Point, Duration) -> Point + duration_to_seconds(Duration).

-spec sub_days(ts(), non_neg_integer()) -> ts().
sub_days(Point, Days) -> sub_duration(Point, {Days, 0, 0, 0}).

-spec sub_hours(ts(), non_neg_integer()) -> ts().
sub_hours(Point, Hours) -> sub_duration(Point, {Hours, 0, 0}).

-spec sub_minutes(ts(), non_neg_integer()) -> ts().
sub_minutes(Point, Minutes) -> sub_duration(Point, {0, Minutes, 0}).

-spec sub_seconds(ts(), non_neg_integer()) -> ts().
sub_seconds(Point, Seconds) -> Point - Seconds.

-spec sub_duration(ts(), duration()) -> ts().
sub_duration(Point, Duration) -> Point - duration_to_seconds(Duration).

duration_to_seconds({H, M, S}) -> (H * 60 + M) * 60 + S;
duration_to_seconds({D, H, M, S}) -> duration_to_seconds({D * 24 + H, M, S}).

%%

-spec get_timezone() -> tzoffset().

get_timezone() ->
    UNow = calendar:universal_time(),
    Now = calendar:universal_time_to_local_time(UNow),
    MinuteDiff = (daytime_to_unixtime(Now) - daytime_to_unixtime(UNow)) div 60,
    Sign = if MinuteDiff < 0 -> '-'; true -> '+' end,
    {Sign, abs(MinuteDiff) div 60, abs(MinuteDiff) rem 60}.
