%%
%% Formatting

-module(genlib_format).

-export([format_int_base/2]).
-export([parse_int_base/2]).

-export([format_decimal/2]).

-export([format_date/2]).

-export([format_datetime/2]).
-export([format_datetime_iso8601/1]).
-export([format_datetime_iso8601_local_tz/1]).
-export([format_datetime_iso8601_tz/2]).

-export([format_timestamp/2]).
-export([format_timestamp_iso8601/1]).
-export([format_timestamp_iso8601_local_tz/1]).
-export([format_timestamp_iso8601_tz/2]).

-export([format_peer/1]).
-export([format_stacktrace/1]).
-export([format_stacktrace/2]).

-export([binary_to_hex/1]).
-export([binary_to_hex/2]).
-export([hex_to_binary/1]).

-export([parse_numeric/2]).
-export([parse_datetime_iso8601/1]).
-export([parse_datetime_iso8601_tz/1]).

%%

-spec format_int_base(Integer :: integer(), Base :: integer()) -> binary().

format_int_base(I, Base) when is_integer(I), is_integer(Base), Base >= 2, Base =< 62 ->
    R = list_to_binary(format_int_base(abs(I), Base, [])),
    if
        I  > 0 -> R;
        I == 0 -> <<$0>>;
        I  < 0 -> <<$-, R/binary>>
    end;

format_int_base(I, Base) ->
    error(badarg, [I, Base]).

format_int_base(0, _Base, R0) ->
    R0;

format_int_base(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if
        D >= 36 -> [D - 36 + $a | R0];
        D >= 10 -> [D - 10 + $A | R0];
        true    -> [D + $0 | R0]
    end,
    format_int_base(I1, Base, R1).

-spec parse_int_base(binary(), Base :: integer()) -> integer().

parse_int_base(<<$-, Bin/binary>>, Base) when byte_size(Bin) > 0 ->
    -parse_int_base(Bin, Base);

parse_int_base(Bin, Base) when byte_size(Bin) > 0, is_integer(Base), Base >= 2, Base =< 62 ->
    parse_int_base(Bin, Base, 0);

parse_int_base(Bin, Base) ->
    error(badarg, [Bin, Base]).

parse_int_base(<<>>, _Base, R) ->
    R;

parse_int_base(<<D, Bin/binary>>, Base, R0) ->
    parse_int_base(Bin, Base, R0 * Base + if
        D >= $a -> D - $a + 36;
        D >= $A -> D - $A + 10;
        true    -> D - $0
    end).

%%

-spec format_decimal(integer(), Exponent :: non_neg_integer()) -> binary().

format_decimal(N, 0) when is_integer(N) ->
    integer_to_binary(N);

format_decimal(N, Exp) when is_integer(N), is_integer(Exp), Exp > 0 ->
    S0 = integer_to_binary(N),
    S1 = genlib_string:pad_left(S0, $0, max(byte_size(S0), Exp + 1)),
    {S2, S3} = split_binary(S1, byte_size(S1) - Exp),
    <<S2/binary, $., S3/binary>>;

format_decimal(_, _) ->
    error(badarg).

%%

-type date() :: {non_neg_integer(), 1..12, 1..31}.
-type time() :: {0..23, 0..60, 0..60}.
-type datetime() :: {date(), time()}.
-type format() :: [yyyy | yy | mm | dd | h | m | s | iodata() | char()].

-spec format_date(format(), date()) -> binary().

format_date(Format, Date) ->
    format_datetime(Format, {Date, undefined}, <<>>).

-spec format_datetime(format(), datetime()) -> binary().

format_datetime(Format, DateTime) ->
    format_datetime(Format, DateTime, <<>>).

format_datetime([], _Date, Bin) ->
    Bin;

format_datetime([Format | Rest], DateTime, Bin) ->
    format_datetime(Rest, DateTime, format_datetime_part(Format, DateTime, Bin)).

-spec format_datetime_iso8601(datetime()) -> binary().

format_datetime_iso8601(Datetime) ->
    Part = format_datetime_iso8601_(Datetime),
    <<Part/binary, $Z>>.

-spec format_datetime_iso8601_local_tz(datetime()) -> binary().

format_datetime_iso8601_local_tz(Datetime) ->
    format_datetime_iso8601_tz(Datetime, genlib_time:get_timezone()).

-spec format_datetime_iso8601_tz(datetime(), genlib_time:tzoffset()) -> binary().

format_datetime_iso8601_tz(Datetime, {Sign, H, M}) ->
    Part = format_datetime_iso8601_(Datetime),
    SignPart = atom_to_binary(Sign, latin1),
    HoursPart = genlib_string:pad_numeric(H, 2),
    MinutesPart = genlib_string:pad_numeric(M, 2),
    <<Part/binary, SignPart/binary, HoursPart/binary, $:, MinutesPart/binary>>.

format_datetime_iso8601_(Datetime) ->
    format_datetime([yyyy, $-, mm, $-, dd, $T, h, $:, m, $:, s], Datetime).

-define(cat(E), <<Bin/binary, (E)/binary>>).

format_datetime_part(yyyy    , {{Y, _, _}, _}, Bin) -> ?cat(genlib_string:pad_numeric(Y, 4));
format_datetime_part(yy      , {{Y, _, _}, _}, Bin) -> ?cat(genlib_string:pad_numeric(Y rem 100, 2));
format_datetime_part(mm      , {{_, M, _}, _}, Bin) -> ?cat(genlib_string:pad_numeric(M, 2));
format_datetime_part(dd      , {{_, _, D}, _}, Bin) -> ?cat(genlib_string:pad_numeric(D, 2));
format_datetime_part(h       , {_, {H, _, _}}, Bin) -> ?cat(genlib_string:pad_numeric(H, 2));
format_datetime_part(m       , {_, {_, M, _}}, Bin) -> ?cat(genlib_string:pad_numeric(M, 2));
format_datetime_part(s       , {_, {_, _, S}}, Bin) -> ?cat(genlib_string:pad_numeric(S, 2));

format_datetime_part(Bin    , _DateTime, Bin) when is_binary(Bin)   -> ?cat(Bin);
format_datetime_part(String , _DateTime, Bin) when is_list(String)  -> ?cat(list_to_binary(String));
format_datetime_part(Char   , _DateTime, Bin) when is_integer(Char) -> <<Bin/binary, Char>>.

%%

-spec format_timestamp(format(), genlib_time:ts()) -> binary().

format_timestamp(Format, Ts) ->
    format_datetime(Format, genlib_time:unixtime_to_daytime(Ts)).

-spec format_timestamp_iso8601(genlib_time:ts()) -> binary().

format_timestamp_iso8601(Ts) ->
    format_datetime_iso8601(genlib_time:unixtime_to_daytime(Ts)).

-spec format_timestamp_iso8601_local_tz(genlib_time:ts()) -> binary().

format_timestamp_iso8601_local_tz(Ts) ->
    format_datetime_iso8601_local_tz(genlib_time:unixtime_to_daytime(Ts)).

-spec format_timestamp_iso8601_tz(genlib_time:ts(), genlib_time:tzoffset()) -> binary().

format_timestamp_iso8601_tz(Ts, Tz) ->
    format_datetime_iso8601_tz(genlib_time:unixtime_to_daytime(Ts), Tz).

%%

-spec format_peer({inet:ip_address(), non_neg_integer()}) -> binary().

format_peer({IP, Port}) when size(IP) > 4 ->
    <<$[, (ntoa(IP))/binary, $], $:, (integer_to_binary(Port))/binary>>;

format_peer({IP, Port}) ->
    <<(ntoa(IP))/binary, $:, (integer_to_binary(Port))/binary>>.

ntoa(IP) ->
    list_to_binary(inet:ntoa(IP)).

%%

-type stack_item() :: {
    Module      :: module(),
    Function    :: atom(),
    ArityOrArgs :: arity() | [term()],
    Location    :: [{file, string()} | {line, pos_integer()}]
}.

-spec format_stacktrace([stack_item()]) -> binary().

format_stacktrace(Trace) ->
    format_stacktrace(Trace, []).

-spec format_stacktrace([stack_item()], Opts :: [newlines]) -> binary().

format_stacktrace(Trace, Opts) ->
    try format_stacktrace(Trace, lists:member(newlines, Opts), <<>>) catch
        _:_ -> genlib:print(Trace, 640)
    end.

-define(MAX_ARGLIST_LENGTH, 64).

format_stacktrace([], _Opts, Acc) ->
    Acc;

format_stacktrace([{Module, Function, As, Opts} | Rest], Nl, Acc) ->
    I = case Acc of
        <<>>      -> <<"in call to">>;
        _ when Nl -> <<$\t, "called from">>;
        _         -> <<"called from">>
    end,
    E = case Rest of
        []                -> <<>>;
        _ when Nl         -> <<$\n>>;
        _                 -> <<",", $\s>>
    end,
    M = atom_to_binary(Module, utf8),
    F = atom_to_binary(Function, utf8),
    Acc1 = <<Acc/binary, I/binary, " ", M/binary, ":", F/binary>>,
    Acc2 = format_args(As, Acc1),
    Acc3 = format_site(Opts, Acc2),
    format_stacktrace(Rest, Nl, <<Acc3/binary, E/binary>>).

format_args(Args, Acc) when is_list(Args) ->
    case genlib:print(Args, ?MAX_ARGLIST_LENGTH) of
        R when byte_size(R) =:= ?MAX_ARGLIST_LENGTH ->
            <<_, R0/binary>> = R,
            case binary:last(R0) of
                $. -> <<Acc/binary, "(", R0/binary, ")">>;
                _ -> <<Acc/binary, "(", R0/binary, "...)">>
            end;
        R ->
            R0 = binary:part(R, 1, byte_size(R) - 2),
            <<Acc/binary, "(", R0/binary, ")">>
    end;

format_args(Arity, Acc) when is_integer(Arity) ->
    <<Acc/binary, $/, (integer_to_binary(Arity))/binary>>.

format_site(Opts, Acc) ->
    format_site(genlib_opts:get(file, Opts), genlib_opts:get(line, Opts), Acc).

format_site(undefined, _, Acc) ->
    Acc;

format_site(Filename, undefined, Acc) ->
    F = genlib:to_binary(Filename),
    <<Acc/binary, " at ", F/binary>>;

format_site(Filename, Line, Acc) ->
    F = genlib:to_binary(Filename),
    L = integer_to_binary(Line),
    <<Acc/binary, " at ", F/binary, ":", L/binary>>.

%%

-spec binary_to_hex(binary()) -> binary().

binary_to_hex(V) ->
    binary_to_hex(V, true).

-spec binary_to_hex(binary(), UpperAlpha :: boolean()) -> binary().

binary_to_hex(V, true) ->
    binary_to_hex_(V, $A);

binary_to_hex(V, false) ->
    binary_to_hex_(V, $a).

binary_to_hex_(V, A) ->
    <<
        <<(if C >= 10 -> C + A - 10; true -> C + $0 end)>> || <<C:4>> <= V
    >>.

-spec hex_to_binary(binary()) -> binary().

hex_to_binary(V) when byte_size(V) rem 2 =:= 0 ->
    <<
        <<(hex_to_int(H)):4, (hex_to_int(L)):4>> || <<H:8, L:8>> <= V
    >>;

hex_to_binary(_) ->
    error(badarg).

hex_to_int(V) when V >= $A andalso V =< $F -> V - $A + 10;
hex_to_int(V) when V >= $a andalso V =< $f -> V - $a + 10;
hex_to_int(V) when V >= $0 andalso V =< $9 -> V - $0;
hex_to_int(_)                              -> error(badarg).

%%

-spec parse_numeric([pos_integer() | iodata()], binary()) -> {[non_neg_integer()], binary()}.

parse_numeric(Fmt, Bin) ->
    parse_numeric(Fmt, [], Bin).

parse_numeric([], Acc, Bin) ->
    {lists:reverse(Acc), Bin};

parse_numeric([S | Rest], Acc, Bin) when is_list(S) orelse is_binary(S) ->
    SBin = iolist_to_binary(S),
    Size = byte_size(SBin),
    case Bin of
        <<SBin:Size/binary, RestBin/binary>> ->
            parse_numeric(Rest, Acc, RestBin);
        _ ->
            error(badarg)
    end;

parse_numeric([N | Rest], Acc, Bin) when is_integer(N) andalso N > 0 andalso byte_size(Bin) >= N ->
    <<F:N/binary, RestBin/binary>> = Bin,
    parse_numeric(Rest, [binary_to_integer(F) | Acc], RestBin);

parse_numeric(_, _, _) ->
    error(badarg).

-spec parse_datetime_iso8601(binary()) -> datetime().

parse_datetime_iso8601(Bin) ->
    Format = [4, <<$->>, 2, <<$->>, 2, <<$T>>, 2, <<$:>>, 2, <<$:>>, 2, <<$Z>>],
    {[Year, Month, Day, Hour, Minute, Second], <<>>} = parse_numeric(Format, Bin),
    {{Year, Month, Day}, {Hour, Minute, Second}}.

-spec parse_datetime_iso8601_tz(binary()) -> {datetime(), genlib_time:tzoffset()}.

parse_datetime_iso8601_tz(Bin) ->
    Format = [4, <<$->>, 2, <<$->>, 2, <<$T>>, 2, <<$:>>, 2, <<$:>>, 2],
    {[Year, Month, Day, Hour, Minute, Second], Rest} = parse_numeric(Format, Bin),
    Datetime = {{Year, Month, Day}, {Hour, Minute, Second}},
    Timezone = case Rest of
        <<C, TzBin/binary>> when C =:= $-; C =:= $+ ->
            {[TzHours, TzMinutes], <<>>} = parse_numeric([2, <<$:>>, 2], TzBin),
            {list_to_atom([C]), TzHours, TzMinutes};
        _ ->
            error(badarg)
    end,
    {Datetime, Timezone}.

