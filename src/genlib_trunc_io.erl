%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with your Erlang distribution. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Corelatus AB.
%% Portions created by Corelatus are Copyright 2003, Corelatus
%% AB. All Rights Reserved.''
%%
%% @doc Module to print out terms for logging. Limits by length rather than depth.
%%
%% The resulting string may be slightly larger than the limit; the intention
%% is to provide predictable CPU and memory consumption for formatting
%% terms, not produce precise string lengths.
%%
%% Typical use:
%%
%%   trunc_io:print(Term, 500).
%%
%% Source license: Erlang Public License.
%% Original author: Matthias Lang, <tt>matthias@corelatus.se</tt>
%%
%% Various changes to this module, most notably the format/3 implementation
%% were added by Andrew Thompson `<andrew@basho.com>'. The module has been renamed
%% to avoid conflicts with the vanilla module.

-module(genlib_trunc_io).
-author('matthias@corelatus.se').
%% And thanks to Chris Newcombe for a bug fix
-export([format/3, format/4, print/2, print/3, fprint/2, fprint/3, safe/2]). % interface functions
-version("$Id: trunc_io.erl,v 1.11 2009-02-23 12:01:06 matthias Exp $").

-ifdef(TEST).
-export([perf/0, perf/3, perf1/0, test/0, test/2]). % testing functions
-include_lib("eunit/include/eunit.hrl").
-endif.

-type option() :: {'depth', integer()}
    | {'lists_as_strings', boolean()}
    | {'force_strings', boolean()}.
-type options() :: [option()].

-record(print_options, {
        %% negative depth means no depth limiting
        depth = -1 :: integer(),
        %% whether to print lists as strings, if possible
        lists_as_strings = true :: boolean(),
        %% force strings, or binaries to be printed as a string,
        %% even if they're not printable
        force_strings = false :: boolean()
    }).

format(Fmt, Args, Max) ->
    format(Fmt, Args, Max, []).

format(Fmt, Args, Max, Options) ->
    try format_(Fmt, Args, Max, Options)
    catch
        _What:_Why ->
            erlang:error(badarg, [Fmt, Args])
    end.

%% @doc Returns an flattened list containing the ASCII representation of the given
%% term.
-spec fprint(term(), pos_integer()) -> string().
fprint(Term, Max) ->
    fprint(Term, Max, []).


%% @doc Returns an flattened list containing the ASCII representation of the given
%% term.
-spec fprint(term(), pos_integer(), options()) -> string().
fprint(T, Max, Options) ->
    {L, _} = print(T, Max, prepare_options(Options, #print_options{})),
    lists:flatten(L).

%% @doc Same as print, but never crashes.
%%
%% This is a tradeoff. Print might conceivably crash if it's asked to
%% print something it doesn't understand, for example some new data
%% type in a future version of Erlang. If print crashes, we fall back
%% to io_lib to format the term, but then the formatting is
%% depth-limited instead of length limited, so you might run out
%% memory printing it. Out of the frying pan and into the fire.
%%
-spec safe(term(), pos_integer()) -> {string(), pos_integer()} | {string()}.
safe(What, Len) ->
    case catch print(What, Len) of
        {L, Used} when is_list(L) -> {L, Used};
        _ -> {"unable to print" ++ io_lib:write(What, 99)}
    end.

%% @doc Returns {List, Length}
-spec print(term(), pos_integer()) -> {iolist(), pos_integer()}.
print(Term, Max) ->
    print(Term, Max, []).

%% @doc Returns {List, Length}
-spec print(term(), pos_integer(), options() | #print_options{}) -> {iolist(), pos_integer()}.
print(Term, Max, Options) when is_list(Options) ->
    %% need to convert the proplist to a record
    print(Term, Max, prepare_options(Options, #print_options{}));

print(Term, _Max, #print_options{force_strings=true}) when not is_list(Term), not is_binary(Term), not is_atom(Term) ->
    erlang:error(badarg);

print(_, Max, _Options) when Max < 0 -> {"...", 3};
print(_, _, #print_options{depth=0}) -> {"...", 3};


%% @doc We assume atoms, floats, funs, integers, PIDs, ports and refs never need
%% to be truncated. This isn't strictly true, someone could make an
%% arbitrarily long bignum. Let's assume that won't happen unless someone
%% is being malicious.
%%
print(Atom, _Max, #print_options{force_strings=NoQuote}) when is_atom(Atom) ->
    L = atom_to_list(Atom),
    R = case atom_needs_quoting_start(L) andalso not NoQuote of
        true -> lists:flatten([$', L, $']);
        false -> L
    end,
    {R, length(R)};

print(Bin, _Max, O = #print_options{depth=1}) when is_binary(Bin) ->
    case O#print_options.lists_as_strings of
        true when Bin == <<>>  ->
            {"<<>>", 4};
        _ ->
            {"<<...>>", 7}
    end;
print(<<>>, _Max, Options) ->
    case Options#print_options.force_strings of
        true ->
            {"", 0};
        false ->
            {"<<>>", 4}
    end;

print(Binary, 0, _Options) when is_bitstring(Binary) ->
    {"<<..>>", 6};

print(Bin, Max, _Options) when is_binary(Bin), Max < 2 ->
    {"<<...>>", 7};
print(Binary, Max, Options) when is_binary(Binary) ->
    B = binary_to_list(Binary, 1, lists:min([Max, byte_size(Binary)])),
    {Res, Length} = case Options#print_options.lists_as_strings orelse
        Options#print_options.force_strings of
        true ->
            Depth = Options#print_options.depth,
            MaxSize = (Depth - 1) * 4,
            %% check if we need to truncate based on depth
            In = case Depth > -1 andalso MaxSize < length(B) andalso
                not Options#print_options.force_strings of
                true ->
                    string:substr(B, 1, MaxSize);
                false -> B
            end,
            MaxLen = case Options#print_options.force_strings of
                true ->
                    Max;
                false ->
                    %% make room for the leading doublequote
                    Max - 1
            end,
            try alist(In, MaxLen, Options) of
                {L0, Len0} ->
                    case Options#print_options.force_strings of
                        false ->
                            case B /= In of
                                true ->
                                    {[$", L0, "..."], Len0+4};
                                false ->
                                    {[$"|L0], Len0+1}
                            end;
                        true ->
                            {L0, Len0}
                    end
            catch
                throw:{unprintable, C} ->
                    Index = string:chr(In, C),
                    case Index > 1 andalso Options#print_options.depth =< Index andalso
                        Options#print_options.depth > -1 andalso
                          not Options#print_options.force_strings of
                        true ->
                            %% print first Index-1 characters followed by ...
                            {L0, Len0} = alist_start(string:substr(In, 1, Index - 1), Max - 1, Options),
                            {L0++"...", Len0+3};
                        false ->
                            list_body(In, Max-4, dec_depth(Options), binary)
                    end
            end;
        _ ->
            list_body(B, Max-4, dec_depth(Options), binary)
    end,
    case Options#print_options.force_strings of
        true ->
            {Res, Length};
        _ ->
            {["<<", Res, ">>"], Length+4}
    end;

%% bitstrings are binary's evil brother who doesn't end on an 8 bit boundary.
%% This makes printing them extremely annoying, so list_body/list_bodyc has
%% some magic for dealing with the output of bitstring_to_list, which returns
%% a list of integers (as expected) but with a trailing binary that represents
%% the remaining bits.
print({inline_bitstring, B}, _Max, _Options) when is_bitstring(B) ->
    Size = bit_size(B),
    <<Value:Size>> = B,
    ValueStr = integer_to_list(Value),
    SizeStr = integer_to_list(Size),
    {[ValueStr, $:, SizeStr], length(ValueStr) + length(SizeStr) +1};
print(BitString, Max, Options) when is_bitstring(BitString) ->
    case byte_size(BitString) > Max of
        true ->
            BL = binary_to_list(BitString, 1, Max);
        _ ->
            R = erlang:bitstring_to_list(BitString),
            {Bytes, [Bits]} = lists:splitwith(fun erlang:is_integer/1, R),
            %% tag the trailing bits with a special tuple we catch when
            %% list_body calls print again
            BL = Bytes ++ [{inline_bitstring, Bits}]
    end,
    {X, Len0} = list_body(BL, Max - 4, dec_depth(Options), bitstring),
    {["<<", X, ">>"], Len0 + 4};

print(Float, _Max, _Options) when is_float(Float) ->
    %% use the same function io_lib:format uses to print floats
    %% float_to_list is way too verbose.
    L = io_lib_format:fwrite_g(Float),
    {L, length(L)};

print(Fun, Max, _Options) when is_function(Fun) ->
    L = erlang:fun_to_list(Fun),
    case length(L) > Max of
        true ->
            S = erlang:max(5, Max),
            Res = string:substr(L, 1, S) ++ "..>",
            {Res, length(Res)};
        _ ->
            {L, length(L)}
    end;

print(Integer, _Max, _Options) when is_integer(Integer) ->
    L = integer_to_list(Integer),
    {L, length(L)};

print(Pid, _Max, _Options) when is_pid(Pid) ->
    L = pid_to_list(Pid),
    {L, length(L)};

print(Ref, _Max, _Options) when is_reference(Ref) ->
    L = erlang:ref_to_list(Ref),
    {L, length(L)};

print(Port, _Max, _Options) when is_port(Port) ->
    L = erlang:port_to_list(Port),
    {L, length(L)};

print({'$lager_record', Name, Fields}, Max, Options) ->
    Leader = "#" ++ atom_to_list(Name) ++ "{",
    {RC, Len} = record_fields(Fields, Max - length(Leader) + 1, dec_depth(Options)),
    {[Leader, RC, "}"], Len + length(Leader) + 1};

print(Tuple, Max, Options) when is_tuple(Tuple) ->
    {TC, Len} = tuple_contents(Tuple, Max-2, Options),
    {[${, TC, $}], Len + 2};

print(Map, Max, Options) when is_map(Map) ->
    {MC, Len} = map_contents(Map, Max - 3, Options),
    {["#{", MC, "}"], Len + 3};

print(List, Max, Options) when is_list(List) ->
    case Options#print_options.lists_as_strings orelse
        Options#print_options.force_strings of
        true ->
            alist_start(List, Max, dec_depth(Options));
        _ ->
            {R, Len} = list_body(List, Max - 2, dec_depth(Options), list),
            {[$[, R, $]], Len + 2}
    end.

%% Returns {List, Length}
map_contents(Map, Max, Options) ->
    list_body(maps:to_list(Map), Max, dec_depth(Options), map).

tuple_contents(Tuple, Max, Options) ->
    list_body(tuple_to_list(Tuple), Max, dec_depth(Options), tuple).

%% Format the inside of a list, i.e. do not add a leading [ or trailing ].
%% Returns {List, Length}
list_body([], _Max, _Options, _) ->
    {[], 0};
list_body(_, Max, _Options, _) when Max < 4 ->
    {"...", 3};
list_body(_, _Max, #print_options{depth=0}, _) ->
    {"...", 3};
list_body([H], Max, Options=#print_options{depth=1}, _) ->
    print(H, Max, Options);
list_body([H|_], Max, Options=#print_options{depth=1}, Type) ->
    {List, Len} = print(H, Max-4, Options),
    Sep = case Type of
              list -> $|;
              _ -> $,
          end,
    {[List ++ [Sep | "..."]], Len + 4};
list_body([{K, V}|T], Max, Options, map) ->
    {L1, N1} = print(K, Max - 4, Options),
    {L2, N2} = print(V, Max - 4 - N1, Options),
    {Final, FLen} = list_bodyc(T, Max - N1 - 4 - N2, Options, map),
    {[L1, " => ", L2|Final], N1 + 4 + N2 + FLen};
list_body([H|T], Max, Options, Type) ->
    {List, Len} = print(H, Max, Options),
    {Final, FLen} = list_bodyc(T, Max - Len, Options, Type),
    {[List|Final], FLen + Len};
list_body(X, Max, Options, _) ->  %% improper list
    {List, Len} = print(X, Max - 1, Options),
    {[$|,List], Len + 1}.

list_bodyc([], _Max, _Options, _) ->
    {[], 0};
list_bodyc(_, Max, _Options, _) when Max < 5 ->
    {",...", 4};
list_bodyc(_, _Max, #print_options{depth=1}, list) ->
    {"|...", 4};
list_bodyc(_, _Max, #print_options{depth=1}, _) ->
    {",...", 4};
list_bodyc([{K, V}|T], Max, Options, map) ->
    Options1 = dec_depth(Options),
    {L1, N1} = print(K, Max - 4 - 1, Options1),
    {L2, N2} = print(V, Max - 4 - N1 - 1, Options1),
    {Final, FLen} = list_bodyc(T, Max - N1 - 4 - N2 - 1, Options1, map),
    {[$,, L1, " => ", L2|Final], N1 + 4 + N2 + FLen + 1};
list_bodyc([H|T], Max, Options, Type) ->
    {List, Len} = print(H, Max, dec_depth(Options)),
    {Final, FLen} = list_bodyc(T, Max - Len - 1, dec_depth(Options), Type),
    {[$,, List|Final], FLen + Len + 1};
list_bodyc(X, Max, Options, _) ->  %% improper list
    {List, Len} = print(X, Max - 1, Options),
    {[$|,List], Len + 1}.

%% The head of a list we hope is ascii. Examples:
%%
%% [65,66,67] -> "ABC"
%% [65,0,67] -> "A"[0,67]
%% [0,65,66] -> [0,65,66]
%% [65,b,66] -> "A"[b,66]
%%
alist_start([], _Max, #print_options{force_strings=true}) -> {"", 0};
alist_start([], _Max, _Options) -> {"[]", 2};
alist_start(_, Max, _Options) when Max < 4 -> {"...", 3};
alist_start(_, _Max, #print_options{depth=0}) -> {"[...]", 5};
alist_start(L, Max, #print_options{force_strings=true} = Options) ->
    alist(L, Max, Options);
%alist_start([H|_T], _Max, #print_options{depth=1}) when is_integer(H) -> {[$[, H, $|, $., $., $., $]], 7};
alist_start([H|T], Max, Options) when is_integer(H), H >= 16#20, H =< 16#7e ->  % definitely printable
    try alist([H|T], Max -1, Options) of
        {L, Len} ->
            {[$"|L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H|T], Max-2, Options, list),
            {[$[, R, $]], Len + 2}
    end;
alist_start([H|T], Max, Options) when is_integer(H), H >= 16#a0, H =< 16#ff ->  % definitely printable
    try alist([H|T], Max -1, Options) of
        {L, Len} ->
            {[$"|L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H|T], Max-2, Options, list),
            {[$[, R, $]], Len + 2}
    end;
alist_start([H|T], Max, Options) when H =:= $\t; H =:= $\n; H =:= $\r; H =:= $\v; H =:= $\e; H=:= $\f; H=:= $\b ->
    try alist([H|T], Max -1, Options) of
        {L, Len} ->
            {[$"|L], Len + 1}
    catch
        throw:{unprintable, _} ->
            {R, Len} = list_body([H|T], Max-2, Options, list),
            {[$[, R, $]], Len + 2}
    end;
alist_start(L, Max, Options) ->
    {R, Len} = list_body(L, Max-2, Options, list),
    {[$[, R, $]], Len + 2}.

alist([], _Max, #print_options{force_strings=true}) -> {"", 0};
alist([], _Max, _Options) -> {"\"", 1};
alist(_, Max, #print_options{force_strings=true}) when Max < 4 -> {"...", 3};
alist(_, Max, #print_options{force_strings=false}) when Max < 5 -> {"...\"", 4};
alist([H|T], Max, Options = #print_options{force_strings=false,lists_as_strings=true}) when H =:= $"; H =:= $\\ ->
    %% preserve escaping around quotes
    {L, Len} = alist(T, Max-1, Options),
    {[$\\,H|L], Len + 2};
alist([H|T], Max, Options) when is_integer(H), H >= 16#20, H =< 16#7e ->     % definitely printable
    {L, Len} = alist(T, Max-1, Options),
    {[H|L], Len + 1};
alist([H|T], Max, Options) when is_integer(H), H >= 16#a0, H =< 16#ff ->     % definitely printable
    {L, Len} = alist(T, Max-1, Options),
    {[H|L], Len + 1};
alist([H|T], Max, Options) when H =:= $\t; H =:= $\n; H =:= $\r; H =:= $\v; H =:= $\e; H=:= $\f; H=:= $\b ->
    {L, Len} = alist(T, Max-1, Options),
    case Options#print_options.force_strings of
        true ->
            {[H|L], Len + 1};
        _ ->
            {[escape(H)|L], Len + 1}
    end;
alist([H|T], Max, #print_options{force_strings=true} = Options) when is_integer(H) ->
    {L, Len} = alist(T, Max-1, Options),
    {[H|L], Len + 1};
alist([H|T], Max, Options = #print_options{force_strings=true}) when is_binary(H); is_list(H) ->
    {List, Len} = print(H, Max, Options),
    case (Max - Len) =< 0 of
        true ->
            %% no more room to print anything
            {List, Len};
        false ->
            %% no need to decrement depth, as we're in printable string mode
            {Final, FLen} = alist(T, Max - Len, Options),
            {[List|Final], FLen+Len}
    end;
alist(_, _, #print_options{force_strings=true}) ->
    erlang:error(badarg);
alist([H|_L], _Max, _Options) ->
    throw({unprintable, H});
alist(H, _Max, _Options) ->
    %% improper list
    throw({unprintable, H}).

%% is the first character in the atom alphabetic & lowercase?
atom_needs_quoting_start([H|T]) when H >= $a, H =< $z ->
    atom_needs_quoting(T);
atom_needs_quoting_start(_) ->
    true.

atom_needs_quoting([]) ->
    false;
atom_needs_quoting([H|T]) when (H >= $a andalso H =< $z);
                        (H >= $A andalso H =< $Z);
                        (H >= $0 andalso H =< $9);
                         H == $@; H == $_ ->
    atom_needs_quoting(T);
atom_needs_quoting(_) ->
    true.

-spec prepare_options(options(), #print_options{}) -> #print_options{}.
prepare_options([], Options) ->
    Options;
prepare_options([{depth, Depth}|T], Options) when is_integer(Depth) ->
    prepare_options(T, Options#print_options{depth=Depth});
prepare_options([{lists_as_strings, Bool}|T], Options) when is_boolean(Bool) ->
    prepare_options(T, Options#print_options{lists_as_strings = Bool});
prepare_options([{force_strings, Bool}|T], Options) when is_boolean(Bool) ->
    prepare_options(T, Options#print_options{force_strings = Bool}).

dec_depth(#print_options{depth=Depth} = Options) when Depth > 0 ->
    Options#print_options{depth=Depth-1};
dec_depth(Options) ->
    Options.

escape($\t) -> "\\t";
escape($\n) -> "\\n";
escape($\r) -> "\\r";
escape($\e) -> "\\e";
escape($\f) -> "\\f";
escape($\b) -> "\\b";
escape($\v) -> "\\v".

record_fields([], _, _) ->
    {"", 0};
record_fields(_, Max, #print_options{depth=D}) when Max < 4; D == 0 ->
    {"...", 3};
record_fields([{Field, Value}|T], Max, Options) ->
    {ExtraChars, Terminator} = case T of
        [] ->
            {1, []};
        _ ->
            {2, ","}
    end,
    {FieldStr, FieldLen} = print(Field, Max - ExtraChars, Options),
    {ValueStr, ValueLen} = print(Value, Max - (FieldLen + ExtraChars), Options),
    {Final, FLen} = record_fields(T, Max - (FieldLen + ValueLen + ExtraChars), dec_depth(Options)),
    {[FieldStr++"="++ValueStr++Terminator|Final], FLen + FieldLen + ValueLen + ExtraChars}.


-ifdef(TEST).
%%--------------------
%% The start of a test suite. So far, it only checks for not crashing.
-spec test() -> ok.
test() ->
    test(trunc_io, print).

-spec test(atom(), atom()) -> ok.
test(Mod, Func) ->
    Simple_items = [atom, 1234, 1234.0, {tuple}, [], [list], "string", self(),
        <<1,2,3>>, make_ref(), fun() -> ok end],
    F = fun(A) ->
            Mod:Func(A, 100),
            Mod:Func(A, 2),
            Mod:Func(A, 20)
    end,

    G = fun(A) ->
            case catch F(A) of
                {'EXIT', _} -> exit({failed, A});
                _ -> ok
            end
    end,

    lists:foreach(G, Simple_items),

    Tuples = [ {1,2,3,a,b,c}, {"abc", def, 1234},
        {{{{a},b,c,{d},e}},f}],

    Lists = [ [1,2,3,4,5,6,7], lists:seq(1,1000),
        [{a}, {a,b}, {a, [b,c]}, "def"], [a|b], [$a|$b] ],


    lists:foreach(G, Tuples),
    lists:foreach(G, Lists).

-spec perf() -> ok.
perf() ->
    {New, _} = timer:tc(trunc_io, perf, [trunc_io, print, 1000]),
    {Old, _} = timer:tc(trunc_io, perf, [io_lib, write, 1000]),
    io:fwrite("New code took ~p us, old code ~p\n", [New, Old]).

-spec perf(atom(), atom(), integer()) -> done.
perf(M, F, Reps) when Reps > 0 ->
    test(M,F),
    perf(M,F,Reps-1);
perf(_,_,_) ->
    done.

%% Performance test. Needs a particularly large term I saved as a binary...
-spec perf1() -> {non_neg_integer(), non_neg_integer()}.
perf1() ->
    {ok, Bin} = file:read_file("bin"),
    A = binary_to_term(Bin),
    {N, _} = timer:tc(trunc_io, print, [A, 1500]),
    {M, _} = timer:tc(io_lib, write, [A]),
    {N, M}.

format_test() ->
    %% simple format strings
    ?assertEqual("foobar", lists:flatten(format("~s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~P", [["foo", $b, $a, $r], 10], 50))),
    ?assertEqual("[[102,111,111],98,97,114]", lists:flatten(format("~w", [["foo", $b, $a, $r]], 50))),

    %% complex ones
    ?assertEqual("    foobar", lists:flatten(format("~10s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("f", lists:flatten(format("~1s", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~22p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~22P", [["foo", $b, $a, $r], 10], 50))),
    ?assertEqual("**********", lists:flatten(format("~10W", [["foo", $b, $a, $r], 10], 50))),
    ?assertEqual("[[102,111,111],98,97,114]", lists:flatten(format("~25W", [["foo", $b, $a, $r], 10], 50))),
    % Note these next two diverge from io_lib:format; the field width is
    % ignored, when it should be used as max line length.
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~10p", [["foo", $b, $a, $r]], 50))),
    ?assertEqual("[\"foo\",98,97,114]", lists:flatten(format("~10P", [["foo", $b, $a, $r], 10], 50))),
    ok.

atom_quoting_test() ->
    ?assertEqual("hello", lists:flatten(format("~p", [hello], 50))),
    ?assertEqual("'hello world'", lists:flatten(format("~p", ['hello world'], 50))),
    ?assertEqual("'Hello world'", lists:flatten(format("~p", ['Hello world'], 50))),
    ?assertEqual("hello_world", lists:flatten(format("~p", ['hello_world'], 50))),
    ?assertEqual("'node@127.0.0.1'", lists:flatten(format("~p", ['node@127.0.0.1'], 50))),
    ?assertEqual("node@nohost", lists:flatten(format("~p", [node@nohost], 50))),
    ?assertEqual("abc123", lists:flatten(format("~p", [abc123], 50))),
    ok.

sane_float_printing_test() ->
    ?assertEqual("1.0", lists:flatten(format("~p", [1.0], 50))),
    ?assertEqual("1.23456789", lists:flatten(format("~p", [1.23456789], 50))),
    ?assertEqual("1.23456789", lists:flatten(format("~p", [1.234567890], 50))),
    ?assertEqual("0.3333333333333333", lists:flatten(format("~p", [1/3], 50))),
    ?assertEqual("0.1234567", lists:flatten(format("~p", [0.1234567], 50))),
    ok.

float_inside_list_test() ->
    ?assertEqual("[97,38.233913133184835,99]", lists:flatten(format("~p", [[$a, 38.233913133184835, $c]], 50))),
    ?assertError(badarg, lists:flatten(format("~s", [[$a, 38.233913133184835, $c]], 50))),
    ok.

quote_strip_test() ->
    ?assertEqual("\"hello\"", lists:flatten(format("~p", ["hello"], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", ["hello"], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", [hello], 50))),
    ?assertEqual("hello", lists:flatten(format("~p", [hello], 50))),
    ?assertEqual("'hello world'", lists:flatten(format("~p", ['hello world'], 50))),
    ?assertEqual("hello world", lists:flatten(format("~s", ['hello world'], 50))),
    ok.

binary_printing_test() ->
    ?assertEqual("<<>>", lists:flatten(format("~p", [<<>>], 50))),
    ?assertEqual("", lists:flatten(format("~s", [<<>>], 50))),
    ?assertEqual("<<..>>", lists:flatten(format("~p", [<<"hi">>], 0))),
    ?assertEqual("<<...>>", lists:flatten(format("~p", [<<"hi">>], 1))),
    ?assertEqual("<<\"hello\">>", lists:flatten(format("~p", [<<$h, $e, $l, $l, $o>>], 50))),
    ?assertEqual("<<\"hello\">>", lists:flatten(format("~p", [<<"hello">>], 50))),
    ?assertEqual("<<104,101,108,108,111>>", lists:flatten(format("~w", [<<"hello">>], 50))),
    ?assertEqual("<<1,2,3,4>>", lists:flatten(format("~p", [<<1, 2, 3, 4>>], 50))),
    ?assertEqual([1,2,3,4], lists:flatten(format("~s", [<<1, 2, 3, 4>>], 50))),
    ?assertEqual("hello", lists:flatten(format("~s", [<<"hello">>], 50))),
    ?assertEqual("hello\nworld", lists:flatten(format("~s", [<<"hello\nworld">>], 50))),
    ?assertEqual("<<\"hello\\nworld\">>", lists:flatten(format("~p", [<<"hello\nworld">>], 50))),
    ?assertEqual("<<\"\\\"hello world\\\"\">>", lists:flatten(format("~p", [<<"\"hello world\"">>], 50))),
    ?assertEqual("<<\"hello\\\\world\">>", lists:flatten(format("~p", [<<"hello\\world">>], 50))),
    ?assertEqual("<<\"hello\\\\\world\">>", lists:flatten(format("~p", [<<"hello\\\world">>], 50))),
    ?assertEqual("<<\"hello\\\\\\\\world\">>", lists:flatten(format("~p", [<<"hello\\\\world">>], 50))),
    ?assertEqual("<<\"hello\\bworld\">>", lists:flatten(format("~p", [<<"hello\bworld">>], 50))),
    ?assertEqual("<<\"hello\\tworld\">>", lists:flatten(format("~p", [<<"hello\tworld">>], 50))),
    ?assertEqual("<<\"hello\\nworld\">>", lists:flatten(format("~p", [<<"hello\nworld">>], 50))),
    ?assertEqual("<<\"hello\\rworld\">>", lists:flatten(format("~p", [<<"hello\rworld">>], 50))),
    ?assertEqual("<<\"hello\\eworld\">>", lists:flatten(format("~p", [<<"hello\eworld">>], 50))),
    ?assertEqual("<<\"hello\\fworld\">>", lists:flatten(format("~p", [<<"hello\fworld">>], 50))),
    ?assertEqual("<<\"hello\\vworld\">>", lists:flatten(format("~p", [<<"hello\vworld">>], 50))),
    ?assertEqual("     hello", lists:flatten(format("~10s", [<<"hello">>], 50))),
    ?assertEqual("[a]", lists:flatten(format("~s", [<<"[a]">>], 50))),
    ?assertEqual("[a]", lists:flatten(format("~s", [[<<"[a]">>]], 50))),

    ok.

bitstring_printing_test() ->
    ?assertEqual("<<1,2,3,1:7>>", lists:flatten(format("~p",
                [<<1, 2, 3, 1:7>>], 100))),
    ?assertEqual("<<1:7>>", lists:flatten(format("~p",
                [<<1:7>>], 100))),
    ?assertEqual("<<1,2,3,...>>", lists:flatten(format("~p",
                [<<1, 2, 3, 1:7>>], 12))),
    ?assertEqual("<<1,2,3,...>>", lists:flatten(format("~p",
                [<<1, 2, 3, 1:7>>], 13))),
    ?assertEqual("<<1,2,3,1:7>>", lists:flatten(format("~p",
                [<<1, 2, 3, 1:7>>], 14))),
    ?assertEqual("<<..>>", lists:flatten(format("~p", [<<1:7>>], 0))),
    ?assertEqual("<<...>>", lists:flatten(format("~p", [<<1:7>>], 1))),
    ?assertEqual("[<<1>>,<<2>>]", lists:flatten(format("~p", [[<<1>>, <<2>>]],
                100))),
    ?assertEqual("{<<1:7>>}", lists:flatten(format("~p", [{<<1:7>>}], 50))),
    ok.

list_printing_test() ->
    ?assertEqual("[]", lists:flatten(format("~p", [[]], 50))),
    ?assertEqual("[]", lists:flatten(format("~w", [[]], 50))),
    ?assertEqual("", lists:flatten(format("~s", [[]], 50))),
    ?assertEqual("...", lists:flatten(format("~s", [[]], -1))),
    ?assertEqual("[[]]", lists:flatten(format("~p", [[[]]], 50))),
    ?assertEqual("[13,11,10,8,5,4]", lists:flatten(format("~p", [[13,11,10,8,5,4]], 50))),
    ?assertEqual("\"\\rabc\"", lists:flatten(format("~p", [[13,$a, $b, $c]], 50))),
    ?assertEqual("[1,2,3|4]", lists:flatten(format("~p", [[1, 2, 3|4]], 50))),
    ?assertEqual("[...]", lists:flatten(format("~p", [[1, 2, 3,4]], 4))),
    ?assertEqual("[1,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 6))),
    ?assertEqual("[1,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 7))),
    ?assertEqual("[1,2,...]", lists:flatten(format("~p", [[1, 2, 3, 4]], 8))),
    ?assertEqual("[1|4]", lists:flatten(format("~p", [[1|4]], 50))),
    ?assertEqual("[1]", lists:flatten(format("~p", [[1]], 50))),
    ?assertError(badarg, lists:flatten(format("~s", [[1|4]], 50))),
    ?assertEqual("\"hello...\"", lists:flatten(format("~p", ["hello world"], 10))),
    ?assertEqual("hello w...", lists:flatten(format("~s", ["hello world"], 10))),
    ?assertEqual("hello world\r\n", lists:flatten(format("~s", ["hello world\r\n"], 50))),
    ?assertEqual("\rhello world\r\n", lists:flatten(format("~s", ["\rhello world\r\n"], 50))),
    ?assertEqual("\"\\rhello world\\r\\n\"", lists:flatten(format("~p", ["\rhello world\r\n"], 50))),
    ?assertEqual("[13,104,101,108,108,111,32,119,111,114,108,100,13,10]", lists:flatten(format("~w", ["\rhello world\r\n"], 60))),
    ?assertEqual("...", lists:flatten(format("~s", ["\rhello world\r\n"], 3))),
    ?assertEqual("[22835963083295358096932575511191922182123945984,...]",
        lists:flatten(format("~p", [
                    [22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984]], 9))),
    ?assertEqual("[22835963083295358096932575511191922182123945984,...]",
        lists:flatten(format("~p", [
                    [22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984]], 53))),
    %%improper list
    ?assertEqual("[1,2,3|4]", lists:flatten(format("~P", [[1|[2|[3|4]]], 5], 50))),
    ?assertEqual("[1|1]", lists:flatten(format("~P", [[1|1], 5], 50))),
    ?assertEqual("[9|9]", lists:flatten(format("~p", [[9|9]], 50))),
    ok.

iolist_printing_test() ->
    ?assertEqual("iolist: HelloIamaniolist",
        lists:flatten(format("iolist: ~s", [[$H, $e,  $l, $l, $o, "I", ["am", [<<"an">>], [$i, $o, $l, $i, $s, $t]]]], 1000))),
    ?assertEqual("123...",
                 lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 6))),
    ?assertEqual("123456...",
                 lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 9))),
    ?assertEqual("123456789H...",
                 lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 13))),
    ?assertEqual("123456789HellIamaniolist",
                 lists:flatten(format("~s", [[<<"123456789">>, "HellIamaniolist"]], 30))),

    ok.

tuple_printing_test() ->
    ?assertEqual("{}", lists:flatten(format("~p", [{}], 50))),
    ?assertEqual("{}", lists:flatten(format("~w", [{}], 50))),
    ?assertError(badarg, lists:flatten(format("~s", [{}], 50))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 1))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 2))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 3))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 4))),
    ?assertEqual("{...}", lists:flatten(format("~p", [{foo}], 5))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo,bar}], 6))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo,bar}], 7))),
    ?assertEqual("{foo,...}", lists:flatten(format("~p", [{foo,bar}], 9))),
    ?assertEqual("{foo,bar}", lists:flatten(format("~p", [{foo,bar}], 10))),
    ?assertEqual("{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(format("~w", [
                    {22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984}], 10))),
    ?assertEqual("{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(format("~w", [
                    {22835963083295358096932575511191922182123945984,
                        bar}], 10))),
    ?assertEqual("{22835963083295358096932575511191922182123945984,...}",
        lists:flatten(format("~w", [
                    {22835963083295358096932575511191922182123945984,
                        22835963083295358096932575511191922182123945984}], 53))),
    ok.

unicode_test() ->
    ?assertEqual([231,167,129], lists:flatten(format("~s", [<<231,167,129>>], 50))),
    ?assertEqual([31169], lists:flatten(format("~ts", [<<231,167,129>>], 50))),
    ok.

depth_limit_test() ->
    ?assertEqual("{...}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 1], 50))),
    ?assertEqual("{a,...}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 2], 50))),
    ?assertEqual("{a,[...]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 3], 50))),
    ?assertEqual("{a,[b|...]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 4], 50))),
    ?assertEqual("{a,[b,[...]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 5], 50))),
    ?assertEqual("{a,[b,[c|...]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 6], 50))),
    ?assertEqual("{a,[b,[c,[...]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 7], 50))),
    ?assertEqual("{a,[b,[c,[d]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 8], 50))),
    ?assertEqual("{a,[b,[c,[d]]]}", lists:flatten(format("~P", [{a, [b, [c, [d]]]}, 9], 50))),

    ?assertEqual("{a,{...}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 3], 50))),
    ?assertEqual("{a,{b,...}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 4], 50))),
    ?assertEqual("{a,{b,{...}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 5], 50))),
    ?assertEqual("{a,{b,{c,...}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 6], 50))),
    ?assertEqual("{a,{b,{c,{...}}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 7], 50))),
    ?assertEqual("{a,{b,{c,{d}}}}", lists:flatten(format("~P", [{a, {b, {c, {d}}}}, 8], 50))),

    ?assertEqual("{\"a\",[...]}", lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 3], 50))),
    ?assertEqual("{\"a\",[\"b\",[[...]|...]]}", lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 6], 50))),
    ?assertEqual("{\"a\",[\"b\",[\"c\",[\"d\"]]]}", lists:flatten(format("~P", [{"a", ["b", ["c", ["d"]]]}, 9], 50))),

    ?assertEqual("[...]", lists:flatten(format("~P", [[1, 2, 3], 1], 50))),
    ?assertEqual("[1|...]", lists:flatten(format("~P", [[1, 2, 3], 2], 50))),
    ?assertEqual("[1,2|...]", lists:flatten(format("~P", [[1, 2, 3], 3], 50))),
    ?assertEqual("[1,2,3]", lists:flatten(format("~P", [[1, 2, 3], 4], 50))),

    ?assertEqual("{1,...}", lists:flatten(format("~P", [{1, 2, 3}, 2], 50))),
    ?assertEqual("{1,2,...}", lists:flatten(format("~P", [{1, 2, 3}, 3], 50))),
    ?assertEqual("{1,2,3}", lists:flatten(format("~P", [{1, 2, 3}, 4], 50))),

    ?assertEqual("{1,...}", lists:flatten(format("~P", [{1, 2, 3}, 2], 50))),
    ?assertEqual("[1,2|...]", lists:flatten(format("~P", [[1, 2, <<3>>], 3], 50))),
    ?assertEqual("[1,2,<<...>>]", lists:flatten(format("~P", [[1, 2, <<3>>], 4], 50))),
    ?assertEqual("[1,2,<<3>>]", lists:flatten(format("~P", [[1, 2, <<3>>], 5], 50))),

    ?assertEqual("<<...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 1], 50))),
    ?assertEqual("<<0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 2], 50))),
    ?assertEqual("<<0,0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 3], 50))),
    ?assertEqual("<<0,0,0,...>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 4], 50))),
    ?assertEqual("<<0,0,0,0>>", lists:flatten(format("~P", [<<0, 0, 0, 0>>, 5], 50))),

    %% this is a seriously weird edge case
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 2], 50))),
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 3], 50))),
    ?assertEqual("<<\"   \"...>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 4], 50))),
    ?assertEqual("<<32,32,32,0>>", lists:flatten(format("~P", [<<32, 32, 32, 0>>, 5], 50))),
    ?assertEqual("<<32,32,32,0>>", lists:flatten(format("~p", [<<32, 32, 32, 0>>], 50))),

    %% depth limiting for some reason works in 4 byte chunks on printable binaries?
    ?assertEqual("<<\"hell\"...>>", lists:flatten(format("~P", [<<"hello world">>, 2], 50))),
    ?assertEqual("<<\"abcd\"...>>", lists:flatten(format("~P", [<<$a, $b, $c, $d, $e, 0>>, 2], 50))),

    %% I don't even know...
    ?assertEqual("<<>>", lists:flatten(format("~P", [<<>>, 1], 50))),
    ?assertEqual("<<...>>", lists:flatten(format("~W", [<<>>, 1], 50))),

    ?assertEqual("{abc,<<\"abc\\\"\">>}", lists:flatten(format("~P", [{abc,<<"abc\"">>}, 4], 50))),

    ok.

print_terms_without_format_string_test() ->
    ?assertError(badarg, format({hello, world}, [], 50)),
    ?assertError(badarg, format([{google, bomb}], [], 50)),
    ?assertError(badarg, format([$h,$e,$l,$l,$o, 3594], [], 50)),
    ?assertEqual("helloworld", lists:flatten(format([$h,$e,$l,$l,$o, "world"], [], 50))),
    ?assertEqual("hello", lists:flatten(format(<<"hello">>, [], 50))),
    ?assertEqual("hello", lists:flatten(format('hello', [], 50))),
    ?assertError(badarg, format(<<1, 2, 3, 1:7>>, [], 100)),
    ?assertError(badarg, format(65535, [], 50)),
    ok.

-endif.


%%
%% lager_format part
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-record(options, {
        chomp = false
    }).

format_(FmtStr, Args, MaxLen) ->
    format_(FmtStr, Args, MaxLen, []).

format_([], [], _, _) ->
    "";
format_(FmtStr, Args, MaxLen, Opts) when is_atom(FmtStr) ->
    format_(atom_to_list(FmtStr), Args, MaxLen, Opts);
format_(FmtStr, Args, MaxLen, Opts) when is_binary(FmtStr) ->
    format_(binary_to_list(FmtStr), Args, MaxLen, Opts);
format_(FmtStr, Args, MaxLen, Opts) when is_list(FmtStr) ->
    case string_p(FmtStr) of
        true ->
            Options = make_options(Opts, #options{}),
            Cs = collect(FmtStr, Args),
            {Cs2, MaxLen2} = build(Cs, [], MaxLen, Options),
            %% count how many terms remain
            {Count, StrLen} = lists:foldl(
                fun({_C, _As, _F, _Adj, _P, _Pad, _Enc}, {Terms, Chars}) ->
                        {Terms + 1, Chars};
                    (_, {Terms, Chars}) ->
                        {Terms, Chars + 1}
                end, {0, 0}, Cs2),
            build2(Cs2, Count, MaxLen2 - StrLen);
        false ->
            erlang:error(badarg)
    end;
format_(_FmtStr, _Args, _MaxLen, _Opts) ->
    erlang:error(badarg).

collect([$~|Fmt0], Args0) ->
    {C,Fmt1,Args1} = collect_cseq(Fmt0, Args0),
    [C|collect(Fmt1, Args1)];
collect([C|Fmt], Args) ->
    [C|collect(Fmt, Args)];
collect([], []) -> [].

collect_cseq(Fmt0, Args0) ->
    {F,Ad,Fmt1,Args1} = field_width(Fmt0, Args0),
    {P,Fmt2,Args2} = precision(Fmt1, Args1),
    {Pad,Fmt3,Args3} = pad_char(Fmt2, Args2),
    {Encoding,Fmt4,Args4} = encoding(Fmt3, Args3),
    {C,As,Fmt5,Args5} = collect_cc(Fmt4, Args4),
    {{C,As,F,Ad,P,Pad,Encoding},Fmt5,Args5}.

encoding([$t|Fmt],Args) ->
    {unicode,Fmt,Args};
encoding(Fmt,Args) ->
    {latin1,Fmt,Args}.

field_width([$-|Fmt0], Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F,left,Fmt,Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F,right,Fmt,Args}.

precision([$.|Fmt], Args) ->
    field_value(Fmt, Args);
precision(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([$*|Fmt], [A|Args]) when is_integer(A) ->
    {A,Fmt,Args};
field_value([C|Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([C|Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10*F + (C - $0));
field_value(Fmt, Args, F) -> %Default case
    {F,Fmt,Args}.

pad_char([$.,$*|Fmt], [Pad|Args]) -> {Pad,Fmt,Args};
pad_char([$.,Pad|Fmt], Args) -> {Pad,Fmt,Args};
pad_char(Fmt, Args) -> {$\s,Fmt,Args}.

%% collect_cc([FormatChar], [Argument]) ->
%%         {Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w|Fmt], [A|Args]) -> {$w,[A],Fmt,Args};
collect_cc([$p|Fmt], [A|Args]) -> {$p,[A],Fmt,Args};
collect_cc([$W|Fmt], [A,Depth|Args]) -> {$W,[A,Depth],Fmt,Args};
collect_cc([$P|Fmt], [A,Depth|Args]) -> {$P,[A,Depth],Fmt,Args};
collect_cc([$s|Fmt], [A|Args]) -> {$s,[A],Fmt,Args};
collect_cc([$e|Fmt], [A|Args]) -> {$e,[A],Fmt,Args};
collect_cc([$f|Fmt], [A|Args]) -> {$f,[A],Fmt,Args};
collect_cc([$g|Fmt], [A|Args]) -> {$g,[A],Fmt,Args};
collect_cc([$b|Fmt], [A|Args]) -> {$b,[A],Fmt,Args};
collect_cc([$B|Fmt], [A|Args]) -> {$B,[A],Fmt,Args};
collect_cc([$x|Fmt], [A,Prefix|Args]) -> {$x,[A,Prefix],Fmt,Args};
collect_cc([$X|Fmt], [A,Prefix|Args]) -> {$X,[A,Prefix],Fmt,Args};
collect_cc([$+|Fmt], [A|Args]) -> {$+,[A],Fmt,Args};
collect_cc([$#|Fmt], [A|Args]) -> {$#,[A],Fmt,Args};
collect_cc([$c|Fmt], [A|Args]) -> {$c,[A],Fmt,Args};
collect_cc([$~|Fmt], Args) when is_list(Args) -> {$~,[],Fmt,Args};
collect_cc([$n|Fmt], Args) when is_list(Args) -> {$n,[],Fmt,Args};
collect_cc([$i|Fmt], [A|Args]) -> {$i,[A],Fmt,Args}.


%% build([Control], Pc, Indentation) -> [Char].
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build([{$n, _, _, _, _, _, _}], Acc, MaxLen, #options{chomp=true}) ->
    %% trailing ~n, ignore
    {lists:reverse(Acc), MaxLen};
build([{C,As,F,Ad,P,Pad,Enc}|Cs], Acc, MaxLen, O) ->
    {S, MaxLen2} = control(C, As, F, Ad, P, Pad, Enc, MaxLen),
    build(Cs, [S|Acc], MaxLen2, O);
build([$\n], Acc, MaxLen, #options{chomp=true}) ->
    %% trailing \n, ignore
    {lists:reverse(Acc), MaxLen};
build([$\n|Cs], Acc, MaxLen, O) ->
    build(Cs, [$\n|Acc], MaxLen - 1, O);
build([$\t|Cs], Acc, MaxLen, O) ->
    build(Cs, [$\t|Acc], MaxLen - 1, O);
build([C|Cs], Acc, MaxLen, O) ->
    build(Cs, [C|Acc], MaxLen - 1, O);
build([], Acc, MaxLen, _O) ->
    {lists:reverse(Acc), MaxLen}.

build2([{C,As,F,Ad,P,Pad,Enc}|Cs], Count, MaxLen) ->
    {S, Len} = control2(C, As, F, Ad, P, Pad, Enc, MaxLen div Count),
    [S|build2(Cs, Count - 1, MaxLen - Len)];
build2([C|Cs], Count, MaxLen) ->
    [C|build2(Cs, Count, MaxLen)];
build2([], _, _) -> [].

%% control(FormatChar, [Argument], FieldWidth, Adjust, Precision, PadChar,
%%         Indentation) -> [Char]
%%  This is the main dispatch function for the various formatting commands.
%%  Field widths and precisions have already been calculated.

control($e, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_e(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($f, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_f(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($g, [A], F, Adj, P, Pad, _Enc, L) when is_float(A) ->
    Res = fwrite_g(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($b, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = unprefixed_integer(A, F, Adj, base(P), Pad, true),
    {Res, L - lists:flatlength(Res)};
control($B, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = unprefixed_integer(A, F, Adj, base(P), Pad, false),
    {Res, L - lists:flatlength(Res)};
control($x, [A,Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A),
                                                 is_atom(Prefix) ->
    Res = prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true),
    {Res, L - lists:flatlength(Res)};
control($x, [A,Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    Res = prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true),
    {Res, L - lists:flatlength(Res)};
control($X, [A,Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A),
                                                 is_atom(Prefix) ->
    Res = prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false),
    {Res, L - lists:flatlength(Res)};
control($X, [A,Prefix], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    Res = prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false),
    {Res, L - lists:flatlength(Res)};
control($+, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    Res = prefixed_integer(A, F, Adj, Base, Pad, Prefix, true),
    {Res, L - lists:flatlength(Res)};
control($#, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    Res = prefixed_integer(A, F, Adj, Base, Pad, Prefix, false),
    {Res, L - lists:flatlength(Res)};
control($c, [A], F, Adj, P, Pad, unicode, L) when is_integer(A) ->
    Res = char(A, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($c, [A], F, Adj, P, Pad, _Enc, L) when is_integer(A) ->
    Res = char(A band 255, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($~, [], F, Adj, P, Pad, _Enc, L) ->
    Res = char($~, F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($n, [], F, Adj, P, Pad, _Enc, L) ->
    Res = newline(F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control($i, [_A], _F, _Adj, _P, _Pad, _Enc, L) ->
    {[], L};
control($s, [A], F, Adj, P, Pad, _Enc, L) when is_atom(A) ->
    Res = string(atom_to_list(A), F, Adj, P, Pad),
    {Res, L - lists:flatlength(Res)};
control(C, A, F, Adj, P, Pad, Enc, L) ->
    %% save this for later - these are all the 'large' terms
    {{C, A, F, Adj, P, Pad, Enc}, L}.

control2($w, [A], F, Adj, P, Pad, _Enc, L) ->
    Term = fprint(A, L, [{lists_as_strings, false}]),
    Res = term(Term, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($p, [A], _F, _Adj, _P, _Pad, _Enc, L) ->
    Term = fprint(A, L, [{lists_as_strings, true}]),
    {Term, lists:flatlength(Term)};
control2($W, [A,Depth], F, Adj, P, Pad, _Enc, L) when is_integer(Depth) ->
    Term = fprint(A, L, [{depth, Depth}, {lists_as_strings, false}]),
    Res = term(Term, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($P, [A,Depth], _F, _Adj, _P, _Pad, _Enc, L) when is_integer(Depth) ->
    Term = fprint(A, L, [{depth, Depth}, {lists_as_strings, true}]),
    {Term, lists:flatlength(Term)};
control2($s, [L0], F, Adj, P, Pad, latin1, L) ->
    List = fprint(maybe_flatten(L0), L, [{force_strings, true}]),
    Res = string(List, F, Adj, P, Pad),
    {Res, lists:flatlength(Res)};
control2($s, [L0], F, Adj, P, Pad, unicode, L) ->
    List = fprint(unicode:characters_to_list(L0), L, [{force_strings, true}]),
    Res = uniconv(string(List, F, Adj, P, Pad)),
    {Res, lists:flatlength(Res)}.

maybe_flatten(X) when is_list(X) ->
    lists:flatten(X);
maybe_flatten(X) ->
    X.

make_options([], Options) ->
    Options;
make_options([{chomp, Bool}|T], Options) when is_boolean(Bool) ->
    make_options(T, Options#options{chomp=Bool}).

-ifdef(UNICODE_AS_BINARIES).
uniconv(C) ->
    unicode:characters_to_binary(C,unicode).
-else.
uniconv(C) ->
    C.
-endif.
%% Default integer base
base(none) ->
    10;
base(B) when is_integer(B) ->
    B.

%% term(TermList, Field, Adjust, Precision, PadChar)
%%  Output the characters in a term.
%%  Adjust the characters within the field if length less than Max padding
%%  with PadChar.

term(T, none, _Adj, none, _Pad) -> T;
term(T, none, Adj, P, Pad) -> term(T, P, Adj, P, Pad);
term(T, F, Adj, P0, Pad) ->
    L = lists:flatlength(T),
    P = case P0 of none -> erlang:min(L, F); _ -> P0 end,
    if
        L > P ->
            adjust(chars($*, P), chars(Pad, F-P), Adj);
        F >= P ->
            adjust(T, chars(Pad, F-L), Adj)
    end.

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) -> %Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    term(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_e(Fl, Fd, P) when Fl < 0.0 -> %Negative numbers
    [$-|float_e(-Fl, Fd, P)];
float_e(_Fl, {Ds,E}, P) ->
    case float_man(Ds, 1, P-1) of
        {[$0|Fs],true} -> [[$1|Fs]|float_exp(E)];
        {Fs,false} -> [Fs|float_exp(E-1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Chars],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs,C} = float_man(Ds, Dc),
    {[$.|Cs],C};
float_man([D|Ds], I, Dc) ->
    case float_man(Ds, I-1, Dc) of
        {Cs,true} when D =:= $9 -> {[$0|Cs],true};
        {Cs,true} -> {[D+1|Cs],false};
        {Cs,false} -> {[D|Cs],false}
    end;
float_man([], I, Dc) -> %Pad with 0's
    {string:chars($0, I, [$.|string:chars($0, Dc)]),false}.

float_man([D|_], 0) when D >= $5 -> {[],true};
float_man([_|_], 0) -> {[],false};
float_man([D|Ds], Dc) ->
    case float_man(Ds, Dc-1) of
        {Cs,true} when D =:= $9 -> {[$0|Cs],true};
        {Cs,true} -> {[D+1|Cs],false};
        {Cs,false} -> {[D|Cs],false}
    end;
float_man([], Dc) -> {string:chars($0, Dc),false}. %Pad with 0's

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e,$+|integer_to_list(E)];
float_exp(E) ->
    [$e|integer_to_list(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) -> %Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    term(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) when Fl < 0.0 ->
    [$-|float_f(-Fl, Fd, P)];
float_f(Fl, {Ds,E}, P) when E =< 0 ->
    float_f(Fl, {string:chars($0, -E+1, Ds),1}, P); %Prepend enough 0's
float_f(_Fl, {Ds,E}, P) ->
    case float_man(Ds, E, P) of
        {Fs,true} -> "1" ++ Fs; %Handle carry
        {Fs,false} -> Fs
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) ->
    float_data(float_to_list(Fl), []).

float_data([$e|E], Ds) ->
    {lists:reverse(Ds),list_to_integer(E)+1};
float_data([D|Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D|Ds]);
float_data([_|Cs], Ds) ->
    float_data(Cs, Ds).

%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4,
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E = if A < 1.0e-1 -> -2;
        A < 1.0e0  -> -1;
        A < 1.0e1  -> 0;
        A < 1.0e2  -> 1;
        A < 1.0e3  -> 2;
        A < 1.0e4  -> 3;
        true       -> fwrite_f
    end,
    if  P =< 1, E =:= -1;
    P-1 > E, E >= -1 ->
        fwrite_f(Fl, F, Adj, P-1-E, Pad);
    P =< 1 ->
        fwrite_e(Fl, F, Adj, 2, Pad);
    true ->
        fwrite_e(Fl, F, Adj, P, Pad)
    end.


%% string(String, Field, Adjust, Precision, PadChar)

string(S, none, _Adj, none, _Pad) -> S;
string(S, F, Adj, none, Pad) ->
    string_field(S, F, Adj, lists:flatlength(S), Pad);
string(S, none, _Adj, P, Pad) ->
    string_field(S, P, left, lists:flatlength(S), Pad);
string(S, F, Adj, P, Pad) when F >= P ->
    N = lists:flatlength(S),
    if F > P ->
            if N > P ->
                    adjust(flat_trunc(S, P), chars(Pad, F-P), Adj);
                N < P ->
                    adjust([S|chars(Pad, P-N)], chars(Pad, F-P), Adj);
                true -> % N == P
                    adjust(S, chars(Pad, F-P), Adj)
            end;
       true -> % F == P
        string_field(S, F, Adj, N, Pad)
    end.

string_field(S, F, _Adj, N, _Pad) when N > F ->
    flat_trunc(S, F);
string_field(S, F, Adj, N, Pad) when N < F ->
    adjust(S, chars(Pad, F-N), Adj);
string_field(S, _, _, _, _) -> % N == F
    S.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase)
%% -> [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            term([$-|S], F, Adj, none, Pad);
       true ->
        S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
        term(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase)
%% -> [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            term([$-,Prefix|S], F, Adj, none, Pad);
       true ->
        S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
        term([Prefix|S], F, Adj, none, Pad)
    end.

%% char(Char, Field, Adjust, Precision, PadChar) -> [Char].

char(C, none, _Adj, none, _Pad) -> [C];
char(C, F, _Adj, none, _Pad) -> chars(C, F);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(F, right, _P, _Pad) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data|Pad];
adjust(Data, Pad, right) -> [Pad|Data].

%% Flatten and truncate a deep list to at most N elements.
flat_trunc(List, N) when is_integer(N), N >= 0 ->
    flat_trunc(List, N, []).

flat_trunc(L, 0, R) when is_list(L) ->
    lists:reverse(R);
flat_trunc([H|T], N, R) ->
    flat_trunc(T, N-1, [H|R]);
flat_trunc([], _, R) ->
    lists:reverse(R).

%% A deep version of string:chars/2,3

chars(_C, 0) ->
    [];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C,C];
chars(C, 3) ->
    [C,C,C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C,S|S].

%chars(C, N, Tail) ->
%    [chars(C, N)|Tail].

%% Lowercase conversion

cond_lowercase(String, true) ->
    lowercase(String);
cond_lowercase(String,false) ->
    String.

lowercase([H|T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H-$A+$a)|lowercase(T)];
lowercase([H|T]) ->
    [H|lowercase(T)];
lowercase([]) ->
    [].


%%
%% lager_stdlib part
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%% @doc Functions from Erlang OTP distribution that are really useful
%% but aren't exported.
%%
%% All functions in this module are covered by the Erlang/OTP source
%% distribution's license, the Erlang Public License.  See
%% http://www.erlang.org/ for full details.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
        true -> string_p1(T);
        _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.
