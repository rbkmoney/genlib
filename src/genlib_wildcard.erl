%
% Wildcard matching

-module(genlib_wildcard).

-export([
    match/2
]).

-spec match(binary(), binary()) -> boolean().
match(Body, Pattern) when is_binary(Body) ->
    match(Body, Pattern, true).

match(Body, Pattern = <<"\\"/utf8, T/binary>>, true) ->
    case T of
        R = <<H/utf8, _/binary>> when H =:= $*; H =:= $?; H =:= $\\ ->
            match(Body, R, false);
        _ ->
            match(Body, Pattern, false)
    end;
match(_Body, <<"*"/utf8>>, true) ->
    true;
match(Body = <<_/utf8, Rest/binary>>, Pattern = <<"*"/utf8, T/binary>>, true) ->
    match(Body, T) orelse match(Rest, Pattern);
match(<<_/utf8, Rest/binary>>, <<"?"/utf8, T/binary>>, true) ->
    match(Rest, T);
match(<<H/utf8, R/binary>>, <<H/utf8, T/binary>>, _) ->
    match(R, T);
match(<<>>, <<>>, _) ->
    true;
match(_, _, _) ->
    false.
