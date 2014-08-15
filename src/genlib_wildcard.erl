%
% Wildcard matching

-module(genlib_wildcard).

-export([
    match/2
]).

-spec match(binary(), binary()) -> boolean().
match(Body, Pattern) when is_binary(Body) ->
    match(Body, Pattern, true).

match(Body, Pattern = <<"\\", T/binary>>, true) ->
    case T of
        R = <<H, _/binary>> when H =:= $*; H =:= $?; H =:= $\\ ->
            match(Body, R, false);
        _ ->
            match(Body, Pattern, false)
    end;
match(_Body, <<"*">>, true) ->
    true;
match(Body = <<_, Rest/binary>>, Pattern = <<"*", T/binary>>, true) ->
    match(Body, T) orelse match(Rest, Pattern);
match(<<_, Rest/binary>>, <<"?", T/binary>>, true) ->
    match(Rest, T);
match(<<H, R/binary>>, <<H, T/binary>>, _) ->
    match(R, T);
match(<<>>, <<>>, _) ->
    true;
match(_, _, _) ->
    false.
