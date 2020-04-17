%
% Wildcard matching

-module(genlib_wildcard).

-export([
    match/2
]).

-spec match(binary(), binary()) -> boolean().

match(<<H/utf8, R/binary>>, <<"\\"/utf8, H/utf8, T/binary>>) ->
    match(R, T);
match(Body = <<_/utf8, Rest/binary>>, Pattern = <<"*"/utf8, T/binary>>) ->
    match(Body, T) orelse match(Rest, Pattern);
match(Body, <<"*"/utf8, T/binary>>) ->
    match(Body, T);
match(<<_/utf8, Rest/binary>>, <<"?"/utf8, T/binary>>) ->
    match(Rest, T);
match(<<H/utf8, R/binary>>, <<H/utf8, T/binary>>) ->
    match(R, T);
match(_, <<"\\"/utf8, _/binary>>) ->
    false;
match(<<>>, <<>>) ->
    true;
match(_, _) ->
    false.
