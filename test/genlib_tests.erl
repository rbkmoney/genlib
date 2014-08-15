%%%

-module(genlib_tests).

%%

-include_lib("eunit/include/eunit.hrl").

%%

dice_roll_test() ->
    _ = random:seed(now()),
    S = 10000,
    N = random_in(12, 20),
    Data = [{{choice, M}, random_in(10, 100)} || M <- lists:seq(1, N)],
    Rolls = [genlib:dice_roll(Data) || _ <- lists:seq(1, S)],
    Choices = lists:sort(group_choices(Rolls)),
    Ratios = compute_ratios(Data, Choices),
    Average = compute_average(Ratios),
    Stddev = compute_stddev(Ratios, Average),
    io:format(user, "~n", []),
    io:format(user, "Sample : ~B~n", [S]),
    io:format(user, "Bins   : ~B~n", [N]),
    io:format(user, "Avg    : ~p~n", [Average]),
    io:format(user, "Stddev : ~p~n", [Stddev]),
    ?assert(Stddev < 1),
    [{_, W1}, {_, W2} | _] = Data,
    [{_, N1}, {_, N2} | _] = Choices,
    ?assert(abs(W1 / W2 - N1 / N2) < 1),
    WSum = lists:foldl(fun ({_, W}, Acc) -> Acc + W end, 0, Data),
    ?assert(abs(Average - S / WSum) < 1).

print_test() ->
    ?assertMatch(<<"My hat">>, genlib:print(<<"My hate is for you, bastard!">>, 6)),
    ?assert(byte_size(genlib:print(<<"qwertysferferferfecefefegergeferfwdwfwfegwfgegr">>, 4)) =< 4),
    ?assert(byte_size(genlib:print({test, test, [lol, dance, "madness", <<"hehe">>]}, 2)) =< 2 ).
%%

random_in(A, B) ->
    random:uniform(B - A) + A - 1.

group_choices(Cs) ->
    lists:foldl(
        fun (C, Acc) -> orddict:update(C, fun (N) -> N + 1 end, 1, Acc) end,
        orddict:new(),
        Cs
    ).

compute_ratios(Data, Choices) ->
    lists:map(
        fun ({What, N}) -> {What, W} = lists:keyfind(What, 1, Data), N / W end,
        Choices
    ).

compute_average(Series) ->
    lists:sum(Series) / length(Series).

compute_stddev(Series, Average) ->
    Sums = lists:foldl(
        fun(V, Acc) -> D = V - Average, Acc + (D * D) end,
        0,
        Series
    ),
    math:sqrt(Sums / (length(Series) - 1)).
