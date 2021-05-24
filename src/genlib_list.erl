-module(genlib_list).

%% API
-export([join/2]).
-export([compact/1]).
-export([wrap/1]).
-export([group_by/2]).
-export([foldl_while/3]).
-export([orderless_equal/2]).

%%
%% API
%%
-spec join(D, list(E)) -> list(D | E).
join(_, []) -> [];
join(_, [H]) -> H;
join(Delim, [H | T]) -> [H, Delim, join(Delim, T)].

-spec compact(list(undefined | T)) -> list(T).
compact(List) ->
    lists:filter(
        fun
            (undefined) -> false;
            (_) -> true
        end,
        List
    ).

%% @doc Wrap given term into a list.
%% If a list is given, this list is returned.
%% If undefined is passed, empty list is returned

-spec wrap(undefined | list(T) | T) -> [] | list(T) when T :: term().
wrap(undefined) ->
    [];
wrap(List) when is_list(List) ->
    List;
wrap(Term) ->
    [Term].

%% @doc Group values in a given list by result of KeyFun.
%% The result is a map, where keys are results of KeyFun, and values are list of values of original list
%% that result in such key.
-spec group_by(fun((T) -> K), list(T)) -> #{K := list(T)} when T :: term(), K :: term().
group_by(KeyFun, List) ->
    lists:foldl(
        fun(Elt, Acc) ->
            Key = KeyFun(Elt),
            GroupList = maps:get(Key, Acc, []),
            maps:put(Key, [Elt | GroupList], Acc)
        end,
        #{},
        List
    ).

%% @doc Like lists:foldl, but can be stopped amid the traversal of a list.
%% Function must return {cont, NewAcc} to continue folding the list, or {halt, FinalAcc} to stop immediately.
-spec foldl_while(fun((T, Acc) -> {cont, Acc} | {halt, Acc}), Acc, list(T)) -> Acc when T :: term(), Acc :: term().
foldl_while(Fun, Acc, List) when is_function(Fun, 2), is_list(List) ->
    do_foldl_while(Fun, Acc, List).

do_foldl_while(_Fun, Acc, []) ->
    Acc;
do_foldl_while(Fun, Acc, [Elem | Rest]) ->
    case Fun(Elem, Acc) of
        {cont, NextAcc} ->
            do_foldl_while(Fun, NextAcc, Rest);
        {halt, FinalAcc} ->
            FinalAcc
    end.

-spec orderless_equal(list(), list()) -> boolean().
orderless_equal(List1, List2) ->
    (List1 -- List2) =:= (List2 -- List1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec wrap_empty_list_for_undefined_test() -> _.

wrap_empty_list_for_undefined_test() ->
    ?assertEqual(genlib_list:wrap(undefined), []).

-endif.
