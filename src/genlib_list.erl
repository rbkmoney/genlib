-module(genlib_list).

%% API
-export([join/2]).
-export([compact/1]).

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
