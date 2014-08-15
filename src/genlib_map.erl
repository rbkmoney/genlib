%%%
%%% Genlib

-module(genlib_map).

%%

-export([get/2]).
-export([get/3]).
-export([foreach/2]).
-export([compact/1]).
-export([atomize/1]).
-export([atomize/2]).

%%

-spec get(any(), map()) -> undefined | term().

get(Key, Map) ->
    get(Key, Map, undefined).

-spec get(any(), map(), Default) -> Default | term() when
    Default :: term().

get(Key, Map = #{}, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

-spec foreach(Function, map()) -> ok when
    Function :: fun((Key :: any(), Value :: any()) -> any()).

foreach(F, Map = #{}) ->
    maps:fold(fun (K, V, _) -> F(K, V), ok end, ok, Map).

-spec compact(map()) -> map().

compact(Map = #{}) ->
    maps:fold(fun (K, undefined, M) -> maps:remove(K, M); (_, _, M) -> M end, Map, Map).

-spec atomize(#{binary() => any()}) -> #{atom() => any()}.

atomize(Map) ->
    maps:fold(fun (K, V, M) -> maps:put(binary_to_atom(K, utf8), V, M) end, #{}, Map).

-spec atomize(Map, pos_integer() | infinity) -> AtomicMap when
    Map       :: #{binary() => Map | term()},
    AtomicMap :: #{atom() | binary() => AtomicMap | term()}.

atomize(Map, 1) ->
    atomize(Map);

atomize(Map, N) ->
    atomize(maps:map(fun (_, V) when is_map(V) -> atomize(V, decrement(N)); (_, V) -> V end, Map)).

decrement(infinity) -> infinity;
decrement(N) -> N - 1.
