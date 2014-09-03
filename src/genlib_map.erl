%%%
%%% Genlib

-module(genlib_map).

%%

-export([get/2]).
-export([get/3]).
-export([mget/2]).
-export([foreach/2]).
-export([truemap/2]).
-export([compact/1]).
-export([atomize/1]).
-export([atomize/2]).
-export([binarize/1]).
-export([binarize/2]).

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

-spec mget([Key | {Key, Default}], map()) -> [Default | term()] when
    Key :: atom() | binary() | number(),
    Default :: term().

mget([{Key, Default} | Rest], Map) ->
    [get(Key, Map, Default) | mget(Rest, Map)];

mget([Key | Rest], Map) ->
    [get(Key, Map) | mget(Rest, Map)];

mget([], _Map) ->
    [].

-spec truemap(Function, map()) -> ok when
    Function :: fun((Key :: any(), Value :: any()) -> {Key :: any(), Value :: any()}).

truemap(F, Map = #{}) ->
    maps:fold(fun (K, V, M) -> {Kn, Vn} = F(K, V), maps:put(Kn, Vn, M) end, #{}, Map).

-spec foreach(Function, map()) -> ok when
    Function :: fun((Key :: any(), Value :: any()) -> any()).

foreach(F, Map = #{}) ->
    maps:fold(fun (K, V, _) -> F(K, V), ok end, ok, Map).

-spec compact(map()) -> map().

compact(Map = #{}) ->
    maps:fold(fun (K, undefined, M) -> maps:remove(K, M); (_, _, M) -> M end, Map, Map).

-spec atomize(#{binary() => any()}) -> #{atom() => any()}.

atomize(Map) ->
    truemap(fun (K, V) -> {binary_to_atom(K, utf8), V} end, Map).

-spec binarize(#{atom() => any()}) -> #{binary() => any()}.

binarize(Map) ->
    truemap(fun (K, V) -> {atom_to_binary(K, utf8), V} end, Map).

-spec atomize(Map, pos_integer() | infinity) -> AtomicMap when
    Map       :: #{binary() => Map | term()},
    AtomicMap :: #{atom() | binary() => AtomicMap | term()}.

atomize(Map, 1) ->
    atomize(Map);

atomize(Map, N) ->
    atomize(maps:map(fun (_, V) when is_map(V) -> atomize(V, decrement(N)); (_, V) -> V end, Map)).

-spec binarize(AtomicMap, pos_integer() | infinity) -> Map when
    Map       :: #{binary() => Map | term()},
    AtomicMap :: #{atom() | binary() => AtomicMap | term()}.

binarize(Map, 1) ->
    binarize(Map);

binarize(Map, N) ->
    binarize(maps:map(fun (_, V) when is_map(V) -> binarize(V, decrement(N)); (_, V) -> V end, Map)).

decrement(infinity) -> infinity;
decrement(N) -> N - 1.
