%%%
%%% Genlib
%%% JSON objects manipulations
%%%
%%% Jiffy-style object notation.
%%% Mimics upcoming R17 release' maps module.

-module(genlib_json).

-export([new      /0]).
-export([get      /2]).
-export([find     /2]).
-export([is_key   /2]).
-export([put      /3]).
-export([update   /3]).
-export([remove   /2]).
-export([size     /1]).
-export([keys     /1]).
-export([values   /1]).
-export([fold     /3]).
-export([map      /2]).
-export([merge    /2]).
-export([without  /2]).
-export([from_list/1]).
-export([to_list  /1]).

-export([to_map   /1]).
-export([from_map /1]).

-export_type([key   /0]).
-export_type([scalar/0]).
-export_type([value /0]).
-export_type([object/0]).
-export_type([json  /0]).


-type(key   () :: binary()).
-type(scalar() :: 'null' | binary() | boolean() | number()).
-type(value () :: scalar() | [value()] | object()).
-type(object() :: {[{key(), value()}]}).
-type(json  () :: value()).


-spec new() -> Object when
    Object :: object().

new() ->
    {[]}.


-spec get(Key, Object) -> Value when
    Key :: key(),
    Object :: object(),
    Value :: value().

get(Key, Object) ->
    case lists:keyfind(Key, 1, to_list(Object)) of
        false        -> undefined;
        {Key, Value} -> Value
    end.


-spec find(Key, Object) -> {ok, Value} | error when
    Key :: key(),
    Object :: object(),
    Value :: value().

find(Key, Object) ->
    case lists:keyfind(Key, 1, to_list(Object)) of
        false        -> error;
        {Key, Value} -> {ok, Value}
    end.


-spec is_key(Key, Object) -> boolean() when
    Key :: key(),
    Object :: object().

is_key(Key, Object) ->
    lists:keymember(Key, 1, to_list(Object)).


-spec put(Key, Value, Object1) -> Object2 when
    Key :: key(),
    Value :: value(),
    Object1 :: object(),
    Object2 :: object().

put(Key, Value, Object) ->
    from_list(lists:keystore(Key, 1, to_list(Object), {Key, Value})).


-spec update(Key, Value, Object1) -> Object2 when
    Key :: key(),
    Value :: value(),
    Object1 :: object(),
    Object2 :: object().

update(Key, Value, Object) ->
    from_list(lists:keyreplace(Key, 1, to_list(Object), {Key, Value})).


-spec remove(Key, Object1) -> Object2 when
    Key :: key(),
    Object1 :: object(),
    Object2 :: object().

remove(Key, Object) ->
    from_list(lists:keydelete(Key, 1, to_list(Object))).


-spec size(Object) -> non_neg_integer() when
    Object :: object().

size(Object) ->
    length(to_list(Object)).


-spec keys(Object) -> Keys when
    Object :: object(),
    Keys :: [Key],
    Key :: key().

keys(Object) ->
    {Keys, _} = lists:unzip(to_list(Object)),
    Keys.


-spec values(Object) -> Keys when
    Object :: object(),
    Keys :: [Key],
    Key :: key().

values(Object) ->
    {_, Values} = lists:unzip(to_list(Object)),
    Values.


-spec fold(Fun, Init, Object) -> Acc when
    Fun :: fun((K, V, AccIn) -> AccOut),
    Init :: term(),
    Acc :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Object :: object(),
    K :: key(),
    V :: value().

fold(Fun, Init, Object) when is_function(Fun,3) ->
    lists:foldl(
        fun({K, V}, A) ->
            Fun(K, V, A)
        end,
        Init,
        to_list(Object)
    ).


-spec map(Fun, Object1) -> Object2 when
    Fun :: fun((K, V1) -> V2),
    Object1 :: object(),
    Object2 :: object(),
    K :: key(),
    V1 :: value(),
    V2 :: value().

map(Fun, Object) when is_function(Fun, 2) ->
    from_list(
        lists:map(
            fun({K, V}) ->
                {K, Fun(K,V)}
            end,
            to_list(Object))
    ).


-spec merge(Object1, Object2) -> Object3 when
    Object1 :: object(),
    Object2 :: object(),
    Object3 :: object().

merge(Object1, Object2) ->
    from_list(lists:keymerge(1, to_list(Object1), to_list(Object2))).


-spec without(Ks, Object1) -> Object2 when
    Ks :: [K],
    Object1 :: object(),
    Object2 :: object(),
    K :: term().

without(Ks, M) when is_list(Ks) ->
    from_list([{K,V} || {K,V} <- to_list(M), not lists:member(K, Ks)]).


-spec from_list(List) -> Object when
    List :: [{Key,Value}],
    Key :: key(),
    Value :: key(),
    Object :: object().

from_list(List) ->
    {List}.


-spec to_list(Object) -> [{Key,Value}] when
    Object :: object(),
    Key :: key(),
    Value :: value().

to_list({List}) ->
    List.

%%
%% WARNING!
%% Maps abused shamelessly down here!

-spec to_map(Object) -> map() | scalar() when
    Object :: object().

to_map(Object) ->
    to_map_opt(Object).

to_map_opt({List}) ->
    maps:from_list([{Key, to_map_opt(Value)} || {Key, Value} <- List]);

to_map_opt(List) when is_list(List) ->
    lists:map(fun to_map_opt/1, List);

to_map_opt(Value) ->
    Value.

-spec from_map(map() | scalar()) -> Object when
    Object :: object().

from_map(Map) ->
    from_map_opt(Map).

from_map_opt(Map = #{}) ->
    from_list(maps:to_list(maps:map(fun (_, Value) -> from_map_opt(Value) end, Map)));

from_map_opt(List) when is_list(List) ->
    lists:map(fun from_map_opt/1, List);

from_map_opt(Value) ->
    Value.
