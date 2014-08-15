%%
%% Option list mangling.
%% Option list assumed to be normalized proplist.

-module(genlib_opts).

%%

-export([get/2]).
-export([get/3]).

-export([require/2]).

-export([values/2]).

%%

-type key() :: atom().
-type opts() :: [{key(), any()}].

-export_type([opts/0]).

-spec get(key(), opts()) -> any() | undefined.

get(Key, Opts) ->
    get(Key, Opts, undefined).

-spec get(key(), opts(), Default) -> any() | Default when Default :: any().

get(Key, Opts, Default) when is_atom(Key) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end;

get(Key, _, _) ->
    error(badarg, [Key]).

-spec require(key(), opts()) -> any() | no_return().

require(Key, Opts) ->
    case get(Key, Opts, Ref = make_ref()) of
        Ref -> error({missing_options, Key});
        Value -> Value
    end.

-spec values([key() | {key(), Default}], opts()) -> [any() | Default] when Default :: any().

values([], _Opts) ->
    [];

values([{Key, Default} | Keys], Opts) ->
    [get(Key, Opts, Default) | values(Keys, Opts)];

values([Key | Keys], Opts) ->
    [get(Key, Opts) | values(Keys, Opts)].
