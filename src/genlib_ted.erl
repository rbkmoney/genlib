-module(genlib_ted).

-export([run/2]).

%%

-type annos() :: #{
    depth := non_neg_integer()
}.

-type editor() :: fun((term(), annos()) -> proceed | {replace, term()}).

-spec run(editor(), term()) -> term().
run(F, T) ->
    run(F, T, #{depth => 0}).

run(F, T, As) ->
    case F(T, As) of
        {replace, T1} ->
            T1;
        proceed ->
            run_proceed(F, T, As)
    end.

run_proceed(F, T, As) when is_map(T) ->
    As1 = submerge(As),
    Mapper = fun(K, Tk) -> {run(F, K, As1), run(F, Tk, As1)} end,
    genlib_map:truemap(Mapper, T);
run_proceed(F, T, As) when is_list(T) ->
    As1 = submerge(As),
    [run(F, Te, As1) || Te <- T];
run_proceed(F, T, As) when is_tuple(T) ->
    As1 = submerge(As),
    list_to_tuple([run(F, Te, As1) || Te <- tuple_to_list(T)]);
run_proceed(_F, T, _As) ->
    T.

submerge(Annos = #{depth := D}) ->
    Annos#{depth := D + 1}.
