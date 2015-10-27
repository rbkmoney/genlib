%%
%% Application related activities

-module(genlib_app).

-export([

    childspec/4,
    childspec/5,
    permanent/3,

    transient/3,
    temporary/3,
    supervisor/3,

    priv_dir/0,
    priv_dir/1,
    priv_path/1,
    priv_path/2,

    start_application/1,
    start_application_with/2,
    stop_unload_applications/1,

    test_application_start/1,
    test_application_stop/1,
    test_application_unload/1,
    env/1,
    env/2,
    env/3

]).

-define(DEFAULT_TIMEOUT, 5000).

-type role()    :: permanent | transient | temporary | supervisor.
-type name()    :: atom() | term().
-type entry()   :: atom(). %% e.g. 'start_link'
-type spec()    :: {name(), module(), entry()} | {name(), module()} | module().
-type regtype() :: none | local | global.

-spec permanent(spec(), regtype(), genlib_opts:opts()) -> supervisor:child_spec().
permanent(Spec, RegType, Options) -> childspec(permanent, Spec, RegType, Options).

-spec transient(spec(), regtype(), genlib_opts:opts()) -> supervisor:child_spec().
transient(Spec, RegType, Options) -> childspec(transient, Spec, RegType, Options).

-spec temporary(spec(), regtype(), genlib_opts:opts()) -> supervisor:child_spec().
temporary(Spec, RegType, Options) -> childspec(temporary, Spec, RegType, Options).

-spec supervisor(spec(), regtype(), genlib_opts:opts()) -> supervisor:child_spec().
supervisor(Spec, RegType, Options) -> childspec(supervisor, Spec, RegType, Options).

-spec childspec(role(), spec(), regtype(), genlib_opts:opts()) -> supervisor:child_spec().

childspec(Role, Spec, RegType, Options) ->
    {Name, Entry, Deps} = entry(Spec, {RegType, Options}),
    {Name, Entry, restart_mode(Role), kill_mode(Role), role(Role), Deps}.

-spec childspec(role(), spec(), regtype(), genlib_opts:opts(), timeout()) -> supervisor:child_spec().

childspec(Role, Spec, RegType, Options, Timeout) ->
    {Name, Entry, Deps} = entry(Spec, {RegType, Options}),
    {Name, Entry, restart_mode(Role), Timeout, role(Role), Deps}.

entry({Name, Module, Entry}, {RegType, Options}) ->
    {Name, {Module, Entry, arguments(RegType, Name, Options)}, [Module]};

entry({Name, Module}, Options) ->
    entry({Name, Module, start_link}, Options);

entry(Module, Options) ->
    entry({Module, Module, start_link}, Options).

arguments(none, _, Args) -> wrap_args(Args);
arguments(Type, Name, Args) -> [{Type, Name} | wrap_args(Args)].

role(supervisor) -> supervisor;
role(_) -> worker.

restart_mode(supervisor) -> permanent;
restart_mode(Mode) -> Mode.

kill_mode(supervisor) -> infinity;
kill_mode(temporary) -> brutal_kill;
kill_mode(_) -> ?DEFAULT_TIMEOUT.

wrap_args(List) when is_list(List) -> List;
wrap_args(Term) -> [Term].

%% Private directories expansion

-spec priv_dir() -> file:filename().

priv_dir() ->
    {ok, AppName} = application:get_application(),
    priv_dir(AppName).

-spec priv_dir(Application :: atom()) -> file:filename().

priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            select_priv_dir([filename:join(["apps", atom_to_list(AppName), "priv"]), "priv"])
     end.

-spec priv_path(Relative :: file:filename()) -> file:filename().

priv_path(Relative) ->
    filename:join(priv_dir(), Relative).

-spec priv_path(Application :: atom(), Relative :: file:filename()) -> file:filename().

priv_path(AppName, Relative) ->
    filename:join(priv_dir(AppName), Relative).

%%

-spec start_application(Application :: atom()) -> [Application] when
    Application :: atom().

start_application(AppName) ->
    case application:start(AppName, permanent) of
        ok ->
            [AppName];
        {error, {already_started, AppName}} ->
            [];
        {error, {Status, DepName}} when Status =:= not_started; Status =:= not_running ->
            start_application(DepName) ++ start_application(AppName);
        {error, Reason} ->
            exit(Reason)
    end.

-spec start_application_with(Application, genlib_opts:opts()) -> [Application] when
    Application :: atom().

start_application_with(App, Env) ->
    _ = application:load(App),
    _ = set_app_env(App, Env),
    start_application(App).

-spec test_application_start(Application) -> [Application] when
    Application :: atom().

test_application_start(App) ->
    start_application(App).

-spec test_application_stop([Application :: atom()]) -> ok.

test_application_stop(Apps) ->
    _ = [application:stop(App) || App <- lists:reverse(Apps)],
    ok.

test_application_unload(Apps) ->
    _ = [application:unload(App) || App <- lists:reverse(Apps)],
    ok.

-spec stop_unload_applications([Application :: atom()]) -> ok.

stop_unload_applications(Apps) ->
    _ = test_application_stop(Apps),
    test_application_unload(Apps).

set_app_env(App, Env) ->
    R = [application:set_env(App, K, V) || {K, V} <- Env],
    _ = lists:all(fun (E) -> E =:= ok end, R) orelse exit(setenv_failed),
    ok.

%%

-spec env(Application :: atom()) -> genlib_opts:opts().

env(App) ->
    application:get_all_env(App).

-spec env(Application :: atom(), Key :: atom()) -> term() | undefined.

env(App, Key) ->
    env(App, Key, undefined).

-spec env(Application :: atom(), Key :: atom(), Default) -> term() | Default when
    Default :: any().

env(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        undefined   -> Default
    end.

%%

-include_lib("kernel/include/file.hrl").

select_priv_dir(Paths) ->
    case lists:dropwhile(fun test_priv_dir/1, Paths) of
        [Path | _] -> Path;
        _          -> exit(no_priv_dir)
    end.

test_priv_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            false;
        _ ->
            true
    end.
