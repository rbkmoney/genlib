%%%
%%% It's the needed private parts from otp gen* modules.
%%%
-module(genlib_gen).

%% API
-export_type([reason                /0]).
-export_type([start_ret             /0]).
-export_type([ref                   /0]).
-export_type([reg_name              /0]).
-export_type([server_from           /0]).
-export_type([server_init_ret       /1]).
-export_type([server_handle_call_ret/1]).
-export_type([server_handle_cast_ret/1]).
-export_type([server_handle_info_ret/1]).
-export_type([server_code_change_ret/1]).
-export_type([supervisor_ret        /0]).
-export([reg_name2_ref  /1]).
-export([where          /1]).
-export([reg_name_to_pid/1]).
-export([ref_to_pid     /1]).

%%
%% API
%%
-type reason() ::
      normal
    | shutdown
    | {shutdown, _}
    | _
.

-type start_ret() ::
      {ok, pid()}
    | ignore
    | {error, _}
.

-type ref() ::
      atom()
    | {node(), atom()}
    | {global, atom()}
    | {via, atom(), term()}
    | pid()
.
-type reg_name() ::
      {local , atom()}
    | {global, term()}
    | {via, module(), term()}
.

-type server_from() :: {pid(), _}.

-type server_init_ret(State) ::
       ignore
    | {ok  , State   }
    | {stop, reason()}
    | {ok  , State   , timeout_() | hibernate}
.

-type server_handle_call_ret(Reply, State) ::
      {noreply, State   }
    | {noreply, State   , timeout_() | hibernate}
    | {reply  , Reply   , State                 }
    | {stop   , reason(), State                 }
    | {reply  , Reply   , State        , timeout_() | hibernate}
    | {stop   , reason(), _Reply       , State                 }
.

-type server_handle_cast_ret(State) ::
      {noreply, State   }
    | {noreply, State   , timeout_() | hibernate}
    | {stop   , reason(), State                 }
.

-type server_handle_info_ret(State) ::
      {noreply, State   }
    | {noreply, State   , timeout_() | hibernate}
    | {stop   , reason(), State                 }
.

-type server_code_change_ret(State) ::
      {ok   , State}
    | {error, _    }
.

-type supervisor_ret() ::
      ignore
    | {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}
.

-spec
reg_name2_ref(reg_name()   ) -> ref().
reg_name2_ref({local, Name}) -> Name;
reg_name2_ref(V={global, _}) -> V;
reg_name2_ref(V={via, _, _}) -> V.

-spec where(reg_name()) ->
    pid().
where({local, Name})  ->
    erlang:whereis(Name);
where({global, Name}) ->
    global:whereis_name(Name);
where({via, Module, Name}) ->
    Module:whereis_name(Name).

-spec reg_name_to_pid(reg_name()) ->
    pid() | undefined.
reg_name_to_pid({global, Name}) ->
    global:whereis_name(Name);
reg_name_to_pid({via, Module, Name}) ->
    Module:whereis_name(Name);
reg_name_to_pid({local, Name})  ->
    erlang:whereis(Name).

-spec ref_to_pid(ref()) ->
    pid() | undefined.
ref_to_pid(Name) when is_atom(Name) ->
    erlang:whereis(Name);
ref_to_pid({Name, Node}) when is_atom(Name) andalso is_atom(Node) ->
    erlang:exit(not_implemented);
ref_to_pid({global, Name}) ->
    global:whereis_name(Name);
ref_to_pid({via, Module, Name}) ->
    Module:whereis_name(Name);
ref_to_pid(Pid) when is_pid(Pid) ->
    Pid.
