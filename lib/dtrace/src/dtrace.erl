-module(dtrace).

-export([init/0, available/0, user_trace/1]).

init() ->
    PrivDir = code:priv_dir(dtrace),
    Lib = filename:join([PrivDir, "lib", "dtrace"]),
    erlang:load_nif(Lib, 0).

available() ->
    nif_not_loaded.

user_trace(_Message) ->
    nif_not_loaded.
