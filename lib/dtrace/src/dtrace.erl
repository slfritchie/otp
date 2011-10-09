-module(dtrace).

-export([init/0, available/0]).

init() ->
    PrivDir = code:priv_dir(dtrace),
    Lib = filename:join([PrivDir, "lib", "dtrace"]),
    erlang:load_nif(Lib, 0).

available() ->
      nif_not_loaded.

