-module(dtrace).

-export([init/0, available/0, user_trace/1,
         p/0, p/1, p/2, p/3, p/4, p/5, p/6, p/7, p/8]).
-export([scaff/0]). % Development only
-export([user_trace_i4s4/9]). % Know what you're doing!

-type probe_arg() :: integer() | iolist().

-spec init() -> ok | {error, {term(), term()}}.

init() ->
    PrivDir = code:priv_dir(dtrace),
    Lib = filename:join([PrivDir, "lib", "dtrace"]),
    erlang:load_nif(Lib, 0).

-spec available() -> true | false.

available() ->
    nif_not_loaded.

-spec user_trace(iolist()) -> true | false | error | badarg.

user_trace(_Message) ->
    nif_not_loaded.

-spec user_trace_i4s4(iolist(),
                      integer(), integer(), integer(), integer(),
                      iolist(), iolist(), iolist(), iolist()) ->
      true | false | error | badarg.

user_trace_i4s4(_, _, _, _, _, _, _, _, _) ->
    nif_not_loaded.

-spec p() -> true | false | error | badarg.

p() ->
    user_trace_int(undef, undef, undef, undef, undef, undef, undef, undef).

-spec p(probe_arg()) -> true | false | error | badarg.

p(I1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, undef, undef, undef, undef);
p(S1) ->
    user_trace_int(undef, undef, undef, undef, S1, undef, undef, undef).

-spec p(probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, undef, undef, undef, undef);
p(I1, S1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, undef, undef, undef);
p(S1, S2) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, undef, undef).

-spec p(probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2, I3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, undef, undef, undef, undef);
p(I1, I2, S1) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, undef, undef, undef);
p(I1, S1, S2) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, undef, undef);
p(S1, S2, S3) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, undef).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, undef, undef, undef, undef);
p(I1, I2, I3, S1) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, undef, undef, undef);
p(I1, I2, S1, S2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, undef, undef);
p(I1, S1, S2, S3) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, undef);
p(S1, S2, S3, S4) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, undef, undef, undef);
p(I1, I2, I3, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, undef, undef);
p(I1, I2, S1, S2, S3) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, undef);
p(I1, S1, S2, S3, S4) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, undef, undef);
p(I1, I2, I3, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3) -> 
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, undef);
p(I1, I2, S1, S2, S3, S4) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, undef);
p(I1, I2, I3, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, S4).

-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4).

-spec user_trace_int(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
                     probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4) ->
    user_trace_i4s4(<<"ToFix">>, I1, I2, I3, I4, S1, S2, S3, S4).

%% Scaffolding to write tedious code: quick brute force and not 100% correct.

scaff_int_args(N) ->
    L = lists:sublist(["I1", "I2", "I3", "I4"], N),
    [string:join(L, ", ")].

scaff_int_guards(N) ->
    L = lists:sublist(["is_integer(I1)", "is_integer(I2)", "is_integer(I3)",
                       "is_integer(I4)"], N),
    lists:flatten(string:join(L, ", ")).

scaff_char_args(N) ->
    L = lists:sublist(["S1", "S2", "S3", "S4"], N),
    [string:join(L, ", ")].

scaff_fill(N) ->
    [string:join(lists:duplicate(N, "undef"), ", ")].

scaff() ->
    L = [begin
             IntArgs = scaff_int_args(N_int),
             IntGuards = scaff_int_guards(N_int),
             IntFill = scaff_fill(4 - N_int),
             CharArgs = scaff_char_args(N_char),
             CharFill = scaff_fill(4 - N_char),
             InArgs = string:join(IntArgs ++ CharArgs, ", "),
             OutArgs = string:join(IntArgs ++ IntFill ++ CharArgs ++ CharFill,
                                   ", "),
             {N_int + N_char,
              lists:flatten([io_lib:format("p(~s) when ~s ->\n",
                                           [InArgs, IntGuards]),
                             io_lib:format("    user_trace_int(~s);\n", [OutArgs])
                            ])}
         end || N_int <- [0,1,2,3,4], N_char <- [0,1,2,3,4]],
    [io:format("%%~p\n~s", [N, Str]) || {N, Str} <- lists:sort(L)].
