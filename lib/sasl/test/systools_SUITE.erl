-module(systools_SUITE).

-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).

%% Test cases
-export([relup_app_start_type/1]).

all(suite) ->
    [relup_app_start_type].

relup_app_start_type(suite) ->
    [];
relup_app_start_type(doc) ->
    ["Release upgrade file with various application start types"];
relup_app_start_type(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Release1 = build_rel_file("release_1", Config),
    ?line Release2 = build_rel_file("release_2", Config),

    ?line {ok, Release2Relup, systools_relup, []} = systools:make_relup(Release2, [Release1], [Release1], [{outdir, PrivDir}, silent]),
    ?line {"2", [{"1",[], UpInstructions}], [{"1",[], DownInstructions}]} = Release2Relup,
    ?line [{load_object_code, {mnesia, _, _}},
           {load_object_code, {crypto, _, _}},
           {load_object_code, {ssl, _, _}},
           {load_object_code, {snmp, _, _}},
           {load_object_code, {ssh, _, _}},
           point_of_no_return
           | UpInstructionsT] = UpInstructions,
    ?line true = lists:member({apply,{application,start,[mnesia,permanent]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,start,[crypto,transient]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,start,[ssl,temporary]}}, UpInstructionsT),
    ?line true = lists:member({apply,{application,load,[snmp]}}, UpInstructionsT),
    ?line false = lists:any(fun({apply,{application,_,[ssh|_]}}) -> true; (_) -> false end, UpInstructionsT),
    ?line [point_of_no_return | DownInstructionsT] = DownInstructions,
    ?line true = lists:member({apply,{application,stop,[mnesia]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[crypto]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[ssl]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[snmp]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,stop,[ssh]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[mnesia]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[crypto]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[ssl]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[snmp]}}, DownInstructionsT),
    ?line true = lists:member({apply,{application,unload,[ssh]}}, DownInstructionsT),
    ok.

%% Build a release file from a version-agnostic release file (i.e. a release
%% file specific to the code that the test server currently knows).
%% Replace "%VSN%" with the actual value from the currently loaded application.
%% Return the path to use (the .rel file without the suffix).
-spec build_rel_file(string(), list()) -> string().
build_rel_file(ReleaseName, Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line ReleaseSourceFile = filename:join([DataDir, ReleaseName, "release.rel.src"]),
    ?line Release = filename:join([PrivDir, ReleaseName]),
    ?line ReleaseFile = Release ++ ".rel",

    ?line {ok, [{release, ReleaseDesc, {erts, "%VSN%"}, Apps}]} = file:consult(ReleaseSourceFile),
    ?line FixedApps = lists:map(fun(AppSpec) ->
        case AppSpec of
            {Application, "%VSN%"} ->
                ?line application:load(Application),
                ?line {Application, _Description, FixedVersion0} = lists:keyfind(Application, 1, application:loaded_applications()),
                ?line {Application, FixedVersion0};
            {Application, "%VSN%", StartType} ->
                ?line application:load(Application),
                ?line {Application, _Description, FixedVersion0} = lists:keyfind(Application, 1, application:loaded_applications()),
                ?line {Application, FixedVersion0, StartType};
            _ -> AppSpec
        end
    end, Apps),
    ?line FixedContent = {release, ReleaseDesc, {erts, erlang:system_info(version)}, FixedApps},
    ?line ok = file:write_file(ReleaseFile, [io_lib:write(FixedContent), "."]),
    Release.
