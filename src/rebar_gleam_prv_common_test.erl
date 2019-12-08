-module(rebar_gleam_prv_common_test).

-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ct).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {bare, true},
                                 {example, "rebar3 ct"},
                                 {short_desc, "Run Common Tests."},
                                 {desc, "Run Common Tests."},
                                 {opts, ct_opts(State)},
                                 {profiles, [test]}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_gleam:provider_do(State, fun rebar_prv_common_test:do/1).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).

ct_opts(_State) ->
    [{dir, undefined, "dir", string, help(dir)}, %% comma-separated list
     {suite, undefined, "suite", string, help(suite)}, %% comma-separated list
     {group, undefined, "group", string, help(group)}, %% comma-separated list
     {testcase, undefined, "case", string, help(testcase)}, %% comma-separated list
     {label, undefined, "label", string, help(label)}, %% String
     {config, undefined, "config", string, help(config)}, %% comma-separated list
     {spec, undefined, "spec", string, help(spec)}, %% comma-separated list
     {join_specs, undefined, "join_specs", boolean, help(join_specs)},
     {allow_user_terms, undefined, "allow_user_terms", boolean, help(allow_user_terms)}, %% Bool
     {logdir, undefined, "logdir", string, help(logdir)}, %% dir
     {logopts, undefined, "logopts", string, help(logopts)}, %% comma-separated list
     {verbosity, undefined, "verbosity", integer, help(verbosity)}, %% Integer
     {cover, $c, "cover", {boolean, false}, help(cover)},
     {cover_export_name, undefined, "cover_export_name", string, help(cover_export_name)},
     {repeat, undefined, "repeat", integer, help(repeat)}, %% integer
     {duration, undefined, "duration", string, help(duration)}, % format: HHMMSS
     {until, undefined, "until", string, help(until)}, %% format: YYMoMoDD[HHMMSS]
     {force_stop, undefined, "force_stop", string, help(force_stop)}, %% String
     {basic_html, undefined, "basic_html", boolean, help(basic_html)}, %% Boolean
     {stylesheet, undefined, "stylesheet", string, help(stylesheet)}, %% String
     {decrypt_key, undefined, "decrypt_key", string, help(decrypt_key)}, %% String
     {decrypt_file, undefined, "decrypt_file", string, help(decrypt_file)}, %% String
     {abort_if_missing_suites, undefined, "abort_if_missing_suites", {boolean, true}, help(abort_if_missing_suites)}, %% Boolean
     {multiply_timetraps, undefined, "multiply_timetraps", integer, help(multiple_timetraps)}, %% Integer
     {scale_timetraps, undefined, "scale_timetraps", boolean, help(scale_timetraps)},
     {create_priv_dir, undefined, "create_priv_dir", string, help(create_priv_dir)},
     {include, undefined, "include", string, help(include)},
     {readable, undefined, "readable", string, help(readable)},
     {verbose, $v, "verbose", boolean, help(verbose)},
     {name, undefined, "name", atom, help(name)},
     {sname, undefined, "sname", atom, help(sname)},
     {setcookie, undefined, "setcookie", atom, help(setcookie)},
     {sys_config, undefined, "sys_config", string, help(sys_config)}, %% comma-separated list
     {compile_only, undefined, "compile_only", boolean, help(compile_only)},
     {retry, undefined, "retry", boolean, help(retry)},
     {fail_fast, undefined, "fail_fast", {boolean, false}, help(fail_fast)}
    ].

help(compile_only) ->
    "Compile modules in the project with the test configuration but do not run the tests";
help(dir) ->
    "List of additional directories containing test suites";
help(suite) ->
    "List of test suites to run";
help(group) ->
    "List of test groups to run";
help(testcase) ->
    "List of test cases to run";
help(label) ->
    "Test label";
help(config) ->
    "List of config files";
help(spec) ->
    "List of test specifications";
help(join_specs) ->
    "Merge all test specifications and perform a single test run";
help(sys_config) ->
    "List of application config files";
help(allow_user_terms) ->
    "Allow user defined config values in config files";
help(logdir) ->
    "Log folder";
help(logopts) ->
    "Options for common test logging";
help(verbosity) ->
    "Verbosity";
help(cover) ->
    "Generate cover data";
help(cover_export_name) ->
    "Base name of the coverdata file to write";
help(repeat) ->
    "How often to repeat tests";
help(duration) ->
    "Max runtime (format: HHMMSS)";
help(until) ->
    "Run until (format: HHMMSS)";
help(force_stop) ->
    "Force stop on test timeout (true | false | skip_rest)";
help(basic_html) ->
    "Show basic HTML";
help(stylesheet) ->
    "CSS stylesheet to apply to html output";
help(decrypt_key) ->
    "Path to key for decrypting config";
help(decrypt_file) ->
    "Path to file containing key for decrypting config";
help(abort_if_missing_suites) ->
    "Abort if suites are missing";
help(multiply_timetraps) ->
    "Multiply timetraps";
help(scale_timetraps) ->
    "Scale timetraps";
help(create_priv_dir) ->
    "Create priv dir (auto_per_run | auto_per_tc | manual_per_tc)";
help(include) ->
    "Directories containing additional include files";
help(readable) ->
    "Shows test case names and only displays logs to shell on failures (true | compact | false)";
help(verbose) ->
    "Verbose output";
help(name) ->
    "Gives a long name to the node";
help(sname) ->
    "Gives a short name to the node";
help(setcookie) ->
    "Sets the cookie if the node is distributed";
help(retry) ->
    "Experimental feature. If any specification for previously failing test is found, runs them.";
help(fail_fast) ->
    "Experimental feature. If any test fails, the run is aborted. Since common test does not "
    "support this natively, we abort the rebar3 run on a failure. This May break CT's disk logging and "
    "other rebar3 features.";
help(_) ->
    "".
