-module(rebar_gleam_prv_compile).

-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 compile"},
        {short_desc, "Build Gleam projects with rebar3"},
        {desc, "Build Gleam projects with rebar3"},
        {opts, [{deps_only, $d, "deps_only", undefined,
                  "Only compile dependencies, no project apps will be built."}]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_gleam:provider_do(State, fun rebar_prv_compile:do/1).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).
