-module(rebar_gleam_prv_eunit).

-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eunit).
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
                                 {example, "rebar3 eunit"},
                                 {short_desc, "Run EUnit Tests."},
                                 {desc, "Run EUnit Tests."},
                                 {opts, rebar_prv_eunit:eunit_opts(State)},
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_gleam:provider_do(State, fun rebar_prv_eunit:do/1).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    rebar_prv_compile:format_error(Reason).
