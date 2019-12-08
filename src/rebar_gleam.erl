-module(rebar_gleam).

% https://www.rebar3.org/docs/plugins

-export([init/1, provider_do/2]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_gleam_prv_compile:init(State),
    {ok, State2} = rebar_gleam_prv_eunit:init(State1),
    {ok, State3} = rebar_gleam_prv_common_test:init(State2),
    {ok, State3}.

provider_do(State, Do) ->
  case rebar_utils:sh("gleam build .", [use_stdout, return_on_error]) of
    {ok, _} ->
      Do(State);

    _ ->
      io:put_chars("\n"),
      {error, "Unable to compile Gleam project"}
  end.
