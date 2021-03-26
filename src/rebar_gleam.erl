-module(rebar_gleam).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_project_builder(State, gleam, rebar_gleam_builder),
    {ok, State1}.

