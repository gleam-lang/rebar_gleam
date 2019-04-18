-module(rebar_gleam).

% https://www.rebar3.org/docs/plugins

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_gleam_prv:init(State),
    {ok, State1}.
