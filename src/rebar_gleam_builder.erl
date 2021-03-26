-module(rebar_gleam_builder).

-export([build/1]).

build(AppInfo) ->
    erlang:display(AppInfo),
    ok.
