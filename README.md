# rebar_gleam

Build Gleam projects with rebar3!

## Installation

Add the plugin to your project's `rebar.config`.

```erlang
{project_plugins, [
    {rebar_gleam, {git, "https://github.com/gleam-lang/rebar_gleam", {branch, "master"}}}
]}.
```

If you wish to write Gleam modules in your project as well as being able to
use dependency modules written in Gleam add this configuration to your
project's `rebar.config`:

```erlang
{src_dirs, ["src", "gen/src"]}.
{profiles, [
    {test, [{src_dirs, ["src", "test", "gen/src", "gen/test"]}]}
]}.
```
