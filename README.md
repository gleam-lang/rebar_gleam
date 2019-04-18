# rebar_gleam

A rebar3 plugin that adds templates for generating [Gleam](https://gleam.run)
projects.

## Installation

Add the plugin to your global rebar config located at `~/.config/rebar3/rebar.config`.

If you do not have a file at `~/.config/rebar3/rebar.config` please create it
and add the following:

```erlang
{plugins, [
    {rebar_gleam, {git, "https://github.com/gleam-lang/rebar_gleam"}},
]}.
```

If you wish to upgrade to the latest version of this plugin run this command:

```shell
rebar3 as global plugins upgrade rebar_gleam
```

## Usage

```
rebar3 new gleam_lib my_amazing_application
```
