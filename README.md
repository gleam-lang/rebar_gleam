rebar_gleam
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar_gleam, ".*", {git, "git@host:user/rebar_gleam.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_gleam
    ===> Fetching rebar_gleam
    ===> Compiling rebar_gleam
    <Plugin Output>
