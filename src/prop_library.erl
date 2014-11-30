-module(prop_library).

%% Prop Callbacks
-export([generate/1, name/0, description/0, options/0]).

name() -> library.
description() -> "Generate a library project.".
options() -> [description_option()].

generate(Prop) ->
  Root = prop:name(Prop),
  prop:template(Prop, "README.md", [Root, "README.md"]),
  prop:template(Prop, "mod.erl", [Root, "src", "{{name}}.erl"]),
  prop:template(Prop, "app.app", [Root, "ebin", "{{name}}.app"]),
  prop:template(Prop, "rebar.config", [Root, "rebar.config"]),
  prop:template(Prop, "sync.config", [Root, "sync.config"]).

%% Private

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the library application"}.
