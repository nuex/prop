-module(prop_module).
-behavior(prop).
-export([generate/1, name/0, description/0, options/0]).

name() -> module.
description() -> "Generate a module".
options() -> [].

generate(Prop) ->
  % TODO set destination dir depending on if we're in a release or not
  Destination = "src",
  prop:template(Prop, "mod.erl", filename:join([Destination, "{{name}}.erl"])).
