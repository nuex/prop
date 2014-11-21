-module(prop_application).
-export([generate/1, name/0, description/0, options/0]).

%% ===================================================================
%% API Functions
%% ===================================================================

name() -> application.
description() -> "Generate an application.".
options() -> [description_option()].

generate(Prop) ->
  Name = prop:attr(Prop, name),
  prop:destination(Prop, ["apps", Name]),
  prop:template(Prop, "README.md"),
  prop:template(Prop, "LICENSE"),
  prop:template(Prop, "app.erl", ["src", "{{name}}_app.erl"]),
  prop:template(Prop, "mod.erl", ["src", "{{name}}.erl"]),
  prop:template(Prop, "sup.erl", ["src", "{{name}}_sup.erl"]),
  prop:template(Prop, "app.app", ["ebin", "{{name}}.app"]),
  prop:template(Prop, "rebar.config").

%% ===================================================================
%% Private Functions
%% ===================================================================

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the OTP application"}.
