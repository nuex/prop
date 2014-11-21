-module(prop_release).
-behavior(prop).
-export([generate/1, name/0, description/0, options/0]).

%% ===================================================================
%% API Functions
%% ===================================================================

name() -> release.
description() -> "Generate a release project".
options() -> [description_option()].

generate(Prop) ->
  Name = prop:attr(Prop, name),
  prop:destination(Prop, Name),
  prop:template(Prop, "README.md", "README.md").

%% ===================================================================
%% Private Functions
%% ===================================================================

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the OTP application"}.
