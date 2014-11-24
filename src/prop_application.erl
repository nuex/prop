-module(prop_application).
-export([generate/1, name/0, description/0, options/0]).

%% ===================================================================
%% API Functions
%% ===================================================================

name() -> application.
description() -> "Generate an application.".
options() -> [release_option(), description_option()].

generate(Prop) ->
  Name = prop:name(Prop),
  case proplists:lookup(release, prop:options(Prop)) /= undefined of
    true -> generate_release_app(Prop, Name);
    % Generate a  Erlang library project
    false -> todo
  end.

%% ===================================================================
%% Private Functions
%% ===================================================================

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the OTP application"}.

release_option() ->
  {release, $r, "release", boolean, ""}.

generate_release_app(Prop, Name) ->
  Root = case prop:root_directory(Prop) of
    undefined -> filename:join(["apps", Name]);
    RootDirectory -> filename:join([RootDirectory, "apps", Name])
  end,
  prop:template(Prop, "app.erl", [Root, "src", "{{name}}_app.erl"]),
  prop:template(Prop, "mod.erl", [Root, "src", "{{name}}.erl"]),
  prop:template(Prop, "sup.erl", [Root, "src", "{{name}}_sup.erl"]),
  prop:template(Prop, "app.app", [Root, "ebin", "{{name}}.app"]),
  prop:template(Prop, "rebar.config", [Root, "rebar.config"]).
