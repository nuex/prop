-module(prop_application).

%% Prop Callbacks
-export([generate/1, name/0, description/0, options/0]).

name() -> application.
description() -> "Generate an application.".
options() -> [release_option(), description_option()].

generate(Prop) ->
  ReleaseOption = proplists:get_value(release, prop:options(Prop)),
  generate_app(ReleaseOption, Prop).

%% Private

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the OTP application"}.

generate_app(true, Prop) -> generate_release_app(Prop);
generate_app(false, Prop) -> generate_standalone_app(Prop);
generate_app(undefined, Prop) ->
  InReleaseDir = filelib:is_dir("apps"),
  generate_app(InReleaseDir, Prop).

generate_release_app(Prop) ->
  Name = prop:name(Prop),
  Root = case prop:root_directory(Prop) of
    undefined -> filename:join(["apps", Name]);
    RootDirectory -> filename:join([RootDirectory, "apps", Name])
  end,
  prop:template(Prop, "app.erl", [Root, "src", "{{name}}_app.erl"]),
  prop:template(Prop, "mod.erl", [Root, "src", "{{name}}.erl"]),
  prop:template(Prop, "sup.erl", [Root, "src", "{{name}}_sup.erl"]),
  prop:template(Prop, "app.app", [Root, "ebin", "{{name}}.app"]),
  prop:template(Prop, "rebar.config", [Root, "rebar.config"]).

generate_standalone_app(Prop) ->
  Name = prop:name(Prop),
  prop:template(Prop, "README.md", [Name, "README.md"]),
  prop:template(Prop, "app.erl", [Name, "src", "{{name}}_app.erl"]),
  prop:template(Prop, "mod.erl", [Name, "src", "{{name}}.erl"]),
  prop:template(Prop, "sup.erl", [Name, "src", "{{name}}_sup.erl"]),
  prop:template(Prop, "app.app", [Name, "ebin", "{{name}}.app"]),
  prop:template(Prop, "rebar.config", [Name, "rebar.config"]).

release_option() -> {release, $r, "release", boolean, ""}.
