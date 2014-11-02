-module(prop_e2_release).
-export([generate/1, command_line_options/0]).
-behaviour(prop).
-prop({e2, release}).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

%% Generate an OTP release skeleton
generate(Prop) ->
  Name = prop:attr(Prop, name),
  ok = prop:dir(Prop, Name, []),
  ok = prop:chdir(Name),
  ok = prop:template(Prop, "README.md", [announce]),
  ok = prop:template(Prop, "rebar.config", [announce]),
  ok = prop:template(Prop, "relx.config", [announce]),
  ok = prop:template(Prop, "relx-dev.config", [announce]),
  ok = prop:template(Prop, "sync.config", [announce]),
  ok = prop:dir(Prop, "apps", [announce]),
  ok = prop:generate({e2, app}, [{name, Name ++ "_core"},
                                 {invocation, command_line}]),
  ok = prop:exec(Prop, "rebar get-deps", [announce]),
  ok = prop:exec(Prop, "rebar compile", [announce]),
  ok.

%% Return command line option spec
command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP application"},
   {project_version, $p, "project_version", {string, "0.0.1"},
                     "Semantic version number for the project"}].
