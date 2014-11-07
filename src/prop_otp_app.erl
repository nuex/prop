-module(prop_otp_app).
-export([generate/1, command_line_options/0]).
-behaviour(prop).
-prop({otp, app}).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

%% Generate an OTP release skeleton
generate(Prop) ->
  Name = prop:attr(Prop, name),
  ok = prop:dir(Prop, Name, []),
  ok = prop:chdir(Name),
  ok = prop:dir(Prop, "src", [announce]),
  ok = prop:template(Prop, "README.md", [announce]),
  ok = prop:template(Prop, "LICENSE", [announce]),
  ok = prop:template(Prop, "app.erl", "src/{{name}}_app.erl", [announce]),
  ok = prop:template(Prop, "mod.erl", "src/{{name}}.erl", [announce]),
  ok = prop:template(Prop, "sup.erl", "src/{{name}}_sup.erl", [announce]),
  ok = prop:dir(Prop, "ebin", [announce]),
  ok = prop:template(Prop, "app.app", "ebin/{{name}}.app", [announce]),
  ok = prop:template(Prop, "rebar.config", [announce]),
  ok = prop:template(Prop, "relx.config", [announce]),
  ok = prop:template(Prop, "relx-dev.config", [announce]),
  ok = prop:template(Prop, "sync.config", [announce]),
  ok = prop:template(Prop, "gitignore", ".gitignore", [announce]),
  ok = prop:exec(Prop, "git init", [announce]),
  ok = prop:exec(Prop, "rebar get-deps", [announce]),
  ok = prop:exec(Prop, "rebar compile", [announce]),
  ok = prop:exec(Prop, "relx -c relx-dev.config", [announce]).

%% Return command line option spec
command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP library application"}].
