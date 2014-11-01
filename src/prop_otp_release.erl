-module(prop_otp_release).
-export([generate/1, command_line_options/0]).
-behaviour(prop).
-prop({otp, release}).

%% ===================================================================
%% CALLBACKS
%% ===================================================================

%% Generate an OTP release skeleton
generate(Prop) ->
  {ok, OutputDirectory} = file:get_cwd(),
  Name = prop:attr(Prop, name),
  _TargetDirectory = filename:join([OutputDirectory, Name]),
  ok = prop:dir(Prop, Name, []),
  ok = prop:chdir(Name),
  ok = prop:dir(Prop, "config", [announce]),
  ok = prop:dir(Prop, "apps", [announce]),
  ok = prop:dir(Prop, "priv", [announce]),
  ok = prop:dir(Prop, "ebin", [announce]),
  ok = prop:template(Prop, "README.md", [announce]),
  ok = prop:template(Prop, "ebin/app.app", "ebin/{{name}}.app", [announce]).

%% Return command line option spec
command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP application"}].
