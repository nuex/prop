-module(prop_otp).
-export([generate/1]).
-behaviour(prop).
-prop(otp).

%% ===================================================================
%% API
%% ===================================================================

%% Generate an OTP release skeleton
generate(Options) ->
  {ok, OutputDirectory} = file:get_cwd(),
  Name = prop:attr(name, Options),
  TargetDirectory = filename:join([OutputDirectory, Name]),
  ok = prop:dir(TargetDirectory),
  ok = prop:chdir(TargetDirectory),
  ok = prop:dir("config"),
  ok = prop:dir("apps"),
  ok = prop:dir("priv"),
  ok = prop:dir("ebin"),
  ok = prop:template(?MODULE, "README.md", Options),
  ok = prop:template(?MODULE, "ebin/app.app", "ebin/{{name}}.app", Options).
