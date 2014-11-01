-module(prop_otp_release).
-export([generate/1, command_line_options/0]).
-behaviour(prop).
-prop({otp, release}).

%% ===================================================================
%% CALLBACKS
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

%% Return command line option spec
command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP application"}].

%% ===================================================================
%% API
%% ===================================================================
