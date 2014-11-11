-module(prop_otp_release).
-export([generate/1, command_line_options/0, description/0]).
-behaviour(prop).
-prop({otp, release}).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

%% Return command line option spec
command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP application"}].

%% Return a description
description() -> "Generate an OTP release".

%% Generate an OTP release skeleton
generate(Prop) ->
  Name = prop:attr(Prop, name),
  ok = prop:dir(Prop, Name, []),
  ok = prop:chdir(Name),
  ok = prop:dir(Prop, "apps", [announce]),
  ok = prop:template(Prop, "README.md", [announce]).
