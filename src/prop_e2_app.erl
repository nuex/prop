-module(prop_e2_app).
-behaviour(prop).
-export([generate/1, command_line_options/0, description/0]).
-prop({e2, app}).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

command_line_options() ->
  [{description, $d, "description", {string, "Default description"},
                 "Description of the OTP application"}].

description() -> "Generate an e2 application.".

generate(Prop) ->
  Name = prop:attr(Prop, name),
  AppDirectory = filename:join(["apps", Name]),
  ok = prop:dir(Prop, AppDirectory, [announce]),
  ok = prop:dir(Prop, filename:join([AppDirectory, "src"]), [announce]),
  ok = prop:dir(Prop, filename:join([AppDirectory, "ebin"]), [announce]),
  ok = prop:dir(Prop, filename:join([AppDirectory, "priv"]), [announce]),
  ok.
