-module(prop_release).
-behavior(prop).
-export([name/0, description/0, options/0, generate/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

name() -> release.
description() -> "Generate a release project".
options() -> [description_option()].

generate(Release) ->
  Name = prop:name(Release),
  prop:template(Release, "README.md", [Name, "README.md"]),
  App = prop:generator(application, Name ++ "_core"),
  % Configure the app to start in the releases root directory
  prop:generate(configure(Release, App)).

%% ===================================================================
%% Private Functions
%% ===================================================================

configure(Release, App) ->
  Options = [{release, true}],
  InvokedVia = prop:invoked_via(Release),
  Name = prop:name(Release),
  prop:set_root_directory(
    prop:set_options(
      prop:set_invoked_via(App, InvokedVia), Options), Name).

description_option() ->
  {description, $d, "description", {string, "Default description"},
   "Description of the OTP application"}.
