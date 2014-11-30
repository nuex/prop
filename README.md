prop
====

Code scaffolding for Erlang

## NOTE

prop is currently alpha, so something could break or some functionality could 
be missing.

## Usage:

    Prop = prop:generator(release, "my_erlang_app"),
    prop:generate(Prop).

Or via the command line:

    prop new release my_erlang_app

## Building a Generator:

    % Create a generator module named with a `prop_` prefix and add it to your
    % ERL_LIBS path.
    -module(prop_release).
    -export([name/0, description/0, options/0, generate/1]).

    % Give it a name
    name() -> release.

    % A description
    description() -> "Generate an OTP release".

    % And make it handle getopt style options on the command line
    % (See Option Spec at https://github.com/jcomellas/getopt)
    options() -> [description_option()].

    description_option() ->
      {description, $d, "description", {string, "Default description"},
       "Description of the OTP application"}.

    % Set up the generate task
    generate(Release) ->
      Name = prop:name(Release),
      prop:template(Release, "README.md", [Name, "README.md"]),
      App = prop:generator(application, Name ++ "_core"),
      % Configure the app to start in the releases root directory
      prop:generate(configure(Prop, App)).

    configure(Prop, App) ->
      Options = [{release, true}],
      InvokedVia = prop:invoked_via(Prop),
      Name = prop:name(Prop),
      prop:set_root_directory(
        prop:set_options(
          prop:set_invoked_via(App, InvokedVia), Options), Name).

## Included Generators

*release* and *application* are included generators. Currently, generated 
code uses the OTP framework, but e2 is planned to be a configurable default 
in the future.

*release* creates a base releas project and generates an application.

*application* creates an OTP application skeleton
