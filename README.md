prop
====

Code scaffolding for Erlang

## Usage:

    prop:generate({otp, release}, [{name, "my_erlang_app"}]).

Or via the command line:

    prop new otp:release my_erlang_app

## Building a Generator:

    % Create a generator module named with a `prop_` prefix and add it to your
    % ERL_LIBS path.
    -module(prop_otp).

    % Give it the prop behavior:
    -behaviour(prop).

    % Make sure the module has the `prop` module attribute
    -prop({otp, release}).

    % Make a task
    generate(Prop) ->
      Name = attr(name, Options),
      prop:dir(Prop, Name, []),
      prop:chdir(TargetDirectory),
      prop:dir(Prop, "src", [announce]),
      prop:dir(Prop, "ebin", [announce]),
      prop:template(Prop, "rebar.config", [announce]),
      prop:template(Prop, "README.md", [announce]),
      prop:exec(Prop, "rebar get-deps"),
      prop:exec(Prop, "rebar compile").

