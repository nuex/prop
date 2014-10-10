prop
====

Code scaffolding for Erlang

## Usage:

    prop:generate(otp, [{name, "my_erlang_app"},
                        {path, "/home/user/my_erlang_app"}]).

## Building a Generator:

    % Create a generator module named with a `prop_` prefix and add it to your
    % ERL_LIBS path.
    -module(prop_otp).

    % Make sure the module has the `prop` module attribute
    -prop(otp).

    % You can also make sub generators by specifying a tuple with the name of
    % the parent as the first element
    % -prop({otp, gen_server}).

    % Give the module generator functions
    -behaviour(prop_generator).

    % Expose option defaults and requirements
    % Defaults can be overriden by specifying a /etc/prop, ~/.prop, or
    % .prop file
    options() ->
      [{rebar, true}, {relx, true}, {makefile, true}].

    % Make a task
    task(Options) ->
      dir("src"),
      dir("ebin"),
      maybe_template(rebar, "rebar.config", Options),
      maybe_template(relx, "relx.config", Options),
      maybe_template(makefile, "Makefile", Options),
      template("README.md", Options).
