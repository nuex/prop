-module(prop).
-export([generate/2]).

generate(Generator, Options) ->
  Mod = find_generator(Generator),
  Mod:generate(Options).

%% Load modules related to prop
ensure_generators_loaded() ->
  [ensure_loaded(F) || P <- code:get_path(),
                       F <- filelib:wildcard(P ++ "/prop_*.beam")].

ensure_loaded(File) ->
  Path = erlang:list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Path).

find_generator(Generator) ->
  ensure_generators_loaded(),
  Modules = [Mod || {Mod, _Path} <- code:all_loaded(),
                                    is_prop_module(Mod),
                                    is_generator(Mod, Generator)],
  erlang:hd(Modules).

is_generator(Mod, Generator) ->
  Value = proplists:get_value(prop, Mod:module_info(attributes)),
  case Value of
    undefined -> false;
    Value -> (erlang:hd(Value) == Generator)
  end.

is_prop_module(Module) ->
  Name = erlang:atom_to_list(Module),
  (re:run(Name, "^prop_") /= nomatch).
