-module(prop_cli).
-export([main/1]).

main([]) -> usage();
main(Args) ->
  [Command | Rest] = Args,
  command(atomize(Command), Rest).

command(new, [Generator, Key]) ->
  prop:generate(atomize(Generator), [{interface, terminal}, {name, Key},
                      {description, "mah " ++ Key ++ " app"}]);
command(Unknown, _Args) ->
  io:format("unknown command ~p~n", [Unknown]).

%% TODO(nuex): add usage info
usage() ->
  io:format("usage: prop [command] [generator] [key]~n", []).

%%
%% PRIVATE FUNCTIONS
%%

atomize(String) -> erlang:list_to_atom(String).
