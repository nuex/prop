-module(prop_cli).
-export([main/1]).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

main([]) -> usage();
main(Args) ->
  [Command | Rest] = Args,
  command(atomize(Command), Rest).

%% ===================================================================
%% PRIVATE FUNCTIONS
%% ===================================================================

command(new, [Name | Args]) ->
  GeneratorName = generator_name(re:split(Name, ":")),
  Generator = prop:find_generator(GeneratorName),
  [ResourceName | OptionalArgs] = Args,
  AvailableOptions = Generator:command_line_options(),
  {ok, {Options, _Extra}} = getopt:parse(AvailableOptions, OptionalArgs),
  Generator:generate([{name, ResourceName} | Options]);
command(Unknown, _Args) ->
  io:format("unknown command ~p~n", [Unknown]).

generator_name(NameArg) when erlang:length(NameArg) == 1 ->
  erlang:list_to_atom(erlang:head(NameArg));
generator_name(NameArgs) ->
  AtomizedNames = [erlang:binary_to_atom(X, utf8) || X <- NameArgs],
  erlang:list_to_tuple(AtomizedNames).

%% TODO(nuex): add usage info
usage() ->
  io:format("usage: prop [command] [generator] [key]~n", []).

%%
%% PRIVATE FUNCTIONS
%%

atomize(String) -> erlang:list_to_atom(String).
