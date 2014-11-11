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

atomize(String) -> erlang:list_to_atom(String).

%% Generate something
command(new, [Name | Args]) ->
  GeneratorName = generator_name(re:split(Name, ":")),
  Generator = prop:find_generator(GeneratorName),
  [ResourceName | OptionalArgs] = Args,
  AvailableOptions = Generator:command_line_options(),
  {ok, {Options, _Extra}} = getopt:parse(AvailableOptions, OptionalArgs),
  ExtraOptions = [{name, ResourceName}, {invocation, command_line},
                  {module, Generator}],
  Generator:generate(lists:append([ExtraOptions, Options]));
%% List installed generators
command(list, []) ->
  [info_line(Generator) || Generator <- prop:generators()];
command(Unknown, _Args) ->
  io:format("unknown command ~p~n", [Unknown]).

generator_name(NameArg) when erlang:length(NameArg) == 1 ->
  erlang:list_to_atom(erlang:head(NameArg));
generator_name(NameArgs) ->
  AtomizedNames = [erlang:binary_to_atom(X, utf8) || X <- NameArgs],
  erlang:list_to_tuple(AtomizedNames).

%% Display a description for a generator for the list command
info_line(Generator) ->
  PrettyName = prop:formatted_name(Generator),
  io:format("prop new ~s - ~s~n", [PrettyName, Generator:description()]).

%% TODO(nuex): add usage info
usage() ->
  io:format("usage: prop [command] [generator] [key]~n", []).
