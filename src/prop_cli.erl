-module(prop_cli).
-export([main/1]).

%% ===================================================================
%% API FUNCTIONS
%% ===================================================================

%% Command line input entry point!
main([]) -> usage();
main(Args) ->
  [Command | Rest] = Args,
  command(atomize(Command), Rest).

%% ===================================================================
%% PRIVATE FUNCTIONS
%% ===================================================================

%% Convert string to an atom
atomize(String) -> erlang:list_to_atom(String).

%% new command: Generate something
command(new, [Name | Args]) ->
  GeneratorName = generator_name(re:split(Name, ":")),
  Generator = prop:find_generator(GeneratorName),
  [ResourceName | OptionalArgs] = Args,
  AvailableOptions = Generator:options(),
  {ok, {Options, _Extra}} = getopt:parse(AvailableOptions, OptionalArgs),
  ExtraOptions = [{name, ResourceName}, {invocation, command_line},
                  {module, Generator}],
  Generator:generate(lists:append([ExtraOptions, Options]));
%% list command: List installed generators
command(list, _Args) ->
  Sorted = lists:sort(prop:generators()),
  Formatted = [name_and_description(Generator) || Generator <- Sorted],
  Lengths = [erlang:length(Name) || {Name, _Generator} <- Formatted],
  case Lengths of
    [] -> io:format("No generators were found.~n", []);
    Lengths ->
      Widest = lists:max(Lengths),
      Padded = [pad(Pair, Widest) || Pair <- Formatted],
      [info_line(Line) || Line <- Padded]
  end;
%% no input: Show usage
command(Unknown, _Args) ->
  io:format("unknown command ~p~n", [Unknown]).

%% Convert the generator name to a string
formatted_name(Generator) ->
  NameParts = erlang:tuple_to_list(Generator:name()),
  StringifiedParts = [erlang:atom_to_list(NamePart) || NamePart <- NameParts],
  string:join(StringifiedParts, ":").

%% Get the tuple version of the generator's name
generator_name(NameArg) when erlang:length(NameArg) == 1 ->
  erlang:list_to_atom(erlang:head(NameArg));
generator_name(NameArgs) ->
  AtomizedNames = [erlang:binary_to_atom(X, utf8) || X <- NameArgs],
  erlang:list_to_tuple(AtomizedNames).

%% Display a description for a generator for the list command
info_line(Line) -> io:format("prop new ~s~n", [Line]).

%% Return a tuple with the formatted name and description of the given
%% generator
name_and_description(Generator) ->
  {formatted_name(Generator), Generator:description()}.

%% Add padding to make output tidy
pad({Name, Description}, Widest) ->
  ExtraSpace = 5,
  Length = (Widest - erlang:length(Name)) + ExtraSpace,
  string:join([Name, " - " ++ Description], string:copies(" ", Length)).

%% TODO(nuex): add usage info
usage() ->
  io:format("usage: prop [command] [generator] [key]~n", []).
