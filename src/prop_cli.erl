-module(prop_cli).
-export([main/1]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% Command line input entry point!
main([]) -> usage();
main(Args) ->
  [Command | Rest] = Args,
  command(atomize(Command), Rest).

%% ===================================================================
%% Private Functions
%% ===================================================================

%% Convert string to an atom
atomize(String) -> erlang:list_to_atom(String).

%% new command: Generate something
command(new, [RawName | Args]) ->
  Name = generator_name(re:split(RawName, ":")),
  [Resource | OptionArguments] = Args,
  Prop = prop:generator(Name, Resource),
  Module = prop:module(Prop),
  AvailableOptions = Module:options(),
  {ok, {Options, _Extra}} = getopt:parse(AvailableOptions, OptionArguments),
  prop:generate(
    prop:set_options( 
      prop:set_invoked_via(Prop, command_line), Options));
%% list command: List installed generators
command(list, _Args) ->
  case prop:generators() of
    [] -> io:format("No generators were found.~n", []);
    Generators ->
      Sorted = lists:sort(Generators),
      Formatted = [name_and_description(Generator) || Generator <- Sorted],
      Lengths = [erlang:length(Name) || {Name, _Generator} <- Formatted],
      Widest = lists:max(Lengths),
      Padded = [pad(Pair, Widest) || Pair <- Formatted],
      [info_line(Line) || Line <- Padded]
  end;
%% no input: Show usage
command(Unknown, _Args) ->
  io:format("unknown command ~p~n", [Unknown]).

%% Convert the generator name to a string
formatted_name(Name) when is_atom(Name) -> erlang:atom_to_list(Name);
formatted_name(Name) when is_tuple(Name) ->
  NameParts = erlang:tuple_to_list(Name),
  StringifiedParts = [erlang:atom_to_list(NamePart) || NamePart <- NameParts],
  string:join(StringifiedParts, ":").

%% Get the tuple version of the generator's name
generator_name(NameArg) when erlang:length(NameArg) == 1 ->
  erlang:list_to_atom(erlang:binary_to_list(erlang:hd(NameArg)));
generator_name(NameArgs) ->
  AtomizedNames = [erlang:binary_to_atom(X, utf8) || X <- NameArgs],
  erlang:list_to_tuple(AtomizedNames).

%% Display a description for a generator for the list command
info_line(Line) -> io:format("prop new ~s~n", [Line]).

%% Return a tuple with the formatted name and description of the given
%% generator
name_and_description(Generator) ->
  {formatted_name(Generator:name()), Generator:description()}.

%% Add padding to make output tidy
pad({Name, Description}, Widest) ->
  ExtraSpace = 5,
  Length = (Widest - erlang:length(Name)) + ExtraSpace,
  string:join([Name, " - " ++ Description], string:copies(" ", Length)).

%% TODO(nuex): add usage info
usage() ->
  io:format("usage: prop [command] [generator] [key]~n", []).
