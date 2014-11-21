-module(prop).
-export([attr/2, chdir/1, destination/2, dir/3, exec/3, find_generator/1,
         generate/2, generators/0, template/3, template/4]).

-callback generate(list()) -> any().
-callback name() -> atom().
-callback description() -> string().
-callback options() -> list().

%% ===================================================================
%% API Functions
%% ===================================================================

find_generator(Generator) ->
  case [Mod || Mod <- generators(), Mod:name() == Generator] of
    [] ->
      io:format("Unable to find generator ~p.~n", [Generator]),
      erlang:error(generator_not_found);
    Found -> erlang:hd(Found)
  end.

generate(Generator, Options) ->
  Mod = find_generator(Generator),
  Mod:generate(Options).

generators() ->
  ensure_generators_loaded(),
  [Mod || {Mod, _Path} <- code:all_loaded(), is_prop_generator(Mod)].

%% ===================================================================
%% Behaviour Functions
%% ===================================================================

%% Fetch an attribute from passed in options
attr(Prop, Key) -> proplists:get_value(Key, Prop).

%% Create the destination directory and change to it
destination(Prop, Directory) ->
  dir(Prop, Directory, [{announce, false}]),
  chdir(Directory).

%% Change directory
chdir(Directory) -> file:set_cwd(Directory).

%% Ensure a directory exists
dir(Prop, Directory, Options) ->
  Invocation = proplists:get_value(invocation, Prop),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  Announcing = is_announcing(Options),
  announce(Announcing, Invocation, {CliMessage, [Directory]}),
  ensure_directory(filelib:is_dir(Directory), Directory).

%% Run a command
exec(Prop, Command, Options) ->
  Invocation = proplists:get_value(invocation, Prop),
  CliMessage = "  " ++ color(success) ++ "  exec" ++ color_reset() ++ " ~s~n",
  Announcing = is_announcing(Options),
  announce(Announcing, Invocation, {CliMessage, [Command]}),
  Result = os:cmd(Command),
  io:format(Result, []),
  ok.

%% Render a template
template(Prop, TemplatePath, RawOutputPath) ->
  template(Prop, TemplatePath, RawOutputPath, []).

%% Render a template with a different output path
template(Prop, TemplatePath, RawOutputPath, Options) ->
  Mod = proplists:get_value(module, Prop),
  Invocation = proplists:get_value(invocation, Prop),
  {ok, PathTemplate} = elk:compile(erlang:list_to_binary(RawOutputPath)),
  RenderedPath = elk:render(PathTemplate, {proplist, binary_keys(Prop)}),
  RenderedPathForAnnounce = erlang:binary_to_list(RenderedPath),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  Announcing = is_announcing(Options),
  announce(Announcing, Invocation, {CliMessage, [RenderedPathForAnnounce]}),
  {ok, RawFileTemplate} = read_template(Mod, TemplatePath),
  {ok, FileTemplate} = elk:compile(RawFileTemplate),
  Rendered = elk:render(FileTemplate, {proplist, binary_keys(Prop)}),
  file:write_file(RenderedPath, Rendered).
  
%% ===================================================================
%% Private Functions
%% ===================================================================

announce(true, command_line, {Message, Options}) ->
  io:format(Message, Options);
announce(true, _Invocation, _Options) -> ok;
announce(false, _Invocation, _Options) -> ok.

%% Make all keys binaries
binary_keys(Context) ->
  [{erlang:list_to_binary(erlang:atom_to_list(Key)), Value} ||
      {Key, Value} <- Context].

%% Return the color for the given type
color(success) -> color_foreground(green).

%% Return the color code for the given color
color_foreground(green) -> "\e[32m".

%% Return a color reset code
color_reset() -> "\e[0m".

%% Make a directory if it doesn't exist
ensure_directory(true, _Directory) -> ok;
ensure_directory(false, Directory) -> file:make_dir(Directory).

%% Load modules related to prop
ensure_generators_loaded() ->
  [ensure_loaded(F) || P <- code:get_path(),
                       F <- filelib:wildcard(P ++ "/prop_*.beam")].

ensure_loaded(File) ->
  Path = erlang:list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Path).

generator_name_to_path(Name) when erlang:is_atom(Name) ->
  erlang:atom_to_list(Name);
generator_name_to_path(Name) when erlang:is_tuple(Name) ->
  filename:join(erlang:tuple_to_list(Name)).

generator_path(Generator) ->
  Name = Generator:name(),
  GeneratoruleDirectory = filename:dirname(code:which(Generator)),
  filename:join([GeneratoruleDirectory, "..", "priv", "generators",
                 generator_name_to_path(Name)]).

is_announcing(Options) when is_list(Options) ->
  is_announcing(proplists:get_value(announce, Options));
is_announcing(undefined) -> true;
is_announcing(Boolean) -> Boolean.

is_generator(Mod) ->
  Exports = Mod:module_info(exports),
  lists:member(name, proplists:get_keys(Exports)).

is_prop_generator(Mod) -> is_prop_module(Mod) and is_generator(Mod).

is_prop_module(Module) ->
  Name = erlang:atom_to_list(Module),
  re:run(Name, "^prop_") /= nomatch.

read_template(Mod, RelativeTemplatePath) ->
  Path = filename:join([generator_path(Mod), "templates", 
                        lists:concat([RelativeTemplatePath, ".mustache"])]),
  file:read_file(Path).

