-module(prop).
-export([attr/2, chdir/1, dir/3, exec/3, find_generator/1, generate/2,
         generators/0, template/3, template/4]).

-callback generate(list()) -> any().
-callback name() -> atom().
-callback description() -> string().
-callback options() -> list().

%% ===================================================================
%% API Functions
%% ===================================================================

find_generator(Generator) ->
  Found = [Mod || Mod <- generators(), Mod:name() == Generator],
  erlang:hd(Found).

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

%% Change directory
chdir(Directory) -> file:set_cwd(Directory).

%% Ensure a directory exists
dir(Prop, Directory, Options) ->
  Invocation = proplists:get_value(invocation, Prop),
  Announcing = proplists:get_value(announce, Options),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  announce(Announcing, Invocation, {CliMessage, [Directory]}),
  ensure_directory(filelib:is_dir(Directory), Directory).

%% Run a command
exec(Prop, Command, Options) ->
  Invocation = proplists:get_value(invocation, Prop),
  Announcing = lists:member(announce, Options),
  CliMessage = "  " ++ color(success) ++ "  exec" ++ color_reset() ++ " ~s~n",
  announce(Announcing, Invocation, {CliMessage, [Command]}),
  Result = os:cmd(Command),
  io:format(Result, []),
  ok.

%% Render a template
template(Prop, OutputPath, Options) ->
  Mod = proplists:get_value(module, Prop),
  Invocation = proplists:get_value(invocation, Prop),
  Announcing = lists:member(announce, Options),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  announce(Announcing, Invocation, {CliMessage, [OutputPath]}),
  {ok, Raw} = read_template(Mod, OutputPath),
  {ok, Template} = elk:compile(Raw),
  Rendered = elk:render(Template, {proplist, binary_keys(Prop)}),
  file:write_file(OutputPath, Rendered).

%% Render a template with a different output path
template(Prop, TemplatePath, RawOutputPath, Options) ->
  Mod = proplists:get_value(module, Prop),
  Invocation = proplists:get_value(invocation, Prop),
  Announcing = lists:member(announce, Options),
  {ok, PathTemplate} = elk:compile(erlang:list_to_binary(RawOutputPath)),
  RenderedPath = elk:render(PathTemplate, {proplist, binary_keys(Prop)}),
  RenderedPathForAnnounce = erlang:binary_to_list(RenderedPath),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  announce(Announcing, Invocation, {CliMessage, [RenderedPathForAnnounce]}),
  {ok, RawFileTemplate} = read_template(Mod, TemplatePath),
  {ok, FileTemplate} = elk:compile(RawFileTemplate),
  Rendered = elk:render(FileTemplate, {proplist, binary_keys(Prop)}),
  file:write_file(RenderedPath, Rendered).
  
%% ===================================================================
%% Private Functions
%% ===================================================================

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

generator_name_to_path(Name) when erlang:is_atom(Name) ->
  erlang:atom_to_list(Name);
generator_name_to_path(Name) when erlang:is_tuple(Name) ->
  filename:join(erlang:tuple_to_list(Name)).

generator_path(Generator) ->
  Name = Generator:name(),
  GeneratoruleDirectory = filename:dirname(code:which(Generator)),
  filename:join([GeneratoruleDirectory, "..", "priv", "generators",
                 generator_name_to_path(Name)]).

read_template(Mod, RelativeTemplatePath) ->
  Path = filename:join([generator_path(Mod), "templates", 
                        lists:concat([RelativeTemplatePath, ".mustache"])]),
  file:read_file(Path).

%% Load modules related to prop
ensure_generators_loaded() ->
  [ensure_loaded(F) || P <- code:get_path(),
                       F <- filelib:wildcard(P ++ "/prop_*.beam")].

ensure_loaded(File) ->
  Path = erlang:list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Path).

is_generator(Mod) ->
  Attributes = Mod:module_info(attributes),
  lists:member(prop, proplists:get_keys(Attributes)).

is_prop_generator(Mod) -> is_prop_module(Mod) and is_generator(Mod).

is_prop_module(Module) ->
  Name = erlang:atom_to_list(Module),
  re:run(Name, "^prop_") /= nomatch.

announce(true, command_line, {Message, Options}) ->
  io:format(Message, Options);
announce(true, _Invocation, _Options) -> ok;
announce(false, _Invocation, _Options) -> ok.
