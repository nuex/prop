-module(prop).
-export([exec/3, generate/1, generator/2, generators/0, invoked_via/1, module/1,
         name/1, options/1, root_directory/1, set_invoked_via/2, set_options/2,
         set_root_directory/2, template/3, template/4]).

-record(prop, {generator, name, module, invoked_via, options, root_directory}).

%% ===================================================================
%% API Functions
%% ===================================================================

generate(#prop{module=Mod}=Prop) -> Mod:generate(Prop).

generator(Generator, Name) ->
  {ok, Module} = find_generator(Generator),
  #prop{generator=Generator, name=Name, module=Module}.

generators() ->
  ensure_generators_loaded(),
  [Mod || {Mod, _Path} <- code:all_loaded(), is_prop_generator(Mod)].

invoked_via(#prop{invoked_via=InvokedVia}=_Prop) -> InvokedVia.

module(#prop{module=Module}=_Prop) -> Module.

name(#prop{name=Name}=_Prop) -> Name.

options(#prop{options=Options}=_Prop) -> Options.

root_directory(#prop{root_directory=Directory}=_Prop) -> Directory.

set_invoked_via(Prop, Type) -> Prop#prop{invoked_via=Type}.

set_options(Prop, Options) -> Prop#prop{options=Options}.

set_root_directory(Prop, Directory) -> Prop#prop{root_directory=Directory}.

%% Run a command
exec(#prop{invoked_via=InvokedVia}=_Prop, Command, Options) ->
  CliMessage = "  " ++ color(success) ++ "  exec" ++ color_reset() ++ " ~s~n",
  Announcing = is_announcing(Options),
  announce(Announcing, InvokedVia, {CliMessage, [Command]}),
  Result = os:cmd(Command),
  io:format(Result, []),
  ok.

%% Render a template
template(Prop, Template, Output) ->
  template(Prop, Template, Output, []).

%% Render a template with a different output path
template(#prop{module=Mod, invoked_via=InvokedVia}=Prop, Template, Output,
         Options) ->
  % Check if OutputPath is a list that needs to be converted to a directory
  OutputPath = case io_lib:printable_list(Output) of
    true -> Output;
    false -> filename:join(Output)
  end,
  {ok, PathTemplate} = elk:compile(erlang:list_to_binary(OutputPath)),
  Context = binary_keys(render_context(Prop)),
  RenderedPath = elk:render(PathTemplate, {proplist, Context}),
  filelib:ensure_dir(erlang:binary_to_list(RenderedPath)),
  RenderedPathForAnnounce = erlang:binary_to_list(RenderedPath),
  CliMessage = "  " ++ color(success) ++ "create" ++ color_reset() ++ " ~s~n",
  Announcing = is_announcing(Options),
  announce(Announcing, InvokedVia, {CliMessage, [RenderedPathForAnnounce]}),
  {ok, RawFileTemplate} = read_template(Mod, Template),
  {ok, FileTemplate} = elk:compile(RawFileTemplate),
  Rendered = elk:render(FileTemplate, {proplist, Context}),
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

%% Load modules related to prop
ensure_generators_loaded() ->
  [ensure_loaded(F) || P <- code:get_path(),
                       F <- filelib:wildcard(P ++ "/prop_*.beam")].

ensure_loaded(File) ->
  Path = erlang:list_to_atom(filename:rootname(filename:basename(File))),
  code:ensure_loaded(Path).

find_generator(Generator) ->
  case [Mod || Mod <- generators(), Mod:name() == Generator] of
    [] -> {error, not_found};
    Found -> {ok, erlang:hd(Found)}
  end.

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

read_template(Mod, RelativeTemplate) ->
  Path = filename:join([generator_path(Mod), "templates", 
                        lists:concat([RelativeTemplate, ".mustache"])]),
  file:read_file(Path).

render_context(#prop{options=Options}=Prop) ->
  lists:append(Options, [{name, name(Prop)}]).
