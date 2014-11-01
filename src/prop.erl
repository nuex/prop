-module(prop).
-export([generate/2, attr/2, chdir/1, dir/3, template/3, template/4,
         find_generator/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{generate, 1}];
behaviour_info(_) -> undefined.

generate(Generator, Options) ->
  Mod = find_generator(Generator),
  Mod:generate(Options).

find_generator(Generator) ->
  ensure_generators_loaded(),
  Modules = [Mod || {Mod, _Path} <- code:all_loaded(),
                                    is_prop_module(Mod),
                                    is_generator(Mod, Generator)],
  erlang:hd(Modules).

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
  Announcing = lists:member(announce, Options),
  maybe_announce(Announcing, Invocation, {"create ~p~n", [Directory]}),
  ensure_directory(filelib:is_dir(Directory), Directory).

%% Render a template
template(Prop, OutputPath, Options) ->
  Mod = proplists:get_value(module, Prop),
  Invocation = proplists:get_value(invocation, Prop),
  Announcing = lists:member(announce, Options),
  maybe_announce(Announcing, Invocation, {"create ~p~n", [OutputPath]}),
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
  maybe_announce(Announcing, Invocation,
                 {"create ~p~n", [RenderedPathForAnnounce]}),
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

%% Make a directory if it doesn't exist
ensure_directory(true, _Directory) -> ok;
ensure_directory(false, Directory) -> file:make_dir(Directory).

generator_name_to_path(Name) when erlang:is_atom(Name) ->
  erlang:atom_to_list(Name);
generator_name_to_path(Name) when erlang:is_tuple(Name) ->
  filename:join(erlang:tuple_to_list(Name)).

generator_path(Mod) ->
  ModuleDirectory = filename:dirname(code:which(Mod)),
  ModuleAttributes = Mod:module_info(attributes),
  [Name | _Rest] = proplists:get_value(prop, ModuleAttributes),
  filename:join([ModuleDirectory, "..", "priv", "generators",
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

is_generator(Mod, Generator) ->
  GeneratorName = proplists:get_value(prop, Mod:module_info(attributes)),
  has_prop_attribute(Generator, GeneratorName).

has_prop_attribute(_Generator, undefined) -> false;
has_prop_attribute(Generator, Value) -> (erlang:hd(Value) == Generator).

is_prop_module(Module) ->
  Name = erlang:atom_to_list(Module),
  (re:run(Name, "^prop_") /= nomatch).

maybe_announce(true, command_line, {Message, Options}) ->
  io:format(Message, Options);
maybe_announce(_Announcing, _Invocation, _MessageAndOptions) -> ok. 
