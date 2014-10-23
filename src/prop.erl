-module(prop).
-export([generate/2, attr/2, chdir/1, dir/1, template/3, template/4]).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{generate, 1}];
behaviour_info(_) -> undefined.

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

%% ===================================================================
%% Behaviour Functions
%% ===================================================================

%% Fetch an attribute from passed in options
attr(Key, Options) -> proplists:get_value(Key, Options).

%% Change directory
chdir(Directory) -> file:set_cwd(Directory).

%% Ensure a directory exists
dir(Directory) -> ensure_directory(filelib:is_dir(Directory), Directory).

%% Render a template
template(Mod, OutputPath, Context) ->
  {ok, Raw} = read_template(Mod, OutputPath),
  {ok, Template} = elk:compile(Raw),
  Rendered = elk:render(Template, {proplist, binary_keys(Context)}),
  file:write_file(OutputPath, Rendered).

%% Render a template with a different output path
template(Mod, TemplatePath, RawOutputPath, Context) ->
  {ok, PathTemplate} = elk:compile(erlang:list_to_binary(RawOutputPath)),
  RenderedPath = elk:render(PathTemplate, {proplist, binary_keys(Context)}),
  {ok, RawFileTemplate} = read_template(Mod, TemplatePath),
  {ok, FileTemplate} = elk:compile(RawFileTemplate),
  Rendered = elk:render(FileTemplate, {proplist, binary_keys(Context)}),
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

generator_path(Mod) ->
  ModuleAttributes = Mod:module_info(attributes),
  [Generator | _Rest] = proplists:get_value(prop, ModuleAttributes),
  ModuleDirectory = filename:dirname(code:which(Mod)),
  filename:join([ModuleDirectory, "..", "priv", "generators",
                 erlang:atom_to_list(Generator)]).

read_template(Mod, RelativeTemplatePath) ->
  Path = filename:join([generator_path(Mod), "templates", 
                        lists:concat([RelativeTemplatePath, ".mustache"])]),
  file:read_file(Path).
