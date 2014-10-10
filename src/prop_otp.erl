-module(prop_otp).
-export([generate/1]).
-prop(otp).

%% ===================================================================
%% API
%% ===================================================================

%% Generate an 
generate(Options) ->
  OutputDirectory = attr(output_directory, Options),
  Name = attr(name, Options),
  TargetDirectory = filename:join([OutputDirectory, Name]),
  dir(TargetDirectory),
  chdir(TargetDirectory),
  dir("config"),
  dir("apps"),
  dir("priv"),
  dir("ebin"),
  template("README.md", Options).

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
template(Name, Context) ->
  ModuleAttributes = ?MODULE:module_info(attributes),
  [Generator | _Rest] = proplists:get_value(prop, ModuleAttributes),
  ModuleDirectory = filename:dirname(code:which(?MODULE)),
  Path = filename:join([ModuleDirectory, "..", "priv",
                        "templates", erlang:atom_to_list(Generator),
                        lists:concat([Name, ".mustache"])]),
  {ok, Raw} = file:read_file(Path),
  {ok, Template} = elk:compile(Raw),
  Rendered = elk:render(Template, {proplist, binary_keys(Context)}),
  file:write_file(Name, Rendered).

%% ===================================================================
%% Private Functions
%% ===================================================================

%% Make all keys binaries
binary_keys(Context) ->
  [{erlang:list_to_binary(erlang:atom_to_list(Key)), Value} ||
      {Key, Value} <- Context ].

%% Make a directory if it doesn't exist
ensure_directory(true, _Directory) -> ok;
ensure_directory(false, Directory) -> file:make_dir(Directory).
