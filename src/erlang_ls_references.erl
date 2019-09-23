-module(erlang_ls_references).

-export([ find_references/2
        , start/0
        ]).

-define(SERVER, references_server).

-spec find_references(erlang_ls_uri:uri(), erlang_ls_poi:poi()) ->
  {ok, [erlang_ls_poi:poi()]}.
find_references(Uri, #{info := {function, {F, A}}}) ->
  M = erlang_ls_uri:module(Uri),
  Query = lists:flatten(io_lib:format("(Lin) (E || ~p)", [{M, F, A}])),
  {ok, Results} = xref:q(?SERVER, Query),
  {ok, lists:flatten([ranges(From, Lines) || {{From, _To}, Lines} <- Results])};
find_references(_Uri, _) ->
  {ok, []}.

-spec start() -> ok.
start() ->
  xref:start(?SERVER),
  xref:set_default(?SERVER, [{verbose, false}, {warnings, false}]),
  [xref:add_directory(?SERVER, Dir)
   || Dir <- erlang_ls_code_navigation:deps_path()],
    [xref:add_directory(?SERVER, Dir)
   || Dir <- erlang_ls_code_navigation:app_path()],
  ok.

%% TODO: Abstract find_file function
-spec ranges(mfa(), [non_neg_integer()]) ->
  [erlang_ls_poi:range()].
ranges({M, _F, _A}, Lines) ->
  %% TODO: Move call to server
  Path = erlang_ls_code_navigation:include_path(),
  FileName = filename(M),
  {ok, IoDevice, FullName} = file:path_open(Path, FileName, [read]),
  file:close(IoDevice),
  [ #{ uri   => erlang_ls_uri:uri(FullName)
     , range => erlang_ls_protocol:range(#{ from => {Line, 0}
                                          , to   => {Line, 0}
                                          })
     }
    || Line <- Lines].

%% TODO: Duplicated from code navigation
-spec filename(atom()) -> binary().
filename(Module) ->
  list_to_binary(atom_to_list(Module) ++ ".erl").
