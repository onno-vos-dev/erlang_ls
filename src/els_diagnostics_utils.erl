%%==============================================================================
%% Diagnostics utils
%%==============================================================================
-module(els_diagnostics_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ dependencies/1
        , send_notification/3
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

-spec dependencies(uri()) -> [atom()].
dependencies(Uri) ->
  dependencies([Uri], []).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec dependencies([uri()], [atom()]) -> [atom()].
dependencies([], Acc) ->
  Acc;
dependencies([Uri|Uris], Acc) ->
  {ok, [Document]} = els_dt_document:lookup(Uri),
  Deps = els_dt_document:pois(Document, [behaviour, parse_transform]),
  IncludedUris = included_uris(Document),
  dependencies(Uris ++ IncludedUris, Acc ++ [Id || #{id := Id} <- Deps]).

-spec included_uris(els_dt_document:item()) -> [uri()].
included_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  included_uris([Id || #{id := Id} <- POIs], []).

-spec included_uris([atom()], [uri()]) -> [uri()].
included_uris([], Acc) ->
  lists:usort(Acc);
included_uris([Id|Ids], Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> included_uris(Ids, [Uri | Acc]);
    {error, _Error} -> included_uris(Ids, Acc)
  end.

-spec send_notification(uri(), [diagnostic()], integer()) -> ok.
send_notification(Uri, Diagnostics, Vsn) ->
  Method = <<"textDocument/publishDiagnostics">>,
  Params  = #{ uri         => Uri
             , version     => Vsn
             , diagnostics => Diagnostics
             },
  els_server:send_notification(Method, Params).
