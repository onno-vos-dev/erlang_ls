-module(els_formatting_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , is_enabled_document/0
        , is_enabled_range/0
        , is_enabled_on_type/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(DEFAULT_SUB_INDENT, 2).

%%==============================================================================
%% els_provider functions
%%==============================================================================

%% Keep the behaviour happy
-spec is_enabled() -> boolean().
is_enabled() -> is_enabled_document().

-spec is_enabled_document() -> boolean().
is_enabled_document() -> true.

-spec is_enabled_range() -> boolean().
is_enabled_range() ->
  true.

%% NOTE: because erlang_ls does not send incremental document changes
%%       via `textDocument/didChange`, this kind of formatting does not
%%       make sense.
-spec is_enabled_on_type() -> document_ontypeformatting_options().
is_enabled_on_type() ->
  #{ <<"firstTriggerCharacter">> => <<".">>
   , <<"moreTriggerCharacter">> => [<<"\t">>]
   }.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_formatting, Params}, State) ->
  #{ <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  Path = els_uri:path(Uri),
  case els_utils:project_relative(Uri) of
    {error, not_relative} ->
      {[], State};
    RelativePath ->
      case els_config:get(bsp_enabled) of
        true ->
          {format_document_bsp(Path, RelativePath, Options), State};
        false ->
          {format_document_local(Path, RelativePath, Options), State}
      end
  end;
handle_request({document_rangeformatting, Params}, State) ->
  #{ <<"range">>     := #{ <<"start">> := StartPos
                         , <<"end">>   := EndPos
                         }
   , <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  Range = #{ start => StartPos, 'end' => EndPos },
  {ok, Document} = els_utils:lookup_document(Uri),
  case rangeformat_document(Uri, Document, Range, Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end;
handle_request({document_ontypeformatting, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"ch">>           := Char
   , <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  case ontypeformat_document(Uri, Document, Line + 1, Character + 1, Char
                            , Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec format_document_bsp(binary(), string(), formatting_options()) ->
        [text_edit()].
format_document_bsp(Path, RelativePath, _Options) ->
  Fun = fun(Dir) ->
            Params = #{ <<"output">>  => els_utils:to_binary(Dir)
                      , <<"file">> => els_utils:to_binary(RelativePath)
                      },
            els_bsp_client:request(<<"custom/format">>, Params),
            OutFile = filename:join(Dir, RelativePath),
            els_text_edit:diff_files(Path, OutFile)
        end,
  tempdir:mktmp(Fun).

-spec format_document_local(binary(), string(), formatting_options()) ->
        [text_edit()].
format_document_local(Path, RelativePath,
                      #{ <<"insertSpaces">> := InsertSpaces
                       , <<"tabSize">> := TabSize } = Options) ->
  SubIndent = maps:get(<<"subIndent">>, Options, ?DEFAULT_SUB_INDENT),
  Opts0 = #{ remove_tabs => InsertSpaces
           , break_indent => TabSize
           , sub_indent => SubIndent
           },
  Fun = fun(Dir) ->
            Opts = Opts0#{output_dir => Dir},
            Formatter = rebar3_formatter:new(default_formatter, Opts, unused),
            rebar3_formatter:format_file(RelativePath, Formatter),
            OutFile = filename:join(Dir, RelativePath),
            els_text_edit:diff_files(Path, OutFile)
        end,
  tempdir:mktmp(Fun).

-spec rangeformat_document(uri(), map(), range(), formatting_options())
                          -> {ok, [text_edit()]}.
rangeformat_document(_Uri, _Document, _Range, _Options) ->
    {ok, []}.

-spec ontypeformat_document(binary(), map()
                           , number(), number(), string(), formatting_options())
                           -> {ok, [text_edit()]}.
ontypeformat_document(_Uri, Document, Line, Col, <<".">>, _Options) ->
  %% _Range = #{ start => #{ line => Line, character => Col }, 'end' => #{ line => Line + 1, character => 0 }},
  %% _NewText = <<"">>,
  ?LOG_DEBUG("Uri: ~p~n _Document: ~p~n", [_Uri, Document]),
  Pois = els_dt_document:pois(Document),
  case lists:filter(fun(#{kind := folding_range}) -> true; (_) -> false end, Pois) of
    [] -> {ok, []};
    FRP ->
      case lists:filter(fun(#{range := #{from := {FromLine, _}, to := {ToLine, _}}}) ->
                     Line >= FromLine andalso Line =< ToLine
                   end, FRP) of
        [] -> {ok, []};
        [R] ->
          {StartLine, _} = maps:get(id, R),
          RangeText = els_text:range(maps:get(text, Document), maps:get(id, R), {Line, Col}),
          {ok, FD, FileName} = write_to_tmp_file(RangeText),
          {ok, [Form]} = els_dodger:parse(FD),
          NewText = default_formatter:format(Form, [0], #{break_indent => 2}),
          ok = file:close(FD),
          ok = file:delete(FileName),
          {ok, [#{ range => #{ start => #{line => StartLine - 1, character => 0},
                               'end' => #{ line => Line - 1, character => Col}},
                   'newText' => list_to_binary(NewText)
                 }]}
      end
  end;
ontypeformat_document(_Uri, _Document, _Line, _Col, _Char, _Options) ->
  {ok, []}.

-spec write_to_tmp_file(any()) -> {ok, any(), any()}.
write_to_tmp_file(Text) ->
  {A, B, C} =
    {erlang:unique_integer([positive]),
     erlang:unique_integer([positive]),
     erlang:unique_integer([positive])},
  N = node(),
  FileName =
    filename:join("/tmp",
                  lists:flatten(
                    io_lib:format("~p-~p.~p.~p", [N, A, B, C]))),
  {ok, FD} = file:open(FileName, [read, write]),
  file:write(FD, Text),
  file:position(FD, 0),
  {ok, FD, FileName}.
