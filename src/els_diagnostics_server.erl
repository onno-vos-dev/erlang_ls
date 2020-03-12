%%==============================================================================
%% Diagnostics Server
%%==============================================================================
-module(els_diagnostics_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start_link/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%% API
-export([ on_save/1
        , on_open/1
        , on_close/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { uri :: uri()
               , version :: non_neg_integer()
               , diagnostics :: map()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(uri()) -> {ok, pid()}.
start_link(Uri) ->
  {ok, _} = gen_server:start_link(?MODULE, Uri, []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(uri()) -> {ok, state()}.
init(Uri) ->
  State = #state{uri = Uri
                , diagnostics = new_diagnostics()
                , version = 0
                },
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(run_diagnostics, _From, #state{ uri = Uri
                                          , diagnostics = Diagnostics} = S) ->
  {reply, ok, S#state{ diagnostics = run_diagnostics(Uri, Diagnostics)}};
handle_call(UnknownMessage, _From, S) ->
  lager:info("Unknown handle_call! message: ~p", [UnknownMessage]),
  {reply, ok, S}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(run_diagnostics, #state{ uri = Uri
                                   , diagnostics = Diagnostics} = S) ->
  {noreply, S#state{ diagnostics = run_diagnostics(Uri, Diagnostics)}};
handle_cast(UnknownMessage, S) ->
  lager:info("Unknown handle_cast! message: ~p", [UnknownMessage]),
  {noreply, S}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'EXIT', _, normal}, S) ->
  {noreply, S};
handle_info(UnknownMessage, S) ->
  lager:info("Unknown handle_info! message: ~p", [UnknownMessage]),
  {noreply, S}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec on_save(uri()) -> ok.
on_save(Uri) ->
  {ok, Pid} = els_diagnostics_sup:lookup_pid(Uri),
  ok = gen_server:call(Pid, run_diagnostics, infinity),
  ok.

-spec on_open(uri()) -> ok.
on_open(Uri) ->
  els_diagnostics_sup:start_server(Uri),
  {ok, Pid} = els_diagnostics_sup:lookup_pid(Uri),
  ok = gen_server:cast(Pid, run_diagnostics),
  ok.

-spec on_close(uri()) -> ok.
on_close(Uri) ->
  els_diagnostics_sup:stop_server(Uri).

-spec run_diagnostics(uri(), map()) -> {map(), integer()}.
run_diagnostics(Uri, Diagnostics) ->
  lists:foldl(
    fun(DiagnosticsMod, DiagnosticsAcc) ->
        DResult = diagnostic(DiagnosticsMod, Uri),
        NewDiagnostics = maps:update(DiagnosticsMod, DResult, DiagnosticsAcc),
        els_diagnostics_utils:send_notification(Uri, merge(NewDiagnostics)),
        NewDiagnostics
    end, Diagnostics, [ els_compiler_diagnostics
                      , els_elvis_diagnostics
                      , els_dialyzer_diagnostics
                      ]).

-spec merge(map()) -> [diagnostic()].
merge(Diagnostics) ->
  lists:flatten(maps:values(Diagnostics)).

-spec diagnostic(atom(), uri()) -> [diagnostic()].
diagnostic(els_compiler_diagnostics, Uri) ->
  CDiagnostics = els_compiler_diagnostics:diagnostics(Uri),
  maybe_compile_and_load(Uri, CDiagnostics),
  CDiagnostics;
diagnostic(Module, Uri) ->
  apply(Module, diagnostics, [Uri]).

-spec maybe_compile_and_load(uri(), [diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CompilerDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := NodeStr} ->
      Node = list_to_atom(NodeStr),
      Module = els_uri:module(Uri),
      case rpc:call(Node, code, is_sticky, [Module]) of
        true -> ok;
        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
      end;
    disabled ->
      ok
  end;
maybe_compile_and_load(_, _) -> ok.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
  Msg = io_lib:format("code_reload success for: ~s", [Module]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => list_to_binary(Msg)
                                });
handle_rpc_result(Err, Module) ->
  lager:info("[code_reload] code_reload using c:c/1 crashed with: ~p",
             [Err]),
  Msg = io_lib:format("code_reload swap crashed for: ~s with: ~w",
                      [Module, Err]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_ERROR,
                                  message => list_to_binary(Msg)
                                }).

-spec new_diagnostics() -> map().
new_diagnostics() ->
  #{ els_compiler_diagnostics => []
   , els_dialyzer_diagnostics => []
   , els_elvis_diagnostics    => []
   }.
