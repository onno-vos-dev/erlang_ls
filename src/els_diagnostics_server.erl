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
-define(COMPILER, els_compiler_diagnostics).
-define(ELVIS, els_elvis_diagnostics).
-define(DIALYZER, els_dialyzer_diagnostics).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { uri :: uri()
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
                , diagnostics = #{ ?COMPILER => []
                                 , ?ELVIS    => []
                                 , ?DIALYZER => []
                                 }
                },
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({Type, Diagnostic, update},
            _From,
            #state{ diagnostics = Diagnostics } = State) ->
  {reply, ok, State#state{ diagnostics = Diagnostics#{ Type => Diagnostic }}};
handle_call(send_notification,
            _From,
            #state{ uri = Uri, diagnostics = Diagnostics } = State) ->
  els_diagnostics_utils:send_notification(Uri, merge(Diagnostics)),
  {reply, ok, State};
handle_call(UnknownMessage, _From, S) ->
  lager:info("Unknown handle_call! message: ~p", [UnknownMessage]),
  {reply, ok, S}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(UnknownMessage, S) ->
  lager:info("Unknown handle_cast! message: ~p", [UnknownMessage]),
  {noreply, S}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(UnknownMessage, S) ->
  lager:info("Unknown handle_info! message: ~p", [UnknownMessage]),
  {noreply, S}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec on_save(uri()) -> ok.
on_save(Uri) ->
  generate_diagnostics(Uri),
  ok.

-spec on_open(uri()) -> ok.
on_open(Uri) ->
  els_diagnostics_sup:start_server(Uri),
  generate_diagnostics(Uri),
  ok.

-spec on_close(uri()) -> ok.
on_close(Uri) ->
  els_diagnostics_sup:stop_server(Uri).

-spec generate_diagnostics(uri()) -> ok.
generate_diagnostics(Uri) ->
  {ok, Pid} = els_diagnostics_sup:lookup_pid(Uri),
  lists:foreach(fun(?COMPILER = Type) ->
                    update_state(Pid, Type, run_diagnostic(Type, Uri)),
                    notify(Pid);
                   (Type) ->
                    update_state(Pid, Type, run_diagnostic(Type, Uri))
                end, [ ?COMPILER, ?ELVIS, ?DIALYZER ]),
  notify(Pid).

-spec update_state(pid(), atom(), [diagnostic()]) -> ok.
update_state(Pid, Type, Diagnostics) ->
  {T, ok} = timer:tc(fun() ->
                         gen_server:call(Pid, {Type, Diagnostics, update})
                     end),
  lager:info("Timer for update_state. type: ~p time: ~p", [Type, T]),
  ok.

-spec notify(pid()) -> ok.
notify(Pid) ->
  {T, ok} = timer:tc(fun() -> gen_server:call(Pid, send_notification) end),
  lager:info("Timer for notify: ~p", [T]),
  ok.

-spec merge(map()) -> [diagnostic()].
merge(Diagnostics) ->
  {T, R} = timer:tc(fun() -> lists:flatten(maps:values(Diagnostics)) end),
  lager:info("Timer for merge: ~p", [T]),
  R.

-spec run_diagnostic(atom(), uri()) -> [diagnostic()].
run_diagnostic(?COMPILER, Uri) ->
  {TCompile, CDiagnostics} = timer:tc(
                               fun() ->
                                   els_compiler_diagnostics:diagnostics(Uri)
                               end),
  lager:info("Timer for compile: ~p", [TCompile]),
  maybe_compile_and_load(Uri, CDiagnostics),
  CDiagnostics;
run_diagnostic(Module, Uri) ->
  {TimeD, Res} = timer:tc(fun() -> apply(Module, diagnostics, [Uri]) end),
  lager:info("Timer for diagnostics mod: ~p ~p", [Module, TimeD]),
  Res.

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
