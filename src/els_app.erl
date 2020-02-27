%%==============================================================================
%% Application Callback Module
%%==============================================================================
-module(els_app).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(application).

%%==============================================================================
%% Exports
%%==============================================================================
%% Application Callbacks
-export([ start/2
        , stop/1
        ]).

%%==============================================================================
%% Application Callbacks
%%==============================================================================
-spec start(normal, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  ok = application:set_env(elvis, no_output, true),
  els_sup:start_link(),
  els_diagnostics_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  ok.
