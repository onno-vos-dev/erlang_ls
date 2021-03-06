%%==============================================================================
%% Application Callback Module
%%==============================================================================
-module(erlang_ls_app).

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
%% Defines
%%==============================================================================
-define(DEFAULT_PORT, 10000).

%%==============================================================================
%% Application Callbacks
%%==============================================================================
-spec start(normal, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  Port = application:get_env(erlang_ls, port, ?DEFAULT_PORT),
  {ok, _} = ranch:start_listener( erlang_ls
                                , ranch_tcp
                                , [{port, Port}]
                                , erlang_ls_server
                                , []
                                ),
  erlang_ls_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  ok.
