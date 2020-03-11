-module(els_diagnostics_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0
        , start_server/1
        , stop_server/1
        , lookup_pid/1
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(DIAGNOSTICS_SERVER_ETS, diagnostics_server_ets).

-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_server(uri()) -> ok.
start_server(Uri) ->
  %% It seems that we receive a double didOpen event so we cannot just
  %% hard match on {ok, Pid}
  case lookup_pid(Uri) of
    {ok, _Pid} -> ok;
    {error, not_found} ->
      {ok, Pid} = supervisor:start_child(?SERVER, [Uri]),
      ets:insert(?DIAGNOSTICS_SERVER_ETS, {Uri, Pid})
  end.

-spec stop_server(uri()) -> ok.
stop_server(Uri) ->
  {ok, Pid} = lookup_pid(Uri),
  supervisor:terminate_child(?SERVER, Pid).

-spec lookup_pid(uri()) -> {ok, pid()} | {error, not_found}.
lookup_pid(Uri) ->
  case ets:lookup(?DIAGNOSTICS_SERVER_ETS, Uri) of
    [{Uri, Pid}] -> {ok, Pid};
    [] -> {error, not_found}
  end.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  ets:new(?DIAGNOSTICS_SERVER_ETS, [public, named_table, set]),
  SupFlags = #{ strategy  => simple_one_for_one
              , intensity => 5
              , period    => 1
              },
  {ok, {SupFlags, [child_spec()]}}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec child_spec() -> map().
child_spec() ->
  #{ id       => els_diagnostics_server
   , start    => {els_diagnostics_server, start_link, []}
   , shutdown => brutal_kill
   }.
