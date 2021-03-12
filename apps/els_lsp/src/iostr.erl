-module(iostr).
-behaviour(gen_server).

-export([start_link/1, start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          data,
          line = 1,
          lines
         }).

-spec start_link(string()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(Data) ->
  {ok, _Pid} = gen_server:start_link(?MODULE, [Data], []).

-spec start(string()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start(Data) ->
  gen_server:start(?MODULE, [Data], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
  gen_server:cast(Pid, stop).

-spec init([string()]) -> {ok, #state{}}.
init([Data0]) ->
  Data = [Line ++ "\n" || Line <- string:tokens(Data0, "\n")],
  {ok, #state{data = Data, lines = length(Data)}}.

-spec handle_call(any(), any(), #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(stop | any(), #state{}) -> {stop, normal, #state{}} | {noreply, #state{}}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info({io_request | any(), pid(), any(), tuple()}, #state{}) ->
  {noreply, #state{}} | {stop, normal, #state{}}.
handle_info({io_request, From, ReplyAs, {get_until, _, _, _, _, _}},
            #state{data = [], lines = L} = State) ->
  From ! {io_reply, ReplyAs, {eof, L}},
  {stop, normal, State};
handle_info({io_request, From, ReplyAs, {get_until, _, _, M, F, Args}},
            #state{data = Data, line = L} = State) ->
  case handler(Data, L, [], M, F, Args) of
    eof ->
      Lines = State#state.lines,
      From ! {io_reply, ReplyAs, {eof, Lines}},
      {stop, normal, State#state{data = []}};
    {ok, Result, Rest, NData, NL} ->
      From ! {io_reply, ReplyAs, Result},
      case Rest of
        [] ->
          {noreply, State#state{data = NData, line = NL}};
        _ ->
          {noreply, State#state{data = [Rest | NData], line = NL}}
      end
  end;
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec handler(any(), any(), any(), any(), any(), any()) -> any().
handler([Input | Data], L, Cont, M, F, Extra) ->
  case catch apply(M, F, [Cont, Input | Extra]) of
    {done, eof, _} ->
      eof;
    {done, Result, Rest} ->
      {ok, Result, Rest, Data, L + 1};
    {more, NCont} ->
      case Data of
        [] ->
          eof;
        _ ->
          handler(Data, L + 1, NCont, M, F, Extra)
      end
  end.
