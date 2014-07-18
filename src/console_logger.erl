%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2014 11:04 PM
%%%-------------------------------------------------------------------
-module(console_logger).
-author("aardvocate").

-behaviour(gen_event).

%% API
-export([start_link/0,
  add_handler/0, log/3]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("erlog_records.hrl").

start_link() ->
  process_flag(trap_exit, true),
  gen_event:start_link({local, ?SERVER}).


add_handler() ->
  gen_event:add_handler(?SERVER, ?MODULE, []).


init([]) ->
  {ok, []}.

log(LogLevel, ConsoleHandler, Msg) ->
  case gen_event:call(?SERVER, ?MODULE, {log, Msg, LogLevel, ConsoleHandler}) of
    {ok, Reply} ->
      Reply;
    log_filtered_by_level ->
      log_filtered_by_level;
    _ ->
      {error, log_returned_unknow_message}
  end.

handle_event(_Event, State) ->
  {ok, State}.

handle_call({log, Msg, LogLevel, #console_handler{formatter = #formatter{format = Format}, level = HandlerLevelNumber}} = _Event, State) ->
  LogLevelNumber = config_loader:level_term_to_number(LogLevel),

  Message = if
    HandlerLevelNumber >= LogLevelNumber ->
      {ToWrite, _Ref} = Msg,
      log_writer:get_msg_formatted(Format, ToWrite, "", {LogLevelNumber});
    true ->
      log_filtered_by_level
  end,
  {ok, Message, State};

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.


terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
