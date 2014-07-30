%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2014 11:57 AM
%%%-------------------------------------------------------------------
-module(file_logger).
-author("aardvocate").

-behaviour(gen_event).

%% API
-export([start_link/0,
  add_handler/0, log/3, add_log_file/1, clear_state/0]).

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

clear_state() ->
  gen_event:call(?SERVER, ?MODULE, clear_state).

add_log_file(File) ->
  gen_event:notify(?SERVER, {add_file, File}).

log(LogLevel, FileHandler, Msg) ->
  gen_event:notify(?SERVER, {log, Msg, LogLevel, FileHandler}).

handle_event({log, _Msg, _LogLevel, #file_handler{}} = Event, State) ->
  log_to_file(Event),
  {ok, State};

handle_event({add_file, File}, State) ->
  State2 = [File | State],
  {ok, State2};

handle_event(_Event, State) ->
  {ok, State}.

handle_call(clear_state, _State) ->
  State2 = [],
  {ok, cleared, State2};

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

log_to_file({log, Msg, LogLevel, #file_handler{dir = Dir, file = File, formatter = #formatter{format = Format}, level = HandlerLevelNumber, max_files = _MaxFiles, size = _Size, name = Name}} = Event) ->
  LogLevelNumber = config_loader:level_term_to_number(LogLevel),
  if
    HandlerLevelNumber >= LogLevelNumber ->
      {ok, LogFileHandle} = file:open(filename:join(Dir, File), [append]),
      {ToWrite, _Ref} = Msg,
      {ok, Str} = log_writer:get_msg_formatted(Format, ToWrite, "", {LogLevelNumber}),
      io:format(LogFileHandle, Str, []),
      file:close(LogFileHandle);
    true ->
      log_filtered_by_level
  end.