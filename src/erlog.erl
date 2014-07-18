%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2014 12:00 AM
%%%-------------------------------------------------------------------
-module(erlog).
-author("aardvocate").

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, start/0,
  log/2, log/3, log/4,
  debug/1, debug/2, debug/3,
  info/1, info/2, info/3,
  warning/1, warning/2, warning/3,
  error/1, error/2, error/3,
  critical/1, critical/2, critical/3,
  alert/1, alert/2, alert/3, load_config_file/1, print_state/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("erlog_records.hrl").

start() ->
  application:start(erlog),
  file_logger:add_handler(),
  console_logger:add_handler(),
  ok.

start_link() ->
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(ConfigFile) ->
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigFile], []).

%%%-------------------------------------------------------------------

log(Msg, Data) ->
  gen_server:cast(?SERVER, {log, Msg, Data}).

log(Level, Msg, Data) ->
  gen_server:cast(?SERVER, {log, Level, Msg, Data}).

log(Level, Logger, Msg, Data) ->
  gen_server:cast(?SERVER, {log, Level, Logger, Msg, Data}).

%%%-------------------------------------------------------------------

init([]) ->
  Rec = #erlog{},
  Rec2 = Rec#erlog{formatters = [#formatter{}], console_handler = #console_handler{}},
  erlang:send_after(?ROLLER_DELAY, self(), roll_log),
  {ok, Rec2}.

%%%-------------------------------------------------------------------

handle_call({reload_config, ConfigFile}, _From, _Config) ->
  NewState = config_loader:load_config(ConfigFile),
  {reply, ok, NewState};

handle_call(print_state, _From, Config) ->
  {reply, {ok, Config}, Config};

handle_call(_Request, _From, Config) ->
  {reply, ok, Config}.

handle_cast({log, Msg, Data}, Config) ->
  Rec = case Config of
          {ok, Rec2} -> Rec2;
          Rec2 -> Rec2
        end,
  log_writer:writelog(none, Rec, {{Msg, Data}, make_ref()}),
  {noreply, {ok, Rec}};

handle_cast({log, Level, Msg, Data}, Config) ->
  Rec = case Config of
          {ok, Rec2} -> Rec2;
          Rec2 -> Rec2
        end,
  log_writer:writelog(Level, Rec, {{Msg, Data}, make_ref()}),
  {noreply, {ok, Rec}};

handle_cast({log, _Logger, _Level, _Msg, _Data}, Config) ->
  %%Not Yet Implemented
  {ok, _Rec} = Config,
  {noreply, Config};

handle_cast(_Request, Config) ->
  {noreply, Config}.

handle_info(roll_log, Config) ->
  Rec = case Config of
    {ok, Rec2} -> Rec2;
    Rec2 -> Rec2
  end,
  Handlers = Rec#erlog.file_handlers,
  log_writer:rotatelog(Handlers),
  erlang:send_after(?ROLLER_DELAY, self(), roll_log),
  {noreply, {ok, Rec}};

handle_info(_Info, Config) ->
  {noreply, Config}.


terminate(_Reason, _Config) ->
  ok.

code_change(_OldVsn, Config, _Extra) ->
  {ok, Config}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
debug(Msg) ->
  log(debug, Msg, []).

debug(Msg, Data) ->
  log(debug, Msg, Data).

debug(Logger, Msg, Data) ->
  log(debug, Logger, Msg, Data).

%%%-------------------------------------------------------------------

info(Msg) ->
  log(info, Msg, []).

info(Msg, Data) ->
  log(info, Msg, Data).

info(Logger, Msg, Data) ->
  log(info, Logger, Msg, Data).

%%%-------------------------------------------------------------------

warning(Msg) ->
  log(warning, Msg, []).

warning(Msg, Data) ->
  log(warning, Msg, Data).

warning(Logger, Msg, Data) ->
  log(warning, Logger, Msg, Data).

%%%-------------------------------------------------------------------

error(Msg) ->
  log(error, Msg, []).

error(Msg, Data) ->
  log(error, Msg, Data).

error(Logger, Msg, Data) ->
  log(error, Logger, Msg, Data).


%%%-------------------------------------------------------------------

critical(Msg) ->
  log(critical, Msg, []).

critical(Msg, Data) ->
  log(critical, Msg, Data).

critical(Logger, Msg, Data) ->
  log(critical, Logger, Msg, Data).

%%%-------------------------------------------------------------------

alert(Msg) ->
  log(alert, Msg, []).

alert(Msg, Data) ->
  log(alert, Msg, Data).

alert(Logger, Msg, Data) ->
  log(alert, Logger, Msg, Data).

%%%-------------------------------------------------------------------

load_config_file(ConfigFile) ->
  gen_server:call(?SERVER, {reload_config, ConfigFile}).

%%%-------------------------------------------------------------------

print_state() ->
  gen_server:call(?SERVER, print_state).