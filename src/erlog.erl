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
-export([start_link/1, start_link/0,
  log/2, log/3, log/4,
  debug/1, debug/2, debug/3,
  info/1, info/2, info/3,
  warning/1, warning/2, warning/3,
  error/1, error/2, error/3,
  critical/1, critical/2, critical/3,
  alert/1, alert/2, alert/3,
  reload_config/1
]).

%% gen_server callbacks
-export([init/1,init/0,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("erlog_records.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(ConfigFile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigFile], []).

%%%-------------------------------------------------------------------

reload_config(ConfigFile) ->
  gen_server:call(?SERVER, {reload, ConfigFile}).

%%%-------------------------------------------------------------------

log(Msg, Data) ->
  gen_server:cast(?SERVER, {log, Msg, Data}).

log(Level, Msg, Data) ->
  gen_server:cast(?SERVER, {log, Level, Msg, Data}).

log(Level, Logger, Msg, Data) ->
  gen_server:cast(?SERVER, {log, Level, Logger, Msg, Data}).

%%%-------------------------------------------------------------------

init() ->
  Rec = #erlog{},
  Rec2 = Rec#erlog{formatters = [#formatter{}], handlers = [#console_handler{}]},
  {ok, _} = dets:open_file(?DB_NAME, [{file, ?DB_FILE}, {type, set}]),
  {ok, Rec2}.

init(ConfigFile) ->
  file:make_dir("misc"),
  {ok, _} = dets:open_file(?DB_NAME, [{file, ?DB_FILE}, {type, set}]),
  Ret = config_loader:load_config(ConfigFile),
  {ok, Ret}.

handle_call({reload, ConfigFile}, _From, _Config) ->
  case config_loader:load_config(ConfigFile) of
    {error, invalid_config_file_detected} ->
      {reply, {error, invalid_config_file_detected}, invalid_config_file_detected};
    _ ->
      {reply, ok, config_loader:load_config(ConfigFile)}
  end;

handle_call(_Request, _From, Config) ->
  {reply, ok, Config}.

handle_cast({log, Msg, Data}, Config) ->
  {ok, Rec} = Config,
  log_writer:writelog(Msg, Data, Rec),
  {noreply, Config};

handle_cast({log, Level, Msg, Data}, Config) ->
  {ok, Rec} = Config,
  log_writer:writelog(Level, Msg, Data, Rec),
  {noreply, Config};

handle_cast({log, _Logger, _Level, _Msg, _Data}, Config) ->
  %%Not Yet Implemented
  {ok, _Rec} = Config,
  {noreply, Config};

handle_cast(_Request, Config) ->
  {noreply, Config}.


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
  log(info, Msg, Data).

warning(Logger, Msg, Data) ->
  log(info, Logger, Msg, Data).

%%%-------------------------------------------------------------------

error(Msg) ->
  log(alert, Msg, []).

error(Msg, Data) ->
  log(info, Msg, Data).

error(Logger, Msg, Data) ->
  log(info, Logger, Msg, Data).


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