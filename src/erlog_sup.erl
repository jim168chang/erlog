
-module(erlog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RestartStategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 1000,
  Type = worker,

  ErlogServerChildDef = {"Erlog Server", {erlog, start_link, []}, Restart, Shutdown, Type, [erlog]},
  FileLoggerEventChildDef = {"File Logger", {file_logger, start_link, []}, Restart, Shutdown, Type, [file_logger]},
  {ok, {SupFlags, [ErlogServerChildDef, FileLoggerEventChildDef]}}.

