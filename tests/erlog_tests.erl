%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2014 11:22 PM
%%%-------------------------------------------------------------------
-module(erlog_tests).
-author("aardvocate").

%% API
-export([tests/0]).

-include("../include/erlog.hrl").

test_write_log() ->
  io:format("Testing log_writer:writelog/3 - "),
  Config = config_loader:load_config("conf/default.conf"),
  K = 2,
  log_writer:writelog("Hello World: ~p~n", [K], Config),
  Me = {{name, "Olusegun Akintayo"}, {age, 21}, {sex, "Male"}},
  log_writer:writelog("Me: ~p~n", [Me], Config),
  io:format("passed~n").

test_config_loader_load_config() ->
  io:format("Testing config_loader:load_config/0 - "),
  Rec = config_loader:load_config("conf/default.conf"),
  1 = length(Rec#erlog.formatters),
  [Formatter | _Rest] = Rec#erlog.formatters,
  console_formatter = Formatter#formatter.name,
  2 = length(Rec#erlog.handlers),
  [FileHandler | [ConsoleHandler | []] ] = Rec#erlog.handlers,
  "MFH" = FileHandler#file_handler.name,
  ?INFO = ConsoleHandler#console_handler.level,
  ?DEBUG = FileHandler#file_handler.level,
  FileFormatter = FileHandler#file_handler.formatter,
  console_formatter = FileFormatter#formatter.name,

  {error, invalid_config_file_detected} = config_loader:load_config("./non_existent.conf"),
  io:format("passed~n").

test_erlog_reload_config() ->
  io:format("Testing erlog:reload_config/0 - "),
  ok = erlog:reload_config("conf/default2.conf"),
  io:format("passed~n").

tests() ->
  io:format("~n~n------- Starting Tests -------~n~n"),
  erlog:start_link("conf/default.conf"),
  test_config_loader_load_config(),
  test_erlog_reload_config(),
  test_write_log(),
  ok.