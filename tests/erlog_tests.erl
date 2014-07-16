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

-include("../include/erlog_records.hrl").
-include_lib("kernel/include/file.hrl").

test_find_oldest() ->
  io:format("Testing log_writer:find_oldest/2 - "),
  {ok, [H | Rest]} = file:list_dir("."),
  {ok, HInfo} = file:read_file_info(H),
  Modified = HInfo#file_info.mtime,
  {ok, {_OldestFile, _Mod}} = log_writer:find_oldest(".", Rest, {H, Modified}),
  %%io:format("Oldest File: ~p~n", [OldestFile]),
  %%OldestFile = "Makefile",
  io:format("passed~n").

test_write_log() ->
  io:format("Testing log_writer:writelog/3 - "),
  {ok, Config} = config_loader:load_config("conf/default.conf"),
  K = 2,
  log_writer:writelog("Hello World: ~p~n", [K], Config),
  Me = {{name, "Olusegun Akintayo"}, {age, 21}, {sex, "Male"}},
  log_writer:writelog("Me: ~p~n", [Me], Config),

  log_writer:writelog(debug, "Me Again: ~p~n", [Me], Config),

  io:format("passed~n").

test_config_loader_load_config() ->
  io:format("Testing config_loader:load_config/0 - "),
  Rec = config_loader:load_config("conf/default.conf"),
  case Rec of
    {error, invalid_config_file_detected} -> io:format("Failed. ~p~n", [Rec]);
    _ ->
      {ok, Config} = Rec,
      1 = length(Config#erlog.formatters),
      [Formatter | _Rest] = Config#erlog.formatters,
      console_formatter = Formatter#formatter.name,
      5 = length(Config#erlog.handlers),
      [FileHandler | [FileHandler2 | [ConsoleHandler | [ConsoleHandler2 | Rest]] ]] = Config#erlog.handlers,
      true = is_list(Rest),
      "MFH" = FileHandler#file_handler.name,
      ?ERROR = ConsoleHandler2#console_handler.level,
      ?NONE = ConsoleHandler#console_handler.level,
      ?DEBUG = FileHandler#file_handler.level,
      FileFormatter = FileHandler#file_handler.formatter,
      ConsoleFormatter = ConsoleHandler2#console_handler.formatter,
      default_f = ConsoleFormatter#formatter.name,
      console_formatter = FileFormatter#formatter.name,

      {error, invalid_config_file_detected} = config_loader:load_config("./non_existent.conf"),
      io:format("passed~n")
  end.

test_erlog_reload_config() ->
  io:format("Testing erlog:reload_config/0 - "),
  RetVal = erlog:reload_config("conf/default2.conf"),
  io:format("Loaded Config: ~p~n", [RetVal]),
  io:format("passed~n").

tests() ->
  io:format("~n~n------- Starting Tests -------~n~n"),
  erlog:start_link("conf/default.conf"),
  test_find_oldest(),
  test_config_loader_load_config(),
  test_erlog_reload_config(),
  test_write_log(),
  ok.