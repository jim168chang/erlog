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

test_find_last_rotated_log_file() ->
  io:format("Testing log_writer:find_last_rotated_file/2 - "),
  {ok, Dirs} = file:list_dir("logs"),
  Filenumber = log_writer:find_last_rotated_file(Dirs, 0),
  %%Filenumber = 2,
  io:format("passed~n").

test_get_msg_formatted() ->
  io:format("Testing log_writer:get_msg_formatted/4 - "),
  DateStr = log_writer:get_msg_formatted([date | []], {"Hello ~p", [1011001]}, "", {64}),
  DateStr = {ok,"2014-7-17"},
  TimeStr = log_writer:get_msg_formatted([time | []], {"Hello ~p", [1011001]}, "", {64}),
  %%TimeStr = {ok, "22:2:26"},
  LevelStrDebug = log_writer:get_msg_formatted([level | []], {"Hello ~p", [1011001]}, "", {64}),
  LevelStrNone= log_writer:get_msg_formatted([level | []], {"Hello ~p", [1011001]}, "", {0}),
  LevelStrDebug = {ok, "debug"},
  LevelStrNone = {ok, "none"},

  MessageStr = log_writer:get_msg_formatted([message | []], {"Hello ~p", [1011001]}, "", {64}),
  MessageStr = {ok, "Hello 1011001"},

  Format = [date, " [", level, "] - ", message, "~n"],

  MessageStr2 = log_writer:get_msg_formatted(Format, {"Hello ~p", [1011001]}, "", {64}),

  MessageStr2 = {ok, "2014-7-17 [debug] - Hello 1011001\n"},

  io:format("passed~n").

test_rotate_log() ->
  io:format("Testing log_writer:rotate_log/4 - "),
  ok = log_writer:rotate_log("erlog_error.log", "logs", 4, 10),
  io:format("passed~n").

tests() ->
  io:format("~n~n------- Starting Tests -------~n~n"),
  test_find_last_rotated_log_file(),
  test_find_oldest(),
  test_get_msg_formatted(),
  test_rotate_log(),
  ok.