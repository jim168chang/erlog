%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2014 4:24 PM
%%%-------------------------------------------------------------------
-module(log_writer).
-author("aardvocate").

%% API
-export([writelog/3, writelog/4, find_oldest/3]).

-include("erlog_records.hrl").
-include_lib("kernel/include/file.hrl").

writelog(Level, Msg, Data, Config) ->
  Handlers = Config#erlog.handlers,
  write_log(Level, Handlers, {Msg, Data}),
  {ok, submitted}.

%%%-------------------------------------------------------------------

writelog(Msg, Data, Config) ->
  write_log_nolevel(Msg, Data, Config),
  {ok, submitted}.

%%%-------------------------------------------------------------------

write_log_nolevel(Msg, Data, Config) ->
  Handlers = Config#erlog.handlers,
  write_log(none, Handlers, {Msg, Data}).

%%%-------------------------------------------------------------------

write_log(LogLevel, [#console_handler{} = H | Rest], ToWrite) ->
  #console_handler{formatter = #formatter{format = Format}, level = HandlerLevelNumber} = H,
  LogLevelNumber = config_loader:level_term_to_number(LogLevel),
  if
    HandlerLevelNumber >= LogLevelNumber ->
      {ok, Str} = get_msg_formatted(Format, ToWrite, "", {LogLevelNumber}),
      io:format(Str, []);
    true ->
      log_filtered_by_level
  end,
  write_log(LogLevel, Rest, ToWrite);

write_log(LogLevel, [#file_handler{} = H | Rest], ToWrite) ->
  #file_handler{dir = Dir, file = File, formatter = #formatter{format = Format}, level = HandlerLevelNumber, max_files = MaxFiles, size = Size} = H,
  LogLevelNumber = config_loader:level_term_to_number(LogLevel),
  if
    HandlerLevelNumber >= LogLevelNumber ->
      Filename = filename:join(Dir, File),
      {ok, LogFileHandle} = file:open(Filename, [append]),
      {ok, Str} = get_msg_formatted(Format, ToWrite, "", {LogLevelNumber}),
      io:format(LogFileHandle, Str, []),
      file:close(LogFileHandle),
      rotate_log({File, Dir, Size, MaxFiles});
    true ->
      log_filtered_by_level
  end,
  write_log(LogLevel, Rest, ToWrite);

write_log(_LogLevel, [], _ToWrite) ->
  ok.

%%%-------------------------------------------------------------------

get_msg_formatted([date | Rest], ToWrite, Str, Others) ->
  {YYYY, MM, DD} = date(),
  DateStr = lists:flatten(io_lib:format("~p-~p-~p", [YYYY, MM, DD])),
  Str2 = Str ++ DateStr,
  get_msg_formatted(Rest, ToWrite, Str2, Others);

get_msg_formatted([message | Rest], {Msg, Data} = ToWrite, Str, Others) ->
  MsgStr = lists:flatten(io_lib:format(Msg, Data)),
  Str2 = Str ++ MsgStr,
  get_msg_formatted(Rest, ToWrite, Str2, Others);

get_msg_formatted([time | Rest], ToWrite, Str, Others) ->
  {HH, MM, SS} = time(),
  TimeStr = lists:flatten(io_lib:format("~p:~p:~p", [HH, MM, SS])),
  Str2 = Str ++ TimeStr,
  get_msg_formatted(Rest, ToWrite, Str2, Others);

get_msg_formatted([level | Rest], ToWrite, Str, Others) ->
  LeveStr = lists:flatten(io_lib:format("~p", [config_loader:number_to_level_term(element(1, Others))])),
  Str2 = Str ++ LeveStr,
  get_msg_formatted(Rest, ToWrite, Str2, Others);

get_msg_formatted([OtherFormats | Rest], ToWrite, Str, Others) ->
  OtherFormatsStr = lists:flatten(io_lib:format(OtherFormats, [])),
  Str2 = Str ++ OtherFormatsStr,
  get_msg_formatted(Rest, ToWrite, Str2, Others);

get_msg_formatted([], _ToWrite, Str, _Others) ->
  {ok, Str}.

%%%-------------------------------------------------------------------

rotate_log({File, Dir, Size, MaxFiles}) ->
  Filename = filename:join(Dir, File),
  {ok, FileInfo} = file:read_file_info(Filename),
  FileSize = FileInfo#file_info.size,
  if
    FileSize >= (Size * 1024) ->
      Ms = case dets:lookup(?DB_NAME, ?DB_KEY) of
        [] -> 1;
        [{_Key, {ok, Val}}] -> Val
      end,

      {ok, [H | Rest]} = file:list_dir(Dir),
      CompareMax = MaxFiles - 1,

      if
        length(Rest) >= CompareMax ->
          {ok, HInfo} = file:read_file_info(filename:join(Dir, H)),
          Modified = HInfo#file_info.mtime,
          {ok, {OldestFile, _Mod}} = find_oldest(Dir, Rest, {filename:join(Dir, H), Modified}),
          file:delete(OldestFile);
        true ->
          ok
      end,
      file:copy(Filename, filename:join(Dir, lists:flatten(io_lib:format("erlog.~p", [Ms])))),
      file:delete(Filename),
      file:write_file(Filename, "", [append]),
      dets:insert(?DB_NAME, [{?DB_KEY, {ok, Ms + 1}}]);
    true ->
      do_nothing
  end.

%%%-------------------------------------------------------------------

find_oldest(Dir, [H | Rest], {_File, Modified} = Acc) ->
  {ok, FileInfo} = file:read_file_info(filename:join(Dir, H)),
  HModified = FileInfo#file_info.mtime,
  Difference = calendar:datetime_to_gregorian_seconds(HModified) - calendar:datetime_to_gregorian_seconds(Modified),
  if
    Difference =< 0 ->
      find_oldest(Dir, Rest, {H, HModified});
    true ->
      find_oldest(Dir, Rest, Acc)
  end;

find_oldest(_Dir, [], Acc) ->
  {ok, Acc}.