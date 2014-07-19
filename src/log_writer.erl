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
-export([writelog/3, find_oldest/3, find_last_rotated_file/2, get_msg_formatted/4, rotatelog/1]).

%% comment this out when going live
-compile(export_all).

-include("erlog_records.hrl").
-include_lib("kernel/include/file.hrl").

writelog(Level, Config, ToLog) ->
  FileHandlers = Config#erlog.file_handlers,
  write_file_log(Level, FileHandlers, ToLog),
  write_console_log(Level, Config#erlog.console_handler, ToLog),
  {ok, submitted}.

%%%-------------------------------------------------------------------

write_file_log(LogLevel, [#file_handler{} = H | Rest], ToLog) ->
  file_logger:log(LogLevel, H, ToLog),
  write_file_log(LogLevel, Rest, ToLog);

write_file_log(_LogLevel, [], _ToLog) ->
  ok.

write_console_log(LogLevel, #console_handler{} = H, ToLog) ->
  io:format("~p", [console_logger:log(LogLevel, H, ToLog)]);

write_console_log(_LogLevel, _, _ToLog) ->
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
rotatelog([#file_handler{} = H | Rest]) ->
  #file_handler{file = File, dir = Dir, size = Size, max_files = MaxFiles} = H,
  rotate_log(File, Dir, Size, MaxFiles),
  rotatelog(Rest);

rotatelog([_H | Rest]) ->
  rotatelog(Rest);

rotatelog([]) ->
  ok.

%%%-------------------------------------------------------------------

rotate_log(File, Dir, Size, MaxFiles) ->
  delete_old_files(File, Dir, MaxFiles),

  Filename = filename:join(Dir, File),
  {ok, FileInfo} = file:read_file_info(Filename),
  FileSize = FileInfo#file_info.size,

  if
    FileSize >= (Size * 1024) ->
      {ok, Dirs} = file:list_dir(Dir),
      Ms = find_last_rotated_file(Dirs, 0) + 1,
      MsString = lists:flatten(io_lib:format("~p", [Ms])),
      To = string:concat(string:concat(Filename, "."), MsString),
      file:copy(Filename, To),
      file:write_file(Filename, "");
    true ->
      ok
  end.

%%%-------------------------------------------------------------------

find_files_starting_with(File, [H | Files], Acc) ->
  case string:str(H, File) of
    0 ->
      find_files_starting_with(File, Files, Acc);
    _ ->
      if
        File =:= H ->
          find_files_starting_with(File, Files, Acc);
        true ->
          find_files_starting_with(File, Files, [H | Acc])
      end
  end;

find_files_starting_with(_File, [], Acc) ->
  Acc.

delete_old_files(File, Dir, MaxFiles) ->
  %%CompareMax = MaxFiles - 1,
  {ok, Files} = file:list_dir(Dir),
  [H | Rest] = find_files_starting_with(File, Files, []),

  if
    length(Rest) >= MaxFiles ->
      {ok, HInfo} = file:read_file_info(filename:join(Dir, H)),
      Modified = HInfo#file_info.mtime,
      {ok, {OldestFile, _Mod}} = find_oldest(Dir, Rest, {filename:join(Dir, H), Modified}),
      case string:str(OldestFile, "log/") of
        0 ->
          file:delete(filename:join(Dir, OldestFile));
        _ ->
          file:delete(OldestFile)
      end;
    true ->
      ok
  end.

%%%-------------------------------------------------------------------

find_last_rotated_file([H | Files], Filenumber) ->
  NewFileNumber = case filename:extension(H) of
                    ".log" ->
                      Filenumber;
                    Val ->
                      {FN, _} = string:to_integer(string:substr(Val, 2)),
                      NewFN = if
                                FN > Filenumber ->
                                  FN;
                                true ->
                                  Filenumber
                              end,
                      NewFN
                  end,
  find_last_rotated_file(Files, NewFileNumber);

find_last_rotated_file([], Filenumber) ->
  Filenumber.

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
