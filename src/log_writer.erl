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
-export([writelog/3, writelog/4]).

-include("erlog.hrl").

writelog(Level, Msg, Data, Config) ->
  spawn(fun() -> write_log(Level, {Msg, Data}, Config) end),
  {ok, submitted}.

writelog(Msg, Data, Config) ->
  spawn(fun() -> write_log_nolevel(Msg, Data, Config) end),
  {ok, submitted}.

write_log_nolevel(Msg, Data, Config) ->
  Handlers = Config#erlog.handlers,
  write_log(none, Handlers, {Msg, Data}).


write_log(LogLevel, [#console_handler{} = H | Rest], ToWrite) ->
  spawn(
    fun() ->
      #console_handler{formatter = #formatter{format = Format}, level = HandlerLevelNumber} = H,
      LogLevelNumber = config_loader:level_term_to_number(LogLevel),
      if
        HandlerLevelNumber > LogLevelNumber ->
          Str = get_msg_formatted(Format, ToWrite, "", {HandlerLevelNumber}),
          io:format(Str, []);
        true ->
          log_filtered_by_level
      end
    end
  ),
  write_log(LogLevel, Rest, ToWrite);

write_log(LogLevel, [#file_handler{} = H | Rest], ToWrite) ->
  spawn(
    fun() ->
      #file_handler{dir = Dir, file = File, formatter = #formatter{format = Format}, level = HandlerLevelNumber, max_files = MaxFiles, name = Name, size = Size} = H,
      LogLevelNumber = config_loader:level_term_to_number(LogLevel),
      if
        HandlerLevelNumber > LogLevelNumber ->
          Filename = filename:join(Dir, File),
          file:make_dir(Dir),
          {ok, LogFileHandle} = file:open(Filename, [append]),
          Str = get_msg_formatted(Format, ToWrite, "", {HandlerLevelNumber}),
          io:format(LogFileHandle, Str, []),
          {ok, Read} = file:read_file(Filename),
          file:close(LogFileHandle);
          %%rotate_log(H),
        true ->
          log_filtered_by_level
      end
    end
  ),
  write_log(LogLevel, Rest, ToWrite);

write_log(_LogLevel, [], _ToWrite) ->
  ok.

get_msg_formatted([date | Rest], ToWrite, Str, Others) ->
  {YYYY, MM, DD} = date(),
  DateStr = lists:flatten(io_lib:format("~p-~p-~p",[YYYY, MM, DD])),
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
  Str.