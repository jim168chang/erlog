%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2014 2:20 PM
%%%-------------------------------------------------------------------
-module(config_loader).
-author("aardvocate").

%% API
-export([load_config/1, number_to_level_term/1, level_term_to_number/1]).

%%-compile(export_all).

-include("erlog.hrl").

load_config(ConfigFile) ->
  case file:consult(ConfigFile) of
    {ok, [Result | _]} ->
      Rec = #erlog{},
      {erlog, Config} = Result,
      load_record(Config, Rec);
    {error, _Val} ->
      {error, invalid_config_file_detected}
  end.


%%-----------------------------------------------------------------

load_record([{formatter, FormatterConfig} | Rest], Rec) ->
  Rec2 = load_formatter_record(FormatterConfig, Rec),
  load_record(Rest, Rec2);

load_record([{console_handler, CHConfig} | Rest], Rec) ->
  Rec2 = load_ch_record(CHConfig, Rec),
  load_record(Rest, Rec2);

load_record([{file_handler, FHConfig} | Rest], Rec) ->
  Rec2 = load_fh_record(FHConfig, Rec),
  load_record(Rest, Rec2);

load_record([], Rec) -> Rec.

%%-----------------------------------------------------------------

load_fh_record(FHConfig, Rec) ->
  [{name, Name}] = lists:filter(fun(I) -> case I of {name, _Name} -> true; _ -> false end end, FHConfig),
  Level = case lists:filter(fun(I) -> case I of {level, _Level} -> true; _ -> false end end, FHConfig) of
            [] -> ?NONE;
            [{level, L}] -> level_term_to_number(L)
          end,

  Formatter = case lists:filter(fun(I) -> case I of {formatter, _Formatter} -> true; _ -> false end end, FHConfig) of
                [] -> #formatter{};
                [{formatter, Fm}] ->
                  Formatters = Rec#erlog.formatters,
                  case lists:filter(fun(I) -> FormatterName = I#formatter.name,
                    if FormatterName =:= Fm -> true; true -> false end end, Formatters) of
                    [] -> error("No Formatter with Name: " ++ Fm ++ " found in config.");
                    [Found] -> Found
                  end
              end,

  File = case lists:filter(fun(I) -> case I of {file, _File} -> true; _ -> false end end, FHConfig) of
           [] -> ?DEFAULT_FILE;
           [{file, F}] -> F
         end,

  Dir = case lists:filter(fun(I) -> case I of {dir, _Dir} -> true; _ -> false end end, FHConfig) of
          [] -> ?DEFAULT_DIR;
          [{dir, D}] -> D
        end,

  Size = case lists:filter(fun(I) -> case I of {size, _Size} -> true; _ -> false end end, FHConfig) of
           [] -> ?DEFAULT_SIZE;
           [{size, S}] -> S
         end,
  MaxFiles = case lists:filter(fun(I) -> case I of {max_files, _MaxFiles} -> true; _ -> false end end, FHConfig) of
               [] -> ?DEFAULT_MAX_FILES;
               [{max_files, M}] -> M
             end,

  FileHandler = #file_handler{name = Name, level = Level, formatter = Formatter, file = File, dir = Dir, size = Size, max_files = MaxFiles},
  Handlers = Rec#erlog.handlers,
  Rec#erlog{handlers = [FileHandler | Handlers]}.

%%-----------------------------------------------------------------

load_ch_record(CHConfig, Rec) ->
  [{name, Name}] = lists:filter(fun(I) -> case I of {name, _Name} -> true; _ -> false end end, CHConfig),
  Level = case lists:filter(fun(I) -> case I of {level, _Level} -> true; _ -> false end end, CHConfig) of
            [] -> ?NONE;
            [{level, L}] -> level_term_to_number(L)
          end,

  Formatter = case lists:filter(fun(I) -> case I of {formatter, _Formatter} -> true; _ -> false end end, CHConfig) of
                [] -> #formatter{};
                [{formatter, Fm}] ->
                  Formatters = Rec#erlog.formatters,
                  case lists:filter(fun(I) -> FormatterName = I#formatter.name,
                    if FormatterName =:= Fm -> true; true -> false end end, Formatters) of
                    [] -> error("No Formatter with Name: " ++ Fm ++ " found in config.");
                    [Found] -> Found
                  end
              end,

  ConsoleHandler = #console_handler{name = Name, level = Level, formatter = Formatter},
  Handlers = Rec#erlog.handlers,
  Rec#erlog{handlers = [ConsoleHandler | Handlers]}.

%%-----------------------------------------------------------------

load_formatter_record(FormatterConfig, Rec) ->
  [{name, Name}] = lists:filter(fun(I) -> case I of {name, _Name} -> true; _ -> false end end, FormatterConfig),
  Format = case lists:filter(fun(I) -> case I of {format, _Format} -> true; _ -> false end end, FormatterConfig) of
             [] -> ?DEFAULT_FORMAT;
             [{format, F}] -> F
           end,
  Formatter = #formatter{name = Name, format = Format},
  Formatters = Rec#erlog.formatters,
  Rec#erlog{formatters = [Formatter | Formatters]}.

%%-----------------------------------------------------------------

level_term_to_number(LevelTerm) ->
  case LevelTerm of
    none -> ?NONE;
    alert -> ?ALERT;
    critical -> ?CRITICAL;
    error -> ?ERROR;
    warning -> ?WARNING;
    info -> ?INFO;
    debug -> ?DEBUG;
    _ -> ?NONE
  end.

number_to_level_term(Number) ->
  case Number of
    0 -> none;
    2 -> alert;
    4 -> critical;
    8 -> error;
    16 -> warning;
    32 -> info;
    64 -> debug
  end.
%%-----------------------------------------------------------------
