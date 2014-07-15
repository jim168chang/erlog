%%%-------------------------------------------------------------------
%%% @author aardvocate
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2014 12:10 AM
%%%-------------------------------------------------------------------
-author("aardvocate").

-define(NONE, 0).
-define(ALERT, 2).
-define(CRITICAL, 4).
-define(ERROR, 8).
-define(WARNING, 16).
-define(INFO, 32).
-define(DEBUG, 64).

-define(DEFAULT_FORMATTER_NAME, default_f).
-define(DEFAULT_FORMAT, [date, " ", time, " [", level, "] - ", message, "\n"]).

-define(DEFAULT_CONSOLE_HANDLER_NAME, default_ch).

-define(DEFAULT_FILE_HANDLER_NAME, default_fh).
-define(DEFAULT_FILE, "erlog.log").
-define(DEFAULT_DIR, ".").
-define(DEFAULT_SIZE, 4096).
-define(DEFAULT_MAX_FILES, 100).

-record(formatter, {name=?DEFAULT_FORMATTER_NAME, format=?DEFAULT_FORMAT}).
-record(console_handler, {name=?DEFAULT_CONSOLE_HANDLER_NAME, level=?NONE, formatter=#formatter{}}).
-record(file_handler, {name=?DEFAULT_FILE_HANDLER_NAME, level=?NONE, file=?DEFAULT_FILE, dir=?DEFAULT_DIR,
                        size=?DEFAULT_SIZE, max_files=?DEFAULT_MAX_FILES, formatter=#formatter{}}).
-record(erlog, {formatters=[], handlers=[]}).

-record(log, {level, logger, msg, data}).