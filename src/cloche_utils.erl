
-module(cloche_utils).
-export([clear_dir/1]).

clear_files(Dir, [ Filename | Filenames ]) ->
  Fn = filename:join([ Dir, Filename ]),
  case file:read_file_info(Fn) of
    { ok, { file_info, _, regular, _, _, _, _, _, _, _, _, _, _, _ } } ->
      io:format("deleting file ~p~n", [ Fn ]),
      file:delete(Fn),
      clear_files(Dir, Filenames);
    { ok, { file_info, _, directory, _, _, _, _, _, _, _, _, _, _, _ } } ->
      clear_dir(Fn);
    Any ->
      io:format("can't clear file ~p : ~p~n", [ Filename, Any ])
      %false
  end;
clear_files(_, []) ->
  true.

clear_dir(Dir) ->
  case file:list_dir(Dir) of
    { ok, Filenames } -> clear_files(Dir, Filenames);
    Any -> io:format("deldir ~p~n", [ Any ])
    %_ -> false
  end,
  %io:format("deleting dir ~p~n", [ Dir ]).
  file:del_dir(Dir).

