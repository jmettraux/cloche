
-module(cloche_utils).
-export([clear_dir/1]).
-export([json_get/2, json_set/3]).

clear_files(Dir, [ Filename | Filenames ]) ->
  Fn = filename:join([ Dir, Filename ]),
  case file:read_file_info(Fn) of
    { ok, { file_info, _, regular, _, _, _, _, _, _, _, _, _, _, _ } } ->
      %io:format("deleting file ~p~n", [ Fn ]),
      file:delete(Fn),
      clear_files(Dir, Filenames);
    { ok, { file_info, _, directory, _, _, _, _, _, _, _, _, _, _, _ } } ->
      clear_dir(Fn);
    %Any ->
    %  io:format("can't clear file ~p : ~p~n", [ Filename, Any ])
    _ ->
      false
  end;
clear_files(_, []) ->
  true.

clear_dir(Dir) ->
  case file:list_dir(Dir) of
    { ok, Filenames } -> clear_files(Dir, Filenames);
    %Any -> io:format("deldir ~p~n", [ Any ])
    _ -> false
  end,
  %io:format("deleting dir ~p~n", [ Dir ]).
  file:del_dir(Dir).


%
% JSON micro stuff
%
% sooner or later, replace with ejson or mochijson
%

json_do_get([], _) ->
  undefined;

json_do_get([ Entry | Rest ], Key) ->
  { K, Value } = Entry,
  Sk = binary_to_list(K),
  io:format(" * ~p ==> ~p~n", [ Sk, Value ]),
  case Sk of
    Key -> if
      is_binary(Value) -> binary_to_list(Value);
      true -> Value
    end;
    _ -> json_do_get(Rest, Key)
  end;

json_do_get({ struct, Entries }, Key) ->
  json_do_get(Entries, Key).

json_get(JsonString, Key) ->
  json_do_get(mochijson2:decode(JsonString), Key).

json_set(JsonString, Key, Value) ->

  KeyVal = "\"" ++ Key ++ "\":" ++ Value,
  { ok, Re } = re:compile("\"?" ++ Key ++ "\"? *: *[^,\\}]+"),

  case re:run(JsonString, Re) of
    { match, _ } ->
      re:replace(JsonString, Re, KeyVal, [ { return, list } ]);
    _ ->
      string:substr(JsonString, 1, length(JsonString) - 1) ++ "," ++ KeyVal ++ "}"
  end.

