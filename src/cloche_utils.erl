
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

% get

json_do_get([], _) ->
  undefined;

json_do_get([ Entry | Rest ], Key) ->
  { K, Value } = Entry,
  Sk = binary_to_list(K),
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

% set

to_binary (Value) when is_list(Value) -> list_to_binary(Value);
to_binary (Value) -> Value.


json_set(JsonString, Key, Value) ->
  { struct, E0 } = mochijson2:decode(JsonString),
  E1 = lists:filter(fun({ K, _ }) -> binary_to_list(K) =/= Key end, E0),
  E2 = [ { list_to_binary(Key), to_binary(Value) } | E1 ],
  binary_to_list(iolist_to_binary(mochijson2:encode({ struct, E2 }))).

