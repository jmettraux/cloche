
-module(cloche).
-export([start/1, shutdown/1]).
-export([read/3, write/2, delete/3]).

read(Registry, Type, Id) ->
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), read }).

write(Registry, Doc) ->
  { Type, Id } = extract_type_and_id(Doc),
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), write, Doc }).

delete(Registry, Type, Id) ->
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), delete }).

rpc(Pid, Request) ->
  Pid ! Request,
  receive
    Response -> Response
  end.

extract_type_and_id(Doc) ->
  { json_value(Doc, "type"), json_value(Doc, "_id") }.

%
% playing the role of a lock
%
file_loop(Registry, Dir, Type, Id) ->

  TypeDir = filename:join([ Dir, Type ]),
  Path = filename:join([ Dir, Type, Id ++ ".json" ]),

  receive

    { touch } ->
      file_loop(Registry, Dir, Type, Id);

    { From, read } ->
      case file:read_file(Path) of
        { ok, Doc } -> From ! Doc;
        _ -> From ! undefined
      end,
      file_loop(Registry, Dir, Type, Id);

    { From, write, Doc } ->
      file:make_dir(TypeDir),
      R = file:write_file(Path, Doc),
      From ! R,
      file_loop(Registry, Dir, Type, Id);

    { From, delete } ->
      R = file:delete(Path),
      From ! R,
      file_loop(Registry, Dir, Type, Id)

  after 2000 ->
    Registry ! { delf, Type, Id }
  end.

%
% the file_registry makes sure there's 1! file_lock err... file_loop per
% file.
%
file_registry_loop(Dir) ->

  receive

    { From, getf, Type, Id } ->
      case get({ Type, Id }) of
        undefined ->
          Pid = spawn(fun() -> file_loop(self(), Dir, Type, Id) end),
          put({ Type, Id }, Pid),
          From ! Pid;
        Pid ->
          Pid ! { touch },
          From ! Pid
      end,
      file_registry_loop(Dir);

    { delf, Type, Id } ->
      erase({ Type, Id }),
      file_registry_loop(Dir);

    shutdown ->
      ok
  end.

start(Dir) ->
  file:make_dir(Dir),
  spawn(fun() -> file_registry_loop(Dir) end).

shutdown(Pid) ->
  Pid ! shutdown.

%
% Extracts the value for a 
%
json_value(JsonString, Key) ->

  { ok, Re } = re:compile("\"?" ++ Key ++ "\"? *: *\"([^\"]+)"),
  % TODO : multiline aware

  { match, [ _ | Cdr ] } = re:run(JsonString, Re),
  [{ Start, Length }] = Cdr,
  string:substr(JsonString, Start+1, Length).

