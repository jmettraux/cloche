
-module(cloche).
-export([start/0]).
-export([read/3, write/2, delete/3]).
-export([test/0]).

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
file_loop(Registry, Type, Id) ->

  Path = filename:join([ Type, Id ++ ".json" ]),

  receive

    { touch } ->
      file_loop(Registry, Type, Id);

    { From, read } ->
      case file:read_file(Path) of
        { ok, Doc } -> From ! Doc;
        _ -> From ! undefined
      end,
      file_loop(Registry, Type, Id);

    { From, write, Doc } ->
      file:make_dir(Type),
      R = file:write_file(Path, Doc),
      From ! R,
      file_loop(Registry, Type, Id);

    { From, delete } ->
      R = file:delete(Path),
      From ! R,
      file_loop(Registry, Type, Id)

  after 2000 ->
    Registry ! { delf, Type, Id }
  end.

%
% the file_registry makes sure there's 1! file_lock err... file_loop per
% file.
%
file_registry_loop() ->

  receive

    { From, getf, Type, Id } ->
      case get({ Type, Id }) of
        undefined ->
          Pid = spawn(fun() -> file_loop(self(), Type, Id) end),
          put({ Type, Id }, Pid),
          From ! Pid;
        Pid ->
          Pid ! { touch },
          From ! Pid
      end,
      file_registry_loop();

    { delf, Type, Id } ->
      erase({ Type, Id }),
      file_registry_loop()
  end.

start() ->
  spawn(fun file_registry_loop/0).

%
% Extracts the value for a 
%
json_value(JsonString, Key) ->

  { ok, Re } = re:compile("\"?" ++ Key ++ "\"? *: *\"([^\"]+)"),
  % TODO : multiline aware

  { match, [ _ | Cdr ] } = re:run(JsonString, Re),
  [{ Start, Length }] = Cdr,
  string:substr(JsonString, Start+1, Length).

test() ->
  C = start(),
  write(C, "{_id:\"nada\",type:\"surf\"}"),
  read(C, "surf", "nada").
  %read(C, "ZERO", "nada").

