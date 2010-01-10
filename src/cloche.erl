
-module(cloche).
-export([start/1, shutdown/1]).
-export([do_get/3, do_put/2, do_delete/2, do_delete/4]).

do_get(Registry, Type, Id) ->
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), do_get }).

do_put(Registry, Doc) ->
  { Type, Id, Rev } = extract_tir(Doc),
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), do_put, Doc, Rev }).

do_delete(Registry, Doc) ->
  { Type, Id, Rev } = extract_tir(Doc),
  do_delete(Registry, Type, Id, Rev).

do_delete(Registry, Type, Id, Rev) ->
  F = rpc(Registry, { self(), getf, Type, Id }),
  rpc(F, { self(), do_delete, Rev }).

rpc(Pid, Request) ->
  Pid ! Request,
  receive
    Response -> Response
  end.

extract_tir(Doc) ->
  { cloche_utils:json_get(Doc, "type"),
    cloche_utils:json_get(Doc, "_id"),
    cloche_utils:json_get(Doc, "_rev") }.

get_file(Path) ->
  case file:read_file(Path) of
    { ok, Data } ->
      Doc = binary_to_list(Data),
      { Type, Id, Rev } = extract_tir(Doc),
      { Type, Id, Rev, Doc };
    _ ->
      { undefined, undefined, undefined, undefined }
  end.

get_path(Dir, Type, Id) ->
  { filename:join([ Dir, Type ]),
    filename:join([ Dir, Type, Id ++ ".json" ]) }.

write_doc(TypePath, DocPath, Doc) ->
  file:make_dir(TypePath),
  file:write_file(DocPath, Doc).

inc_rev(Doc, undefined) ->
  inc_rev(Doc, "-1");
inc_rev(Doc, Rev) ->
  NewRev = integer_to_list(list_to_integer(Rev) + 1),
  cloche_utils:json_set(Doc, "_rev", NewRev).

%
% playing the role of a lock
%
file_loop(Registry, Dir, Type, Id) ->

  { TypePath, DocPath } = get_path(Dir, Type, Id),

  receive

    { touch } ->
      file_loop(Registry, Dir, Type, Id);

    { From, do_get } ->
      { _, _, _, Doc } = get_file(DocPath),
      From ! Doc,
      file_loop(Registry, Dir, Type, Id);

    { From, do_put, Doc, Rev } ->
      { _, _, CurrentRev, CurrentDoc } = get_file(DocPath),
      if
        CurrentRev =:= Rev ->
          write_doc(TypePath, DocPath, inc_rev(Doc, Rev)),
          From ! ok;
        true ->
          From ! CurrentDoc
      end,
      file_loop(Registry, Dir, Type, Id);

    { From, do_delete, Rev } ->
      R = file:delete(DocPath),
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

