
-module(cloche).
-export([start/1, shutdown/1]).
-export([do_get/3, do_put/2, do_delete/2, do_delete/4]).


%
% exported methods
%

do_get(Cloche, Type, Id) ->
  F = rpc(Cloche, { self(), getf, Type, Id }),
  rpc(F, { self(), do_get }).

do_put(Cloche, Doc) ->
  { Type, Id, Rev } = extract_tir(Doc),
  F = rpc(Cloche, { self(), getf, Type, Id }),
  rpc(F, { self(), do_put, Doc, Rev }).

do_delete(Cloche, Doc) ->
  { Type, Id, Rev } = extract_tir(Doc),
  do_delete(Cloche, Type, Id, Rev).

do_delete(Cloche, Type, Id, Rev) ->
  F = rpc(Cloche, { self(), getf, Type, Id }),
  rpc(F, { self(), do_delete, Rev }).

start(Dir) ->
  file:make_dir(Dir),
  spawn(fun() -> file_registry_loop(Dir) end).

shutdown(Pid) ->
  Pid ! shutdown.


%
% the rest
%

rpc(Pid, Request) ->
  Pid ! Request,
  receive
    Response -> Response
  end.

extract_tir(Doc) ->
  { cloche_utils:json_get(Doc, "type"),
    cloche_utils:json_get(Doc, "_id"),
    cloche_utils:json_get_int(Doc, "_rev") }.

get_file(Path) ->
  case file:read_file(Path) of
    { ok, Data } ->
      Doc = binary_to_list(Data),
      { Type, Id, Rev } = extract_tir(Doc),
      { Type, Id, Rev, Doc };
    _ ->
      { undefined, undefined, undefined, undefined }
  end.

last_two_chars(Id) ->
  if
    length(Id) > 1 ->
      string:sub_string(Id, length(Id) - 1);
    true ->
      string:right(Id, 2, "Z")
  end.

get_path(Dir, Type, Id) ->
  TypePath = filename:join([ Dir, Type ]),
  IdPath = filename:join([ TypePath, last_two_chars(Id) ]),
  DocPath = filename:join([ IdPath, Id ++ ".json" ]),
  { TypePath, IdPath, DocPath }.

write_doc(TypePath, IdPath, DocPath, Doc) ->
  file:make_dir(TypePath),
  file:make_dir(IdPath),
  file:write_file(DocPath, Doc).

inc_rev(Doc, undefined) ->
  inc_rev(Doc, -1);
inc_rev(Doc, Rev) ->
  cloche_utils:json_set(Doc, "_rev", integer_to_list(Rev + 1)).

%
% playing the role of a lock
%
file_loop(Cloche, Dir, Type, Id) ->

  { TypePath, IdPath, DocPath } = get_path(Dir, Type, Id),

  receive

    { touch } ->
      file_loop(Cloche, Dir, Type, Id);

    { From, do_get } ->
      { _, _, _, Doc } = get_file(DocPath),
      From ! Doc,
      file_loop(Cloche, Dir, Type, Id);

    { From, do_put, Doc, Rev } ->
      { _, _, CurrentRev, CurrentDoc } = get_file(DocPath),
      if
        CurrentRev == Rev ->
          write_doc(TypePath, IdPath, DocPath, inc_rev(Doc, Rev)),
          From ! ok;
        true ->
          From ! CurrentDoc
      end,
      file_loop(Cloche, Dir, Type, Id);

    { From, do_delete, Rev } ->
      { _, _, CurrentRev, CurrentDoc } = get_file(DocPath),
      if
        CurrentDoc == undefined ->
          From ! ok;
        CurrentRev == Rev ->
          R = file:delete(DocPath),
          From ! R; % ok or { error, ... }
        true ->
          From ! CurrentDoc
      end,
      file_loop(Cloche, Dir, Type, Id)

  after 2000 ->
    over
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
          case erlang:is_process_alive(Pid) of
            true ->
              Pid ! { touch },
              From ! Pid;
            false ->
              NewPid = spawn(fun() -> file_loop(self(), Dir, Type, Id) end),
              put({ Type, Id }, NewPid),
              From ! NewPid
          end
      end,
      file_registry_loop(Dir);

    shutdown ->
      ok
  end.

