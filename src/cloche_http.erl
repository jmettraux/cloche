
-module(cloche_http).

-export([start/3, stop/1]).

start(Name, Port, Cloche) ->
  mochiweb_http:start([
    { name, Name },
    { port, Port },
    { loop, fun(Req) -> handle(Req, Cloche) end } ]),
  io:format("cloche_http listening on ~p~n", [ Port ]).

stop(Name) ->
  mochiweb_http:stop(Name).

%
% handle
%

handle(Req, Cloche) ->
  try
    [ _ | Path ] = re:split(Req:get(path), "\/", [ { return, list } ]),
    handle(Req:get(method), Path, Req, Cloche)
  catch
    A:B ->
      %io:format("error : ~p ~p~n", [ A, B ]),
      io:format("error : ~p ~p~n~p~n", [ A, B, erlang:get_stacktrace() ]),
      error(Req, "intercepted error")
  end.

%
% GET
%

handle('GET', [ Type, Id | _ ], Req, Cloche) ->
  case cloche:do_get(Cloche, Type, Id) of
    undefined -> not_found(Req);
    Json -> ok(Req, Json)
  end;

%
% PUT
%

handle('PUT', _, Req, Cloche) ->
  Body = binary_to_list(Req:recv_body(1024 * 1024)),
  case cloche:do_put(Cloche, Body) of
    ok ->
      ok(Req);
    Json -> 
      ok(Req, Json)
  end;

%
% DELETE
%

handle('DELETE', [ Type ], Req, Cloche) ->
  cloche:clear_type(Cloche, Type),
  ok(Req);

handle('DELETE', [ Type, Id ], Req, Cloche) ->
  case lists:keyfind("rev", 1, Req:parse_qs()) of
    { "rev", Rev } ->
      handle('DELETE', [ Type, Id, Rev ], Req, Cloche);
    false ->
      error(Req, 400, "missing rev")
  end;

handle('DELETE', [ Type, Id, Rev ], Req, Cloche) ->
  case cloche:do_delete(Cloche, Type, Id, list_to_integer(Rev)) of
    ok ->
      ok(Req);
    Json -> 
      ok(Req, Json)
  end;

%
% catchall
%

handle(M, P, Req, _) ->
  error(Req, io_lib:format("not implemented : ~p ~p", [ M, P ])).

%
% helpers
%

do_log(Req, Code) ->
  io:format(
    "~s ~s ~p ~s~n",
    [ Req:get(method), Req:get(raw_path), Code, Req:get(peer) ]).

ok(Req) ->
  ok(Req, "application/json", "{\"ok\":true}").

ok(Req, Body) ->
  ok(Req, "application/json", Body).

ok(Req, ContentType, Body) ->
  do_log(Req, 200),
  Req:respond({ 200, [{ "Content-Type", ContentType }], Body }).

not_found(Req) ->
  do_log(Req, 404),
  Req:respond({ 404, [{ "Content-Type", "text/plain" }], "not found." }).

error(Req, Code, Msg) ->
  do_log(Req, Code),
  Req:respond({ Code, [{ "Content-Type", "text/plain" }], "error : " ++ Msg }).

error(Req, Msg) ->
  error(Req, 500, Msg).

