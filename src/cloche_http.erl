
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


handle(Req, Cloche) ->
  handle(Req:get(method), Req, Cloche).

handle('GET', Req, Cloche) ->
  %io:format("GET ~p~n", [ Req ]),
  case re:split(Req:get(path), "\/", [ { return, list } ]) of
    [ [], Type, Id | _ ] ->
      case cloche:do_get(Cloche, Type, Id) of
        undefined -> not_found(Req);
        Json -> ok(Req, Json)
      end;
    Any -> error(Req, "I don't get it : " ++ Any)
  end.

%
% helpers
%

do_log(Req, Code) ->
  io:format("~s ~s ~p~n", [ Req:get(method), Req:get(path), Code ]).

ok(Req, Body) ->
  ok(Req, "application/json", Body).

ok(Req, ContentType, Body) ->
  do_log(Req, 200),
  Req:respond({ 200, [{ "Content-Type", ContentType }], Body }).

not_found(Req) ->
  do_log(Req, 404),
  Req:respond({ 404, [{ "Content-Type", "text/plain" }], "not found." }).

error(Req, Msg) ->
  do_log(Req, 500),
  Req:respond({ 500, [{ "Content-Type", "text/plain" }], "error : " ++ Msg }).

