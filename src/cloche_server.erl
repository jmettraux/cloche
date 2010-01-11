
-module(cloche_server).

-export([start/2, stop/0]).

start(Port, ClocheDir) ->
  Cloche = cloche:start(ClocheDir),
  mochiweb_http:start([
    { port, Port },
    { loop, fun(Req) -> handle(Req:get(method), Req, Cloche) end } ]).

stop() ->
  % TODO : stop the cloche...
  mochiweb_http:stop().

%handle(Req) ->
%  handle(Req:get(method), Req).
  %io:format("===== ~p~n", [ Req:get(method) ]),
  %Req:ok({ "text/html", "toto" }).

handle('GET', Req, Cloche) ->
  %Req:ok({ "text/html", "GOT it" }).
  %Req:serve_file("toto.json", ".").
  case re:split(Req:get(path), "\/", [ { return, list } ]) of
    [ [], Type, Id | _ ] ->
      case cloche:do_get(Cloche, Type, Id) of
        undefined ->
          Req:respond({ 404, [{ "Content-Type", "text/plain" }], "not found." });
        Json ->
          Req:ok({ "application/json", Json })
      end;
    Any ->
      Req:ok({ "text/plain", "I don't get it : " ++ Any })
  end.

