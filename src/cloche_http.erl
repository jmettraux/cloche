
-module(cloche_http).

-export([start/3, stop/1]).

start(Name, Port, Cloche) ->
  mochiweb_http:start([
    { name, Name },
    { port, Port },
    { loop, fun(Req) -> handle(Req, Cloche) end } ]).

stop(Name) ->
  mochiweb_http:stop(Name).

%handle(Req) ->
  %Req:serve_file("toto.json", ".").
  %handle(Req:get(method), Req).
  %io:format("===== ~p~n", [ Req:get(method) ]),
  %Req:ok({ "text/html", "toto" }).

handle(Req, Cloche) ->
  handle(Req:get(method), Req, Cloche).

handle('GET', Req, Cloche) ->
  %io:format("GET ~p~n", [ Req ]),
  case re:split(Req:get(path), "\/", [ { return, list } ]) of
    [ [], Type, Id | _ ] ->
      case cloche:do_get(Cloche, Type, Id) of
        undefined -> not_found(Req);
        Json -> Req:ok({ "application/json", Json })
      end;
    Any -> error(Req, "I don't get it : " ++ Any)
  end.

%
% helpers
%

not_found(Req) ->
  Req:respond({ 404, [{ "Content-Type", "text/plain" }], "not found." }).

error(Req, Msg) ->
  Req:respond({ 500, [{ "Content-Type", "text/plain" }], "error : " ++ Msg }).

