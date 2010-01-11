#!/usr/bin/env escript

main([ Port, ClocheDir ]) ->

  code:add_patha("ebin"),
  Cloche = cloche:start(ClocheDir),
  cloche_http:start("http", list_to_integer(Port), Cloche),

  receive
    _ -> true
  end.

