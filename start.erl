#!/usr/bin/env escript

%
% usage :
%
% ./start.erl {port} {cloche_dir}
%
% like :
%
% ./start.erl 9000 htest
%

main([ Port, ClocheDir ]) ->

  code:add_patha("ebin"),
  Cloche = cloche:start(ClocheDir),
  cloche_http:start("http", list_to_integer(Port), Cloche),

  receive
    _ -> true
  end.

