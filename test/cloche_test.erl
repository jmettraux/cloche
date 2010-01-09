
-module(cloche_test).
-include_lib("eunit/include/eunit.hrl").

cloche_test_() ->
  { setup,
    fun() ->
      cloche:start("work_test")
    end,
    fun(Pid) ->
      cloche:shutdown(Pid)
    end,
    fun write_new/1 }.

write_new(Pid) ->

  cloche:write(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}"),

  ?_assertEqual(
    { ok, <<"{\"_id\":\"toto\",\"type\":\"person\"}">> },
    file:read_file("work_test/person/toto.json")).

