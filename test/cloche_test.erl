
-module(cloche_test).
-include_lib("eunit/include/eunit.hrl").

read_test_() ->
  { setup,
    fun() ->
      Pid = cloche:start("work_test"),
      cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}"),
      Pid
    end,
    fun(Pid) ->
      cloche:shutdown(Pid),
      cloche_utils:clear_dir("work_test")
    end,
    fun generate_read_tests/1 }.

generate_read_tests(Pid) ->
  [ ?_assertEqual(
      "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":0}",
      cloche:do_get(Pid, "person", "toto")),
    ?_assertEqual(
      undefined,
      cloche:do_get(Pid, "person", "nemo")) ].

write_test_() ->
  { setup,
    fun() ->
      file:make_dir("work_test"),
      file:make_dir("work_test/person"),
      file:write_file(
        "work_test/person/toto.json",
        "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}"),
      cloche:start("work_test")
    end,
    fun(Pid) ->
      cloche:shutdown(Pid),
      cloche_utils:clear_dir("work_test")
    end,
    fun generate_write_tests/1 }.

generate_write_tests(Pid) ->
  [ ?_assertEqual(
      "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}",
      cloche:do_get(Pid, "person", "toto")),
    ?_assertEqual(
      ok,
      cloche:do_put(Pid, "{\"_id\":\"jeff\",\"type\":\"person\"}")),
    ?_assertEqual(
      "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}",
      cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}")),
    ?_assertEqual(
      ok,
      cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}")) ].

