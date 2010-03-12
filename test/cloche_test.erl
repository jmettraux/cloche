
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
      "{\"_rev\":0,\"_id\":\"toto\",\"type\":\"person\"}",
      cloche:do_get(Pid, "person", "toto")),
    ?_assertEqual(
      undefined,
      cloche:do_get(Pid, "person", "nemo")) ].


write_test_() ->
  { foreach,
    fun() ->
      file:make_dir("work_test"),
      file:make_dir("work_test/person"),
      file:make_dir("work_test/person/to"),
      file:write_file(
        "work_test/person/to/toto.json",
        "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}"),
      cloche:start("work_test")
    end,
    fun(Pid) ->
      cloche:shutdown(Pid),
      cloche_utils:clear_dir("work_test"),
      cloche_utils:clear_dir("work_test")
        % why oh why should I have this 2 times here !
        % of course, it's my fault
    end,
    [ fun wt_write_new/1,
      fun wt_write_with_wrong_rev/1,
      fun wt_write/1,
      fun wt_write2/1 ] }.

wt_write_new(Pid) ->
  ?_assertEqual(
    ok,
    cloche:do_put(Pid, "{\"_id\":\"jeff\",\"type\":\"person\"}")).

wt_write_with_wrong_rev(Pid) ->
  ?_assertEqual(
    "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}",
    cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}")).

  % there should be only one ?_assertEqual

wt_write(Pid) ->
  cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}"),
  ?_assertEqual(
    { ok, <<"{\"_rev\":3,\"_id\":\"toto\",\"type\":\"person\"}">> },
    file:read_file("work_test/person/to/toto.json")).

wt_write2(Pid) ->
  cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}"),
  ?_assertEqual(
    "{\"_rev\":3,\"_id\":\"toto\",\"type\":\"person\"}",
    cloche:do_get(Pid, "person", "toto")).


delete_test_() ->
  { foreach,
    fun() ->
      Pid = cloche:start("work_test"),
      cloche:do_put(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}"),
      Pid
    end,
    fun(Pid) ->
      cloche:shutdown(Pid),
      cloche_utils:clear_dir("work_test")
    end,
    [ fun dt_delete_with_wrong_rev/1,
      fun dt_delete_ok/1,
      fun dt_delete_missing/1 ] }.

dt_delete_with_wrong_rev(Pid) ->
  ?_assertEqual(
    "{\"_rev\":0,\"_id\":\"toto\",\"type\":\"person\"}",
    cloche:do_delete(Pid, "person", "toto", 1)).

dt_delete_ok(Pid) ->
  ?_assertEqual(
    ok,
    cloche:do_delete(Pid, "person", "toto", 0)).

dt_delete_missing(Pid) ->
  ?_assertEqual(
    ok,
    cloche:do_delete(Pid, "person", "nemo", 0)).

