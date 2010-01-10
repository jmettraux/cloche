
-module(cloche_test).
-include_lib("eunit/include/eunit.hrl").

%write_test_() ->

read_test_() ->
  { setup,
    fun() ->
      Pid = cloche:start("work_test"),
      cloche:write(Pid, "{\"_id\":\"toto\",\"type\":\"person\"}"),
      Pid
    end,
    fun(Pid) ->
      cloche:shutdown(Pid),
      cloche_utils:clear_dir("work_test")
    end,
    fun generate_read_tests/1 }.

generate_read_tests(Pid) ->
  [ ?_assertEqual(
      <<"{\"_id\":\"toto\",\"type\":\"person\"}">>,
      cloche:read(Pid, "person", "toto")),
    ?_assertEqual(
      undefined,
      cloche:read(Pid, "person", "nemo")) ].

