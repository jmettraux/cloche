
-module(cloche_utils_test).
-include_lib("eunit/include/eunit.hrl").

clear_dir_test_() ->
  { setup,
    fun() ->
      file:make_dir("cu_test"),
      file:make_dir("cu_test/person"),
      file:write_file("cu_test/person/nada.json", "nada"),
      cloche_utils:clear_dir("cu_test/person")
    end,
    fun(_) ->
      file:del_dir("cu_test")
    end,
    fun generate_clear_dir_tests/1 }.

generate_clear_dir_tests(_) ->
  [ ?_assertEqual({ error, enoent }, file:read_file_info("cu_test/person")) ].

json_get_test_() ->
  [ ?_assertEqual(
    "toto", cloche_utils:json_get("{\"_id\":\"toto\"}", "_id")),
    ?_assertEqual(
    "toto", cloche_utils:json_get("{_id:\"toto\"}", "_id")),
    ?_assertEqual(
    undefined, cloche_utils:json_get("{_id:\"toto\"}", "colour")) ].

json_set_test_() ->
  [ ?_assertEqual(
      "{\"_id\":\"toto\",\"_rev\":2}",
      cloche_utils:json_set("{\"_id\":\"toto\"}", "_rev", "2")),
    ?_assertEqual(
      "{\"_id\":\"toto\",\"_rev\":2}",
      cloche_utils:json_set("{\"_id\":\"toto\",\"_rev\":0}", "_rev", "2")),
    ?_assertEqual(
      "{\"_id\":\"toto\",\"_rev\":2}",
      cloche_utils:json_set("{\"_id\":\"toto\",_rev:0}", "_rev", "2")) ].

