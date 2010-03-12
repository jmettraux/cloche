
-module(cloche_utils_test).
-include_lib("eunit/include/eunit.hrl").

clear_dir_test_() ->
  { setup,
    fun() ->
      file:make_dir("cu_test"),
      file:make_dir("cu_test/person"),
      file:make_dir("cu_test/person/na"),
      file:write_file("cu_test/person/na/nada.json", "nada"),
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
      undefined, cloche_utils:json_get("{\"_id\":\"toto\"}", "colour")),
    ?_assertEqual(
      undefined, cloche_utils:json_get("{\"_id\":\"toto\",\"type\":\"person\"}", "_rev")),
    ?_assertEqual(
      2, cloche_utils:json_get("{\"_id\":\"toto\",\"type\":\"person\",\"_rev\":2}", "_rev")) ].

json_set_test_() ->
  [ ?_assertEqual(
      "{\"_rev\":\"2\",\"_id\":\"toto\"}",
      cloche_utils:json_set("{\"_id\":\"toto\"}", "_rev", "2")),
    ?_assertEqual(
      "{\"_rev\":2,\"_id\":\"toto\"}",
      cloche_utils:json_set("{\"_id\":\"toto\",\"_rev\":0}", "_rev", 2)) ].

get_set_test() ->
  Doc0 = "{\"_id\":\"toto\",\"_rev\":2}",
  Doc1 = cloche_utils:json_set(
    Doc0, "_rev", integer_to_list(cloche_utils:json_get(Doc0, "_rev") + 1)),
  ?_assertEqual("{\"_rev\":3,\"_id\":\"toto\"}", Doc1).

