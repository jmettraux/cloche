
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

