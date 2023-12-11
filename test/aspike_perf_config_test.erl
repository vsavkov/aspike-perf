-module(aspike_perf_config_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/aspike_perf.hrl").

load_profile_test() ->
  No_file = "test/no_file.config",
  No_file_ret = aspike_perf_utils:load_config(No_file, abc),
  ?assertEqual({error,{file,No_file,enoent}}, No_file_ret),

  Config_01 = "test/profile_01.config",

  No_profile = xyz,
  No_profile_ret = aspike_perf_utils:load_config(Config_01, No_profile),
  ?assertEqual(
    {error, {profile, No_profile, not_found_in_config_file, Config_01}},
    No_profile_ret),

  No_user_profile = no_user,
  No_user_profile_ret = aspike_perf_utils:load_config(Config_01, No_user_profile),
  ?assertEqual({error,no_user}, No_user_profile_ret),

  No_password_profile = "no password",
  No_password_profile_ret = aspike_perf_utils:load_config(Config_01, No_password_profile),
  ?assertEqual({error,no_password}, No_password_profile_ret),

  With_password_profile = "with password",
  With_password_profile_ret = aspike_perf_utils:load_config(Config_01, With_password_profile),
  ?assertNotEqual({error,no_password}, With_password_profile_ret),

  With_password_file_no_file_profile = "with password file, no file",
  With_password_file_no_file_profile_ret = aspike_perf_utils:load_config(Config_01, With_password_file_no_file_profile),
  ?assertEqual({error,no_password}, With_password_file_no_file_profile_ret),

  With_password_file_password_short_profile = "with password file, password short",
  With_password_file_password_short_profile_ret = aspike_perf_utils:load_config(Config_01, With_password_file_password_short_profile),
  ?assertEqual({error,no_password}, With_password_file_password_short_profile_ret),

  With_password_file = "with password file",
  With_password_file_ret = aspike_perf_utils:load_config(Config_01, With_password_file),
  ?assertNotEqual({error,no_password}, With_password_file_ret).

normalize_seeds_test() ->
  Ret1 = aspike_perf_utils:normalize_seeds(123),
  ?assertEqual({seed, undefined}, Ret1),
  Ret2 = aspike_perf_utils:normalize_seeds([123]),
  ?assertEqual({seed, undefined}, Ret2),
  Ret3 = aspike_perf_utils:normalize_seeds({123}),
  ?assertEqual({seed, undefined}, Ret3),

  Seed_addr_1 = "127.0.0.1", Seed_port_1 = 3001,
  Seed_1 = {Seed_addr_1, Seed_port_1},
  Seed_1_exp = #aspike_endpoint{
    address = list_to_binary(Seed_addr_1), port = Seed_port_1},
  Seed_addr_2 = "127.0.0.2", Seed_port_2 = 3002,
  Seed_2 = {Seed_addr_2, Seed_port_2},

  Seed_1_ret = aspike_perf_utils:normalize_seeds(Seed_1),
  ?assertEqual({seed, Seed_1_exp}, Seed_1_ret),

  Seed_2_ret = aspike_perf_utils:normalize_seeds([Seed_1, Seed_2]),
  ?assertEqual({seed, Seed_1_exp}, Seed_2_ret),
  pass.

normalize_nodes_test() ->
  Ret1 = aspike_perf_utils:normalize_nodes(123),
  ?assertEqual({nodes, []}, Ret1),
  Ret2 = aspike_perf_utils:normalize_nodes([123]),
  ?assertEqual({nodes, []}, Ret2),
  Ret3 = aspike_perf_utils:normalize_nodes({123}),
  ?assertEqual({nodes, []}, Ret3),

  Node_addr_1 = "255.0.0.1", Node_port_1 = 7001,
  Node_1 = {Node_addr_1, Node_port_1},
  Node_1_exp = #aspike_endpoint{
    address = list_to_binary(Node_addr_1), port = Node_port_1},
  Node_addr_2 = "255.0.0.2", Node_port_2 = 7002,
  Node_2 = {Node_addr_2, Node_port_2},
  Node_2_exp = #aspike_endpoint{
    address = list_to_binary(Node_addr_2), port = Node_port_2},

  Node_1_ret = aspike_perf_utils:normalize_nodes(Node_1),
  ?assertEqual({nodes, [Node_1_exp]}, Node_1_ret),

  Node_2_ret = aspike_perf_utils:normalize_nodes([Node_1, Node_2]),
  ?assertEqual({nodes, [Node_1_exp, Node_2_exp]}, Node_2_ret),
  pass.

name_nodes_test() ->
  Node_addr_1 = "255.0.0.1", Node_port_1 = 7001,
  Node_1 = #aspike_endpoint{
    address = list_to_binary(Node_addr_1), port = Node_port_1},
  Node_addr_2 = "255.0.0.2", Node_port_2 = 7002,
  Node_2 = #aspike_endpoint{
    address = list_to_binary(Node_addr_2), port = Node_port_2},
  Node_addr_3 = "255.0.0.3", Node_port_3 = 7003, Node_name_3 = "A3",
  Node_3 = #aspike_endpoint{name = Node_name_3,
    address = list_to_binary(Node_addr_3), port = Node_port_3},

  [#aspike_endpoint{name = Name1}, #aspike_endpoint{name = Name2}]
    = aspike_perf_utils:name_nodes([Node_1, Node_2], "X", 0),
  ?assertEqual(<<"X0">>, Name1),
  ?assertEqual(<<"X1">>, Name2),

  [#aspike_endpoint{name = Name1},
    #aspike_endpoint{name = Name3},
    #aspike_endpoint{name = Name2}]
    = aspike_perf_utils:name_nodes([Node_1, Node_3, Node_2], "X", 0),
  ?assertEqual(<<"X0">>, Name1),
  ?assertEqual(<<"X1">>, Name2),
  ?assertEqual(list_to_binary(Node_name_3), Name3),
  pass.

seeds_nodes_test() ->
  Config_02 = "test/profile_02.config",
  User_expected = <<"User">>,
  Password_expected = <<"012345678901234567890123456789012345678901234567890123456789">>,
  Seed_address_expected = <<"127.0.0.1">>,
  Seed_port_expected = 3001,

  Node_addr_1 = "255.0.0.1", Node_port_1 = 7001,
  Node_1_exp = #aspike_endpoint{name = <<"X0">>,
    address = list_to_binary(Node_addr_1), port = Node_port_1},
  Node_addr_2 = "255.0.0.2", Node_port_2 = 7002,
  Node_2_exp = #aspike_endpoint{name = <<"X1">>,
    address = list_to_binary(Node_addr_2), port = Node_port_2},

  Seeds_no_nodes = "seeds, no nodes",
  Seeds_no_nodes_ret = aspike_perf_utils:load_config(Config_02, Seeds_no_nodes),
  ?assertMatch({ok, #aspike_perf_config{}}, Seeds_no_nodes_ret),
  {ok, #aspike_perf_config{seed = Seed_01, nodes = Nodes_01}} = Seeds_no_nodes_ret,
  ?assertEqual(
    #aspike_endpoint{address = Seed_address_expected, port = Seed_port_expected},
    Seed_01),
  ?assertEqual([], Nodes_01),

  No_seeds_node = "no seeds, node",
  No_seeds_node_ret = aspike_perf_utils:load_config(Config_02, No_seeds_node),
  ?assertMatch({ok, #aspike_perf_config{}}, No_seeds_node_ret),
  {ok, #aspike_perf_config{seed = Seed_02, nodes = Nodes_02}} = No_seeds_node_ret,
  ?assertEqual(undefined, Seed_02),
  ?assertEqual([Node_1_exp], Nodes_02),

  Seeds_nodes = "seeds, nodes",
  Seeds_nodes_ret = aspike_perf_utils:load_config(Config_02, Seeds_nodes),
  ?assertMatch({ok, #aspike_perf_config{}}, Seeds_nodes_ret),
  {ok, #aspike_perf_config{user = User,
    seed = Seed_03, nodes = Nodes_03}}
    = Seeds_nodes_ret,
  ?assertEqual(#aspike_user{name = User_expected, credential = Password_expected}, User),
  ?assertEqual(
    #aspike_endpoint{address = Seed_address_expected, port = Seed_port_expected},
    Seed_03),
  ?assertEqual([Node_1_exp, Node_2_exp], Nodes_03),

  pass.

perf_config_test() ->
  Config_03 = "test/profile_03.config",

  Full_config = "full config",
  Full_config_ret = aspike_perf_utils:load_config(Config_03, Full_config),
  ?assertMatch({ok, #aspike_perf_config{}}, Full_config_ret),
  {ok, #aspike_perf_config{
    connection_options = Connection_options,
    pool_options = Pool_options,
    socket_options = Socket_options}}
    = Full_config_ret,
  ?assertMatch(#aspike_connection_options{}, Connection_options),
  ?assertMatch(#aspike_pool_options{}, Pool_options),
  ?assert(is_list(Socket_options)),

  pass.

discover_nodes_test() ->

  No_seed_no_nodes = #aspike_perf_config{seed = undefined, nodes = []},
  No_seed_no_nodes_ret = aspike_perf_utils:discover_nodes(No_seed_no_nodes),
  ?assertEqual({error, seed_is_not_provided_to_discover_nodes}, No_seed_no_nodes_ret),

  No_seed_nodes = #aspike_perf_config{
    seed = undefined,
    nodes = [?BOGUS_ENDPOINT_3, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_1,
      ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5]},
  No_seed_nodes_ret = aspike_perf_utils:discover_nodes(No_seed_nodes),
  ?assertMatch({ok, #aspike_perf_config{}}, No_seed_nodes_ret),
  {ok, #aspike_perf_config{nodes = Nodes}} = No_seed_nodes_ret,
  ?assertEqual([?BOGUS_ENDPOINT_1, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_3,
    ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5], Nodes),

  Seed_no_nodes = #aspike_perf_config{
    user = #aspike_user{},
    seed = #aspike_endpoint{address = ?BOGUS_ADDRESS_RETURN_NODES},
    nodes = []},
  Seed_no_nodes_ret = aspike_perf_utils:discover_nodes(Seed_no_nodes),
  ?assertMatch({ok, #aspike_perf_config{}}, Seed_no_nodes_ret),
  {ok, #aspike_perf_config{nodes = Nodes1}} = Seed_no_nodes_ret,
  ?assertEqual([?BOGUS_ENDPOINT_1, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_3,
    ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5], Nodes1),

  Seed_error_no_nodes = #aspike_perf_config{
    user = #aspike_user{},
    seed = #aspike_endpoint{address = ?BOGUS_ADDRESS_RETURN_ERROR},
    nodes = []},
  Seed_error_no_nodes_ret = aspike_perf_utils:discover_nodes(Seed_error_no_nodes),
  ?assertEqual({error, ?BOGUS_ADDRESS_RETURN_ERROR}, Seed_error_no_nodes_ret),

  Seed_one_node = #aspike_perf_config{
    seed = #aspike_endpoint{},
    nodes = [?BOGUS_ENDPOINT_3]},
  Seed_one_node_ret = aspike_perf_utils:discover_nodes(Seed_one_node),
  ?assertMatch({ok, #aspike_perf_config{}}, Seed_one_node_ret),
  {ok, #aspike_perf_config{nodes = Nodes2}} = Seed_one_node_ret,
  ?assertEqual([?BOGUS_ENDPOINT_3], Nodes2),

  Seed_many_nodes = #aspike_perf_config{
    seed = #aspike_endpoint{},
    nodes = [?BOGUS_ENDPOINT_3, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_1,
      ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5]},
  Seed_many_nodes_ret = aspike_perf_utils:discover_nodes(Seed_many_nodes),
  ?assertMatch({ok, #aspike_perf_config{}}, Seed_many_nodes_ret),
  {ok, #aspike_perf_config{nodes = Nodes3}} = Seed_many_nodes_ret,
  ?assertEqual([?BOGUS_ENDPOINT_1, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_3,
    ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5], Nodes3),

  pass.

output_config_test() ->
  Config_03 = "test/profile_03.config",

  Full_config = "full config",
  Full_config_ret = aspike_perf_utils:load_config(Config_03, Full_config),
  ?assertMatch({ok, #aspike_perf_config{}}, Full_config_ret),
  {ok, Config} = Full_config_ret,

  Discover_nodes_ret = aspike_perf_utils:discover_nodes(Config),
  ?assertMatch({ok, #aspike_perf_config{}}, Discover_nodes_ret),
  {ok, Config1} = Discover_nodes_ret,

%%  ?debugFmt("Output:~n~p~n", [Config1]),
  Output_config = aspike_perf_utils:output_config(Config1),
%%  ?debugFmt("Output:~n~p~n", [Output]),
  [User_output, Seed_output, [<<"Nodes:">> | Nodes_output],
    [Connection_options_output, Socket_options_output],
    Pool_options_output] = Output_config,
  ?assert(is_binary(User_output)),
  ?assert(is_binary(Seed_output)),
  ?assert(is_list(Nodes_output)),
  ?assert(is_binary(Connection_options_output)),
  ?assert(is_binary(Socket_options_output)),
  ?assert(is_binary(Pool_options_output)),

  pass.
