-module(aspike_perf).

-include("../include/aspike_perf.hrl").
-include("../include/aspike_perf_task.hrl").

%% performance API
-export([
  run_op/1,
  run_ops/1,
  run_scenario/1
]).

%% generic API
-export([
  start/2,
  start_nodes/1
]).

%% utils
-export([
]).

start(Config_file, Profile_name) ->
  case aspike_perf_utils:load_config(Config_file, Profile_name) of
    {error, _Reason} = Err -> Err;
    {ok, Config} ->
      case aspike_perf_utils:discover_nodes(Config) of
        {error, _Reason} = Err -> Err;
        {ok, Config1} ->
          Output_config = aspike_perf_utils:output_config(Config1),
          io:format("Effective config:~n~p~nEffective config. End~n", [Output_config]),
          case aspike_perf_utils:mk_node_params(Config1) of
            {error, _Reason} = Err -> Err;
            {ok, Nodes_params} ->
              Id_pool_size = [
                {aspike_perf_utils:mk_node_id(Name, Address, Port), Pool_size}
                || #aspike_node_params{endpoint = #aspike_endpoint{
                    name = Name, address = Address, port = Port},
                  pool_options = #aspike_pool_options{
                    pool_size = Pool_size}}
                <- Nodes_params],
              {Ids, _} = lists:unzip(Id_pool_size),
              Id_node_params = lists:zip(Ids, Nodes_params),
              start_nodes(Id_node_params),
              Availability = [aspike_perf_utils:wait_until_all_available(Id, Pool_size, 1000)
                || {Id, Pool_size}  <- Id_pool_size],
              Id_node_availiability = lists:zip3(Ids, Nodes_params, Availability),
              Report = aspike_perf_utils:report_availability(Id_node_availiability),
              io:format("~nNodes availability:~n~p~nNodes availability. End~n", [Report]),
              {ok, Id_node_availiability}
          end
      end
  end.

start_nodes(Id_node_params) ->
  aspike_perf_utils:start_shackle_app(),
  [start_node(Node) || {_Node_id, _Node_params} = Node <- Id_node_params].

start_node({Node_id, Node_params}) ->
  aspike_node:start(Node_id, Node_params).

run_op(#aspike_perf_op{op = Op,
  target = #aspike_perf_target{node_id = Node_id} = Node,
  task = Task}) ->
  From = self(),
  spawn(fun() ->
    Result = aspike_perf_task:run_op(Op, Node, Task),
    Stats = aspike_perf_stats:prepare(Op, Result),
    Report = aspike_perf_stats:report(Stats),
    From ! {completed, Node_id, Op, Report} end).

run_ops(Ops) ->
  [run_op(Op) || Op <- Ops],
  N = length(Ops),
  receive_ops(N).

run_scenario(Ops) ->
  N = length(Ops),
  io:format("~p processes are about to start.~n", [N]),
  {Timing, _}
    = timer:tc(aspike_perf, run_ops, [Ops], second),
  io:format("~p processes, total time: ~p seconds.~n", [N, Timing]).

receive_ops(0) ->
  io:format("~nAll processes completed.~n");
receive_ops(N) ->
  io:format("~n~p processes to complete.~n", [N]),
  receive
    {completed, Node_id, Op, Report} ->
      io:format("~n~p: ~p~n~p~n", [Node_id, Op, Report]),
      receive_ops(N-1)
  end.
