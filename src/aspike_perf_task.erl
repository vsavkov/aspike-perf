-module(aspike_perf_task).

-include("../include/aspike_perf_task.hrl").

%% API
-export([
  mk_target/4,
  mk_task/4,
  mk_op/3,
  mk_ops/3,
  mk_scenario/4,
  run_op/3,
  run_op_task/2
]).

mk_target(Node_id, Namespace, Set, Bins) ->
  #aspike_perf_target{
    node_id = Node_id,
    namespace = Namespace,
    set = Set,
    bins = Bins,
    n_bins = length(Bins)
  }.

mk_task(Seed, Count, Sleep_min, Sleep_max) when is_float(Seed) ->
  mk_task(round(Seed), Count, Sleep_min, Sleep_max);
mk_task(Seed, Count, Sleep_min, Sleep_max) when is_integer(Seed) ->
  Seed1 = max(1, Seed),
  Count1 = max(1, Count),
  #aspike_perf_task{
    method = increase,
    seed_original = Seed1,

    step_count_original = Count1,
    sleep_min = Sleep_min,
    sleep_max = Sleep_max,

    seed_next = Seed1,
    step_count_down = Count1,
    steps_acc = []
  };
mk_task({Range_min, Range_max}, Count, Sleep_min, Sleep_max)
  when is_number(Range_min) andalso is_number(Range_max) ->
  Range_min1 = if is_float(Range_min) -> round(Range_min); true -> Range_min end,
  Range_max1 = if is_float(Range_max) -> round(Range_max); true -> Range_max end,
  Range_min2 = max(1, Range_min1),
  Range_max2 = max(1, Range_max1),
  {Range_min3, Range_max3} =
    if Range_max2 < Range_min2 -> {Range_max2, Range_min2};
      true -> {Range_min2, Range_max2} end,
  Count1 = max(1, Count),
  #aspike_perf_task{
    method = random,
    range_min = Range_min3,
    range_max = Range_max3,

    step_count_original = Count1,
    sleep_min = Sleep_min,
    sleep_max = Sleep_max,

    step_count_down = Count1,
    steps_acc = []
  };
mk_task(_Seed, Count, Sleep_min, Sleep_max) ->
  mk_task(1, Count, Sleep_min, Sleep_max).

mk_op(Op, Target, Task) ->
  #aspike_perf_op{op = Op, target = Target, task = Task}.

mk_ops(Targets, Op, Task) ->
  [mk_op(Op, Target, Task) || Target <- Targets].

mk_scenario(Nodes, {Namespace, Set, Bins}, Op, Tasks) ->
  [mk_op(Op, mk_target(Node_id, Namespace, Set, Bins), Task)
    || {Node_id, _, Availability} <- Nodes, Availability,
    Task <- Tasks].

%% run_op
run_op(Op, #aspike_perf_target{} = Target, #aspike_perf_task{} = Task) ->
  Task1 = Task#aspike_perf_task{target = Target},
  {Timing, {ok, #aspike_perf_task{steps_acc = Acc}}}
    = timer:tc(aspike_perf_task, run_op_task, [Op, Task1], millisecond),
  {Timing, Acc}.

run_op_task(Op, #aspike_perf_task{} = Task) ->
  case run_op_next(Op, Task) of
    {stop, Task} -> {ok, Task};
    {next, Step} ->
      Result = run_op_step(Op, Step),
      Task1 = run_op_update(Op, Task, Result),
      run_op_task(Op, Task1)
  end.

run_op_next(_Op, #aspike_perf_task{step_count_down = 0} = Task) ->
  {stop, Task};
run_op_next(put, #aspike_perf_task{
  target =  #aspike_perf_target{bins = Bins, n_bins = N_bins} = Target,
  step_count_original = Max_value,
  sleep_min = Min, sleep_max = Max} = Task) ->
  K = gen_key(Task),
  Vs = gen_values(K, N_bins, Max_value),
  BVs = lists:zip(Bins, Vs),
  Sleep = if Min =:= Max -> Min; true -> aspike_perf_utils:rand(Min, Max) end,
  {next, #aspike_perf_step{
    target = Target,
    key = K,
    bin_value_arr = BVs,
    sleep = Sleep,
    timing_unit = microsecond
  }};
run_op_next(get, #aspike_perf_task{
  target = #aspike_perf_target{bins = Bins} = Target,
  sleep_min = Min, sleep_max = Max} = Task) ->
  K = gen_key(Task),
  Sleep = if Min =:= Max -> Min; true -> aspike_perf_utils:rand(Min, Max) end,
  {next, #aspike_perf_step{
    target = Target,
    key = K,
    bin_value_arr = Bins,
    sleep = Sleep,
    timing_unit = microsecond
  }};
run_op_next(remove, #aspike_perf_task{
  target = Target,
  sleep_min = Min, sleep_max = Max} = Task) ->
  K = gen_key(Task),
  Sleep = if Min =:= Max -> Min; true -> aspike_perf_utils:rand(Min, Max) end,
  {next, #aspike_perf_step{
    target = Target,
    key = K,
    sleep = Sleep,
    timing_unit = microsecond
  }}.

run_op_step(Op, #aspike_perf_step{target = _Target,
  sleep = Sleep, timing_unit = Timing_unit} = Step) ->
  if Sleep =:= 0 -> erlang:yield(); true -> timer:sleep(Sleep) end,
  {Mod, Fun, Args} = run_op_mfa(Op, Step),
  {Timing, Result} = timer:tc(Mod, Fun, Args, Timing_unit),
  #aspike_perf_step_result{
    sleep = Sleep, timing = Timing, timing_unit = Timing_unit,
    result = Result
  }.

run_op_mfa(put, #aspike_perf_step{
  target = #aspike_perf_target{node_id = Node_id,
    namespace = Namespace, set = Set},
  key = K, bin_value_arr = BVs}) ->
  Key_digest = aspike_protocol:digest(Set, K),
  {aspike_node, put, [Node_id, Namespace, Set, Key_digest, BVs]};
run_op_mfa(get, #aspike_perf_step{
  target = #aspike_perf_target{node_id = Node_id,
    namespace = Namespace, set = Set},
  key = K, bin_value_arr = BVs}) ->
  Key_digest = aspike_protocol:digest(Set, K),
  {aspike_node, get, [Node_id, Namespace, Set, Key_digest, BVs]};
run_op_mfa(remove, #aspike_perf_step{
  target = #aspike_perf_target{node_id = Node_id,
    namespace = Namespace, set = Set},
  key = K}) ->
  Key_digest = aspike_protocol:digest(Set, K),
  {aspike_node, remove, [Node_id, Namespace, Set, Key_digest]}.

run_op_update(put, #aspike_perf_task{method = increase, seed_next = Seed_next,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{seed_next = Seed_next + 1,
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]};
run_op_update(put, #aspike_perf_task{method = random,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]};
run_op_update(get, #aspike_perf_task{method = increase, seed_next = Seed_next,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{seed_next = Seed_next + 1,
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]};
run_op_update(get, #aspike_perf_task{method = random,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]};
run_op_update(remove, #aspike_perf_task{method = increase, seed_next = Seed_next,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{seed_next = Seed_next + 1,
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]};
run_op_update(remove, #aspike_perf_task{method = random,
  step_count_down = Step_count_down,
  steps_acc = Steps_acc} = Task,
    #aspike_perf_step_result{} = Result) ->
  Task#aspike_perf_task{
    step_count_down = Step_count_down - 1,
    steps_acc = [Result | Steps_acc]}.

% utils
gen_key(#aspike_perf_task{method = increase, seed_next = Next}) ->
  integer_to_list(Next);
gen_key(#aspike_perf_task{method = random, range_min = Range_min, range_max = Range_max}) ->
  integer_to_list(aspike_perf_utils:rand(Range_min, Range_max)).

gen_values(Key, N, Max_value) ->
  [ Key ++ integer_to_list(I) ++ "." ++ integer_to_list(aspike_perf_utils:rand(1, Max_value))
    || I <- lists:seq(1, N)].
