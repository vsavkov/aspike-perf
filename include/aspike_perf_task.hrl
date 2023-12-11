-record(aspike_perf_target,{
  node_id,
  namespace,
  set,
  bins,
  n_bins
}).

-record(aspike_perf_task,{
  target, % #aspike_perf_target

  % task init params, immutable
  method, % increase | random
  seed_original, % increase
  range_min, % random
  range_max, % random
  step_count_original,
  sleep_min,
  sleep_max,

  % task state, mutable
  seed_next,
  step_count_down,
  steps_acc
}).

-record(aspike_perf_op, {
  op, % put | get | remove
  target, % aspike_perf_target
  task % aspike_perf_task
}).

-record(aspike_perf_step,{
  target, % #aspike_perf_target
  key,
  bin_value_arr,
  sleep,
  timing_unit
}).

-record(aspike_perf_step_result,{
  sleep,
  timing,
  timing_unit,
  result
}).
