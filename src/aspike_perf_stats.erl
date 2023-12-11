-module(aspike_perf_stats).

-include("../include/aspike_perf_task.hrl").
-include("../include/aspike_perf_stats.hrl").

%% API
-export([
  report/1,
  prepare/2
]).

new_stats() ->
  #aspike_perf_stats{
    total_steps = 0,
    n_ok = 0,
    n_errors = 0,
    total_sleep = 0,
    total_ops_timing = 0,
    ok_ops_timing = 0,
    error_ops_timing = 0,
    errors = #{}
  }.

report({Timing, #aspike_perf_stats{
  total_steps = Total_steps, n_ok = N_ok, n_errors = N_errors,
  total_sleep = Total_sleep, total_ops_timing = Total_ops_timing,
  ok_ops_timing = Ok_ops_timing, error_ops_timing = Error_ops_timing,
  errors = Errors_map}}) ->

  O_steps = integer_to_binary(Total_steps),
  O_ok = integer_to_binary(N_ok),
  O_errors = integer_to_binary(N_errors),
  O_error_kinds = integer_to_binary(maps:size(Errors_map)),
  O_operations = <<"Operations: ", O_steps/binary, " = ",
    O_ok/binary, " (Ok) + ",
    O_errors/binary, " / ", O_error_kinds/binary,  " (errors/kinds)">>,

  O_timing = integer_to_binary(Timing),
  O_sleep = integer_to_binary(Total_sleep),
  O_ops = integer_to_binary(Total_ops_timing div 1000),
  O_duration = <<"Total duration: ", O_timing/binary, " (ms) = ",
    O_ops/binary, " (ops) + ", O_sleep/binary, " (sleeps)">>,

  Ops_per_sec = ops_per_sec(Total_ops_timing, microsecond, Total_steps),
  Ok_ops_per_sec = ops_per_sec(Ok_ops_timing, microsecond, N_ok),
  Error_ops_per_sec = ops_per_sec(Error_ops_timing, microsecond, N_errors),
  O_ops_per_sec = integer_to_binary(Ops_per_sec),
  O_ok_ops_per_sec = integer_to_binary(Ok_ops_per_sec),
  O_error_ops_per_sec = integer_to_binary(Error_ops_per_sec),
  O_performance = <<"Performance: ", O_ops_per_sec/binary, " ops/sec",
    ", OK: ", O_ok_ops_per_sec/binary, " ops/sec",
    ", Error: ", O_error_ops_per_sec/binary, " ops/sec">>,

  O_report_errors = report_handle_errors(Errors_map),

  case size(O_report_errors) of
    0 -> [O_operations, O_duration, O_performance];
    _ -> [O_operations, O_duration, O_performance, O_report_errors]
  end.

report_handle_errors(#{} = Errors_map) when map_size(Errors_map) =:= 0 ->
  <<>>;
report_handle_errors(#{} = Errors_map) ->
  Errors_list = maps:to_list(Errors_map),
  Error_list_binary = [aspike_perf_utils:to_binary(Err) || Err <- Errors_list],
  report_handle_errors(Error_list_binary);
report_handle_errors([]) ->
  <<>>;
report_handle_errors([H|T]) ->
  lists:foldl(fun (X, Acc) -> <<Acc/binary, ", ", X/binary>> end,
    <<"Errors: ", H/binary>>, T).

prepare(put, {Timing, Steps}) ->
  S_0 = new_stats(),
  Stats = lists:foldl(
    fun (
        #aspike_perf_step_result{
          sleep = Step_sleep, timing = Step_timing, result = Result} = _Step,
        #aspike_perf_stats{
          total_steps = Total_step, n_ok = N_ok, n_errors = N_errors,
          total_sleep = Total_sleep, total_ops_timing = Total_ops_timing,
          ok_ops_timing = Ok_ops_timing, error_ops_timing = Error_ops_timing,
          errors = Errors_map}) ->
      {Ok, Error, Ok_timing, Error_timing, Errors_map_updated} =
        case Result of
          ok -> {1, 0, Step_timing, 0, Errors_map};
          Err ->
            Errors_map2 = maps:update_with(Err,
              fun (V) -> V+1 end, 1, Errors_map),
            {0, 1, 0, Step_timing, Errors_map2}
        end,
      #aspike_perf_stats{
        total_steps = Total_step + 1,
        n_ok = N_ok + Ok,
        n_errors = N_errors + Error,
        total_sleep = Total_sleep + Step_sleep,
        total_ops_timing = Total_ops_timing + Step_timing,
        ok_ops_timing = Ok_ops_timing + Ok_timing,
        error_ops_timing = Error_ops_timing + Error_timing,
        errors = Errors_map_updated}
    end, S_0, Steps),
  {Timing, Stats};

prepare(get, {Timing, Steps}) ->
  S_0 = new_stats(),
  Stats = lists:foldl(
    fun (
        #aspike_perf_step_result{
          sleep = Step_sleep, timing = Step_timing, result = Result} = _Step,
        #aspike_perf_stats{
          total_steps = Total_step, n_ok = N_ok, n_errors = N_errors,
          total_sleep = Total_sleep, total_ops_timing = Total_ops_timing,
          ok_ops_timing = Ok_ops_timing, error_ops_timing = Error_ops_timing,
          errors = Errors_map}) ->
      {Ok, Error, Ok_timing, Error_timing, Errors_map_updated} =
        case Result of
          {ok, _} -> {1, 0, Step_timing, 0, Errors_map};
          Err ->
            Errors_map2 = maps:update_with(Err,
              fun (V) -> V+1 end, 1, Errors_map),
            {0, 1, 0, Step_timing, Errors_map2}
        end,
      #aspike_perf_stats{
        total_steps = Total_step + 1,
        n_ok = N_ok + Ok,
        n_errors = N_errors + Error,
        total_sleep = Total_sleep + Step_sleep,
        total_ops_timing = Total_ops_timing + Step_timing,
        ok_ops_timing = Ok_ops_timing + Ok_timing,
        error_ops_timing = Error_ops_timing + Error_timing,
        errors = Errors_map_updated}
    end, S_0, Steps),
  {Timing, Stats};

prepare(remove, {Timing, Steps}) ->
  S_0 = new_stats(),
  Stats = lists:foldl(
    fun (
        #aspike_perf_step_result{
          sleep = Step_sleep, timing = Step_timing, result = Result} = _Step,
        #aspike_perf_stats{
          total_steps = Total_step, n_ok = N_ok, n_errors = N_errors,
          total_sleep = Total_sleep, total_ops_timing = Total_ops_timing,
          ok_ops_timing = Ok_ops_timing, error_ops_timing = Error_ops_timing,
          errors = Errors_map}) ->
      {Ok, Error, Ok_timing, Error_timing, Errors_map_updated} =
        case Result of
          ok -> {1, 0, Step_timing, 0, Errors_map};
          Err ->
            Errors_map2 = maps:update_with(Err,
              fun (V) -> V+1 end, 1, Errors_map),
            {0, 1, 0, Step_timing, Errors_map2}
        end,
      #aspike_perf_stats{
        total_steps = Total_step + 1,
        n_ok = N_ok + Ok,
        n_errors = N_errors + Error,
        total_sleep = Total_sleep + Step_sleep,
        total_ops_timing = Total_ops_timing + Step_timing,
        ok_ops_timing = Ok_ops_timing + Ok_timing,
        error_ops_timing = Error_ops_timing + Error_timing,
        errors = Errors_map_updated}
    end, S_0, Steps),
  {Timing, Stats}.

ops_per_sec(0, _Time_unit, _N_ops) -> 0;
ops_per_sec(Timing, microsecond, N_ops) ->
  round((1_000_000 / Timing) * N_ops).
