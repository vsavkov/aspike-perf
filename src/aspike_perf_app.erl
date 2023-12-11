%%%-------------------------------------------------------------------
%% @doc aspike_perf public API
%% @end
%%%-------------------------------------------------------------------

-module(aspike_perf_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aspike_perf_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
