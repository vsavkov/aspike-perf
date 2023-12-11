aspike_perf
=====

Driver to run ``Erlang Aerospike node client - Aerospike cluster`` performance and load tests

Build
-----

    $ make compile

Test
----

    $ make eunit

Run
----

    $ rebar3 shell
    Erlang/OTP ...

Performance Test Setup
----------------------
#### Diagram 1. Components aspike-perf relies on
```
===================================
test machine
-----------------------------------
aspike-perf
request/response round-trip time
is measured at this level
-----------------------------------
aspike-discover | aspike-node
-----------------------------------
aspike-protocol | shackle
-----------------------------------
Erlang runtime
===================================
                 |  ^
                 |  |
                 V  |
===================================
 network infrastructure
===================================
                 |  ^
                 |  |
                 V  |
===================================
Aerospike Nodes: Node1, ..., NodeN
-----------------------------------
Namespaces
===================================
```

Terminology
----

#### Profile
_Profile_ is an erlang term that defines:
- User's name and encrypted password location;
- Cluster's seeds or nodes;
- Connections to nodes pool options;
- Nodes connections options;
- Sockets options.

#### Operations
_Operations_ are
- ``put`` key-value(s);
- ``get`` value(s) by key;
- ``remove`` key.

#### Target
_Target_ is an Aerospike Cluster ``node`` with a ``namespace``, ``set`` and ``bins``.

The purpose of target is to describe where the operations will be performed.

Target example:
```erlang
1> rr("include/aspike_perf_task.hrl").
[aspike_perf_op,aspike_perf_step,aspike_perf_step_result,aspike_perf_target,aspike_perf_task]
2> #aspike_perf_target{} = Target = aspike_perf_task:mk_target(aspike_node_a1, "namespace-1", "set-1", ["bin1", "bin2", "bin3"]).
```

#### Task
_Task_ is an entity to describe
- How many operations should be performed;
- How a key for each operation should be produced: in increasing manner or randomly;
- How long should be time period between operations.

Task examples:

1. Create a task to:
- generate ``10,000`` keys in increasing manner;
- first key is ``3``;
- perform operations with intervals between ``100`` millisecond and ``700`` milliseconf
```erlang
3> #aspike_perf_task{} = Task1 = aspike_perf_task:mk_task(3, 10_000, 100, 700).
```

2. Create a task to:
- generate ``10,000`` keys randomly;
- the keys should be generated in interval from ``1`` to ``5,000``;
- perform operations with intervals between ``100`` millisecond and ``700`` milliseconf
```erlang
4> #aspike_perf_task{} = Task2 = aspike_perf_task:mk_task({1, 5_000}, 10_000, 100, 700).
```

#### Scenario
_Scenario_ describes what ``tasks`` will be performed on what ``targets``.

## Performance Examples

#### Environments to run examples
- Aerospike Cluster Standard/Enterprise/Cloud (https://aerospike.com/products/features-and-editions/);
- Aerospike Cluster Community Edition (CE) (https://hub.docker.com/r/aerospike/aerospike-server);
- Aerospike Server Emulator (EM) (https://github.com/vsavkov/aspike-server).

How to setup the environments, see section **Environments Setup** at the bottom.

### Aerospike Cluster Community Edition (CE) Examples

NOTE: Before running the examples check that files ``examples/ce_aerospike.config`` and ``examples/ce_password.txt`` are present.

    $ rebar3 shell
    Erlang/OTP ...

#### Example 1. 1,000 ``put`` operations to ``one`` node over ``one`` connection

```erlang
1> Ret_start = aspike_perf:start("examples/ce_aerospike.config", "1 node, pool size = 1").
```
The output to see
```erlang
Effective config:
[<<"User: User-does-not-matter-for-CE, password is provided.">>,
 <<"Seed: not provided.">>,
 [<<"Nodes:">>,<<"Name: X0, Address: 127.0.0.1, Port: 3000">>],
 [<<"Reconnect: false, Reconnect time (min. max): (2000, 120000)">>,
  <<"Socket options: [{mode,binary},\n {packet,raw},\n {buffer,65535},\n {nodelay,true},\n {send_timeout,50},\n {send_timeout_close,true}]">>],
 <<"Backlog size: 1024, Max retries: 0, Pool size: 1, Pool strategy: random">>]
Effective config. End
```
followed by
```erlang
Nodes availability:
[<<"aspike_X0_127_0_0_1_3000: 127.0.0.1:3000: Available">>]
Nodes availability. End
```
The output indicates that:
- ``aspike_node`` started with Node Id = ``aspike_X0_127_0_0_1_3000``;
- connection was established to Aerospike Node running on ``127.0.0.1:3000``.

Next, extract list of nodes:
```erlang
2> {ok, Nodes} = Ret_start.
```
Create a task
- to run ``1,000 operations``,
- with keys starting from ``1`` and ``incrementing``,
- without delays between operations.
```erlang
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
```

Create a scenario
- to target ``Nodes``, namespace ``test``, set ``set-test``, bins ``bin1``, ``bin2``, ``bin3``;
- to perform ``put`` operations;
- according to task ``T1_000``.
```erlang
4> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000]).
```
Run the scenario ``Sc1``
```erlang
5> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
1 processes are about to start.

1 processes to complete.

aspike_X0_127_0_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 1275 (ms) = 974 (ops) + 0 (sleeps)">>,
 <<"Performance: 1026 ops/sec, OK: 1026 ops/sec, Error: 0 ops/sec">>]

All processes completed.
1 processes, total time: 1 seconds.
```

#### Example 2. 1,000 ``put`` operations to ``two`` nodes over ``three`` connections to each node

```erlang
1> Ret_start = aspike_perf:start("examples/ce_aerospike.config", "2 nodes, pool size = 3").
Nodes availability:
[<<"aspike_X0_127_0_0_1_3000: 127.0.0.1:3000: Available">>,
<<"aspike_X1_127_0_0_1_3000: 127.0.0.1:3000: Available">>]
Nodes availability. End
2> {ok, Nodes} = Ret_start.
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
4> T1_000_rand = aspike_perf_task:mk_task({200, 700}, 1_000, 0, 0).
5> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000, T1_000_rand]).
6> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
4 processes are about to start.

4 processes to complete.

aspike_X0_127_0_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 2326 (ms) = 2192 (ops) + 0 (sleeps)">>,
 <<"Performance: 456 ops/sec, OK: 456 ops/sec, Error: 0 ops/sec">>]

3 processes to complete.

aspike_X0_127_0_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 2330 (ms) = 2200 (ops) + 0 (sleeps)">>,
 <<"Performance: 454 ops/sec, OK: 454 ops/sec, Error: 0 ops/sec">>]

2 processes to complete.

aspike_X1_127_0_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 2328 (ms) = 2193 (ops) + 0 (sleeps)">>,
 <<"Performance: 456 ops/sec, OK: 456 ops/sec, Error: 0 ops/sec">>]

1 processes to complete.

aspike_X1_127_0_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 2334 (ms) = 2204 (ops) + 0 (sleeps)">>,
 <<"Performance: 454 ops/sec, OK: 454 ops/sec, Error: 0 ops/sec">>]

All processes completed.
4 processes, total time: 2 seconds.
```

### Aerospike Server Emulator (EM) Examples

NOTE 1: Before running these examples follow the instructions in section **Aerospike Server Emulator (EM) Setup** at the bottom.

NOTE 2: Before running the examples check that files ``examples/em_aerospike.config`` and ``examples/em_password.txt`` are present.

    $ rebar3 shell
    Erlang/OTP ...

#### Example 1. 1,000 ``put`` operations to ``one`` node over ``one`` connection

```erlang
1> Ret_start = aspike_perf:start("examples/em_aerospike.config", "1 node, pool size = 1").
Nodes availability:
[<<"aspike_X0_127_0_0_1_4041: 127.0.0.1:4041: Available">>]
Nodes availability. End
2> {ok, Nodes} = Ret_start.
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
4> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000]).
5> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
1 processes are about to start.

1 processes to complete.

aspike_X0_127_0_0_1_4041: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 108 (ms) = 99 (ops) + 0 (sleeps)">>,
<<"Performance: 10090 ops/sec, OK: 10090 ops/sec, Error: 0 ops/sec">>]

All processes completed.
1 processes, total time: 0 seconds.
```

#### Example 2. 1,000 ``put`` operations to ``two`` nodes over ``three`` connections to each node

```erlang
1> Ret_start = aspike_perf:start("examples/em_aerospike.config", "2 nodes, pool size = 3").
Nodes availability:
[<<"aspike_X0_127_0_0_1_4041: 127.0.0.1:4041: Available">>,
<<"aspike_X1_127_0_0_1_4041: 127.0.0.1:4041: Available">>]
Nodes availability. End
2> {ok, Nodes} = Ret_start.
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
4> T1_000_rand = aspike_perf_task:mk_task({200, 700}, 1_000, 0, 0).
5> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000, T1_000_rand]).
6> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
4 processes are about to start.

4 processes to complete.

aspike_X0_127_0_0_1_4041: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 272 (ms) = 165 (ops) + 0 (sleeps)">>,
<<"Performance: 6035 ops/sec, OK: 6035 ops/sec, Error: 0 ops/sec">>]

3 processes to complete.

aspike_X0_127_0_0_1_4041: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 273 (ms) = 166 (ops) + 0 (sleeps)">>,
<<"Performance: 5989 ops/sec, OK: 5989 ops/sec, Error: 0 ops/sec">>]

2 processes to complete.

aspike_X1_127_0_0_1_4041: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 273 (ms) = 167 (ops) + 0 (sleeps)">>,
<<"Performance: 5980 ops/sec, OK: 5980 ops/sec, Error: 0 ops/sec">>]

1 processes to complete.

aspike_X1_127_0_0_1_4041: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 274 (ms) = 166 (ops) + 0 (sleeps)">>,
<<"Performance: 6004 ops/sec, OK: 6004 ops/sec, Error: 0 ops/sec">>]

All processes completed.
4 processes, total time: 0 seconds.
```

### Aerospike Cluster Examples

##### Pre-requisites
1. User and password;
2. Cluster Seed (or any cluster node) IP address and port;
3. Open file ``examples/cluster_aerospike.config`` and edit **ALL** fields where phrase `your-cluster-` appears;
4. Namespace in the cluster available for tests. In the following examples we will assume that namespace is ``test``.


    $ rebar3 shell
    Erlang/OTP ...

```erlang
1> Encrypted_password = aspike_blowfish:crypt("your-password").
```
NOTE: The operation above could take 2-5 seconds.
```erlang
2> ok = file:write_file("examples/cluster_password.txt", Encrypted_password).
3> Encrypted_password = aspike_perf_utils:password("examples/cluster_password.txt").
```

#### Example 1. 1,000 ``put`` operations to ``one`` node over ``one`` connection

```erlang
1> Ret_start = aspike_perf:start("examples/em_aerospike.config", "1 node, pool size = 1").
```
Because field ``seeds`` was entered in the config, the discovery process was performed and produced available nodes.

(Note: Node's names and IP address will be specific to your cluster)
```erlang
Nodes availability:
[<<"aspike_X5_192_168_0_1_3000: 192.168.0.1:3000: Available">>,
<<"aspike_X9_192_168_0_2_3000: 192.168.0.2:3000: Available">>,
<<"aspike_X7_192_168_0_3_3000: 192.168.0.3:3000: Available">>,
<<"aspike_X3_192_168_0_4_3000: 192.168.0.4:3000: Available">>,
<<"aspike_X8_192_168_0_5_3000: 192.168.0.5:3000: Available">>,
<<"aspike_X6_192_168_0_6_3000: 192.168.0.6:3000: Available">>,
<<"aspike_X0_192_168_0_7_3000: 192.168.0.7:3000: Available">>,
<<"aspike_X2_192_168_0_8_3000: 192.168.0.8:3000: Available">>,
<<"aspike_X4_192_168_0_9_3000: 192.168.0.9:3000: Available">>,
<<"aspike_X1_192_168_0_10_3000: 192.168.0.10:3000: Available">>]
Nodes availability. End
```
```erlang
2> {ok, Nodes} = Ret_start.
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
4> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000]).
5> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
10 processes are about to start.

10 processes to complete.

aspike_X0_192_168_0_7_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 994 (ms) = 945 (ops) + 0 (sleeps)">>,
<<"Performance: 1058 ops/sec, OK: 1058 ops/sec, Error: 0 ops/sec">>]

9 processes to complete.

aspike_X7_192_168_0_3_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1002 (ms) = 953 (ops) + 0 (sleeps)">>,
<<"Performance: 1048 ops/sec, OK: 1048 ops/sec, Error: 0 ops/sec">>]

...

2 processes to complete.

aspike_X6_192_168_0_6_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1074 (ms) = 1026 (ops) + 0 (sleeps)">>,
<<"Performance: 974 ops/sec, OK: 974 ops/sec, Error: 0 ops/sec">>]

1 processes to complete.

aspike_X3_192_168_0_4_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1089 (ms) = 1040 (ops) + 0 (sleeps)">>,
<<"Performance: 961 ops/sec, OK: 961 ops/sec, Error: 0 ops/sec">>]

All processes completed.
10 processes, total time: 1 seconds.
```

#### Example 2. 1,000 ``put`` operations to ``two`` nodes over ``three`` connections to each node

```erlang
1> Ret_start = aspike_perf:start("examples/cluster_aerospike.config", "2 nodes, pool size = 3").
```
Because field ``seeds`` was entered in the config, the discovery process was performed and produced available nodes.

(Note: Node's names and IP address will be specific to your cluster)
```erlang
Nodes availability:
[<<"aspike_X5_192_168_0_1_3000: 192.168.0.1:3000: Available">>,
<<"aspike_X9_192_168_0_2_3000: 192.168.0.2:3000: Available">>,
<<"aspike_X7_192_168_0_3_3000: 192.168.0.3:3000: Available">>,
<<"aspike_X3_192_168_0_4_3000: 192.168.0.4:3000: Available">>,
<<"aspike_X8_192_168_0_5_3000: 192.168.0.5:3000: Available">>,
<<"aspike_X6_192_168_0_6_3000: 192.168.0.6:3000: Available">>,
<<"aspike_X0_192_168_0_7_3000: 192.168.0.7:3000: Available">>,
<<"aspike_X2_192_168_0_8_3000: 192.168.0.8:3000: Available">>,
<<"aspike_X4_192_168_0_9_3000: 192.168.0.9:3000: Available">>,
<<"aspike_X1_192_168_0_10_3000: 192.168.0.10:3000: Available">>]
Nodes availability. End
```
```erlang
2> {ok, Nodes} = Ret_start.
3> T1_000 = aspike_perf_task:mk_task(1, 1_000, 0, 0).
4> T1_000_rand = aspike_perf_task:mk_task({200, 700}, 1_000, 0, 0).
5> Sc1 = aspike_perf_task:mk_scenario(Nodes, {"test", "set-test", ["bin1", "bin2", "bin3"]}, put, [T1_000, T1_000_rand]).
6> aspike_perf:run_scenario(Sc1).
```
The output suppose to be:
```erlang
20 processes are about to start.

20 processes to complete.

aspike_X2_192_168_0_8_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 1321 (ms) = 1273 (ops) + 0 (sleeps)">>,
 <<"Performance: 785 ops/sec, OK: 785 ops/sec, Error: 0 ops/sec">>]

19 processes to complete.

aspike_X5_192_168_0_1_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
 <<"Total duration: 1325 (ms) = 1277 (ops) + 0 (sleeps)">>,
 <<"Performance: 783 ops/sec, OK: 783 ops/sec, Error: 0 ops/sec">>]

18 processes to complete.

...

3 processes to complete.

aspike_X3_192_168_0_4_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1433 (ms) = 1386 (ops) + 0 (sleeps)">>,
<<"Performance: 721 ops/sec, OK: 721 ops/sec, Error: 0 ops/sec">>]

2 processes to complete.

aspike_X0_192_168_0_7_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1443 (ms) = 1395 (ops) + 0 (sleeps)">>,
<<"Performance: 717 ops/sec, OK: 717 ops/sec, Error: 0 ops/sec">>]

1 processes to complete.

aspike_X0_192_168_0_7_3000: put
[<<"Operations: 1000 = 1000 (Ok) + 0 / 0 (errors/kinds)">>,
<<"Total duration: 1455 (ms) = 1407 (ops) + 0 (sleeps)">>,
<<"Performance: 710 ops/sec, OK: 710 ops/sec, Error: 0 ops/sec">>]

All processes completed.
20 processes, total time: 1 seconds.
```

## Environments Setup

##### Aerospike Cluster Community Edition (CE) Setup
Follow the instructions on https://hub.docker.com/r/aerospike/aerospike-server

##### Aerospike Server Emulator (EM) Setup
1. Clone https://github.com/vsavkov/aspike-server;
2. From terminal, in `aspike-server` directory, run
```bash
iex -S mix
[info] Accepting connections on port 4040
[info] Accepting connections on port 4041
```
Aerospike Server Emulator accepts its own `text protocol` on port `4040`.

Aerospike Server Emulator accepts Aerospike `binary protocol` on port `4041`.

3. Create `namespace` `test` for the examples

- From another terminal run `telnet` or `nc` (aka `netcat`)
```bash
nc -vv 127.0.0.1 4040
```

- To create `namespace` `test`, type in
```
CREATE test
```

- To check that `namespace` `test` exists, type in
```
NAMESPACES
```

```
[test]
OK
indicates that `namespace` `test` exists.
```


