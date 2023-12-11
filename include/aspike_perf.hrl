-record(aspike_endpoint, {
  name :: string(),
  address :: inet:ip_address() | inet:hostname(),
  port :: inet:port_number()
}).

-record(aspike_user, {
  name :: binary(),
  credential :: aspike:credential()
}).

-record(aspike_connection_options, {
  reconnect :: boolean(),
  reconnect_time_min :: pos_integer(),
  reconnect_time_max :: pos_integer() | infinity,
  socket_options :: [gen_tcp:connect_option()]
}).

-record(aspike_pool_options, {
  backlog_size :: pos_integer() | infinity,
  max_retries :: non_neg_integer(),
  pool_size :: pos_integer(),
  pool_strategy :: random | round_robin
}).

-record(aspike_node_params, {
  endpoint :: #aspike_endpoint{},
  user :: #aspike_user{},
  connection_options :: #aspike_connection_options{},
  pool_options :: #aspike_pool_options{}
}).

-record(aspike_perf_config, {
  user :: #aspike_user{},
  seed :: #aspike_endpoint{},
  nodes :: list(#aspike_endpoint{}),
  connection_options :: #aspike_connection_options{},
  pool_options :: #aspike_pool_options{},
  socket_options :: [gen_tcp:connect_option()]
}).

-define(DEFAULT_NODE, "A777").
-define(DEFAULT_ADDRESS, "127.0.0.1").
-define(DEFAULT_PORT, 54321).

-define(DEFAULT_USER, "user_not_provided").
-define(DEFAULT_PASSWORD, "password_not_provided").

-define(DEFAULT_RECONNECT, false).
-define(DEFAULT_RECONNECT_TIME_MAX, 120000).
-define(DEFAULT_RECONNECT_TIME_MIN, 2000).

-define(DEFAULT_BACKLOG_SIZE, 1024).
-define(DEFAULT_MAX_RETRIES, 0).
-define(DEFAULT_POOL_SIZE, 1).
-define(DEFAULT_POOL_STRATEGY, random).

-define(DEFAULT_SOCKET_MODE, binary).
-define(DEFAULT_SOCKET_PACKET, raw).
-define(DEFAULT_SOCKET_BUFFER, 65535).
-define(DEFAULT_SOCKET_NODELAY, true).
-define(DEFAULT_SOCKET_SEND_TIMEOUT, 50).
-define(DEFAULT_SOCKET_SEND_TIMEOUT_CLOSE, true).

-define(BOGUS_ADDRESS_RETURN_ERROR, <<"Bogus address for testing to return error">>).
-define(BOGUS_ADDRESS_RETURN_NODES, <<"Bogus address for testing to return bogus nodes">>).
-define(BOGUS_ENDPOINT_1,
  #aspike_endpoint{name = <<"Bogus name 1">>, address = <<"Bogus address 1">>, port = 1}).
-define(BOGUS_ENDPOINT_2,
  #aspike_endpoint{name = <<"Bogus name 2">>, address = <<"Bogus address 2">>, port = 2}).
-define(BOGUS_ENDPOINT_3,
  #aspike_endpoint{name = <<"Bogus name 3">>, address = <<"Bogus address 3">>, port = 3}).
-define(BOGUS_ENDPOINT_4,
  #aspike_endpoint{name = <<"Bogus name 4">>, address = <<"Bogus address 4">>, port = 4}).
-define(BOGUS_ENDPOINT_5,
  #aspike_endpoint{name = <<"Bogus name 5">>, address = <<"Bogus address 5">>, port = 5}).
