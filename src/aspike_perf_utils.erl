-module(aspike_perf_utils).
-include("../include/aspike_perf.hrl").

-dialyzer({nowarn_function, wait_until_all_available/3}).

%% API
-export([
  start_shackle_app/0,
  wait_until_all_available/3,
  connect/5,
  discover/4
]).

%% configs-related API
-export([
  mk_node_id/3,
  load_config/2,
  discover_nodes/1,
  mk_node_params/1,
  normalize_seeds/1,
  normalize_nodes/1,
  name_nodes/3,
  output_config/1,
  report_availability/1,
  save_flat_config/2,
  read_flat_config/1,
  write_terms/2,
  read_terms/1,
  password/1
]).

%% utils
-export([
  shuffle/1,
  rand/2,
  sort_by_address/1,
  endpoint_by_address/2,
  to_binary/1,
  to_binary/2,
  is_charlist/1,
  to_endpoint/1
]).

start_shackle_app() ->
  {ok, _} = application:ensure_all_started(shackle).

%%wait_until_all_available(Node_id, Timeout) ->
%%  shackle_pool:wait_until_all_available(Node_id, Timeout).

wait_until_all_available(Node_id, Pool_size, Timeout) ->
  Now = os:system_time(millisecond),
  wait_until_available2(Node_id, Pool_size, Now, Now + Timeout).

wait_until_available2(_Name, _Pool_size, Now, Expiration) when Now >= Expiration ->
  false;
wait_until_available2(Name, Pool_size, _Now, Expiration) ->
  case active(Name, Pool_size) of
    false ->
      timer:sleep(10),
      wait_until_available2(Name, Pool_size, os:system_time(millisecond), Expiration);
    _ ->
      true
  end.

active(Name, Pool_size) ->
  lists:all(fun(I) -> shackle_status:active(_ServerId = {Name, I})
    end, lists:seq(1, Pool_size)).

connect(Node_id,
    #aspike_endpoint{} = Endpoint,
    #aspike_user{} = User,
    #aspike_connection_options{} = Connection_options,
    #aspike_pool_options{pool_size = Pool_size} = Pool_options) ->
  Node_params = #aspike_node_params{endpoint = Endpoint,
    user = User,
    connection_options = Connection_options,
    pool_options = Pool_options},
  start_shackle_app(),
  aspike_node:start(Node_id, Node_params),
  Available = wait_until_all_available(Node_id, Pool_size, 1000),
  #aspike_node_params{endpoint = Endpoint} = Node_params,
  {Available, Node_id, Endpoint}.

mk_node_id(Name, Address, Port) ->
  N = if is_list(Name) -> list_to_binary(Name); true -> Name end,
  A = if is_list(Address) -> list_to_binary(Address); true -> Address end,
  A_underscore = binary:replace(A, <<".">>, <<"_">>, [global]),
  P = list_to_binary(integer_to_list(Port)),
  binary_to_atom(<<"aspike_",N/binary,"_",A_underscore/binary,"_",P/binary>>).

mk_node_params(#aspike_perf_config{nodes = undefined}) -> {error, no_nodes};
mk_node_params(#aspike_perf_config{nodes = []}) -> {error, no_nodes};
mk_node_params(#aspike_perf_config{
  user = User,
  nodes = Nodes,
  connection_options = Connection_options,
  pool_options = Pool_options}) ->
  {ok,
    [mk_node_params(User, Node, Connection_options, Pool_options)
      || Node <- Nodes]};
mk_node_params({error, _Reason} = Err) -> Err;
mk_node_params(_) -> {error, input_is_not_aspike_perf_config}.

mk_node_params(User, Node, Connection_options, Pool_options) ->
  #aspike_node_params{endpoint = Node, user = User,
    connection_options = Connection_options,
    pool_options = Pool_options}.

%% Flat config file format:
%% {term(), term()}.
%% {term(), term()}.
%% ...
%% {term(), term()}.
%% For example,
%%  {1, "ab"}.
%%  {trying_to_use_map, #{1 => "a"}}.
%%  {[1, atom, "bc"], <<"1234567">>}.

load_config(File, Profile_name) ->
  case read_terms(File) of
    {error, _Reason} = Err -> Err;
    {ok, Terms} ->
      case load_profile(Terms, Profile_name) of
        {error, not_found} ->
          {error, {profile, Profile_name, not_found_in_config_file, File}};
        {ok, Profile} -> mk_perf_config(Profile)
      end
  end.

load_profile([], _Profile_name) ->
  {error, not_found};
load_profile([H|T], Profile_name) ->
  H1 = if is_tuple(H) -> tuple_to_list(H); true -> H end,
  case is_profile(H1, Profile_name) of
    true -> {ok, H1};
    _ -> load_profile(T, Profile_name)
  end.

discover(?BOGUS_ADDRESS_RETURN_ERROR = Address,
    _Port, _User, _Encrypted_password) ->
  {error, Address};
discover(?BOGUS_ADDRESS_RETURN_NODES = _Address,
    _Port, _User, _Encrypted_password) ->
  [?BOGUS_ENDPOINT_3, ?BOGUS_ENDPOINT_2, ?BOGUS_ENDPOINT_1,
    ?BOGUS_ENDPOINT_4, ?BOGUS_ENDPOINT_5];
discover(Address, Port, User, Encrypted_password) ->
  case aspike_discover:nodes(Address, Port, User, Encrypted_password) of
    {error, _Reason} = Err -> Err;
    Nodes ->
      [#aspike_endpoint{name = N, address = A, port = P} || {N, A, P} <- Nodes]
  end.

discover_nodes(#aspike_perf_config{seed = _Seed, nodes = [_|_] = Nodes} = Config) ->
  {ok, Config#aspike_perf_config{
    nodes = lists:sort(fun endpoint_by_address/2, Nodes)}};
discover_nodes(#aspike_perf_config{
  user = #aspike_user{name = User, credential = Encrypted_password},
  seed = #aspike_endpoint{address = Address, port = Port},
  nodes = []} = Config) ->
  case discover(Address, Port, User, Encrypted_password) of
    {error, _Reason} = Err -> Err;
    Nodes ->
      {ok, Config#aspike_perf_config{
        nodes = lists:sort(fun endpoint_by_address/2, Nodes)}}
  end;
discover_nodes(#aspike_perf_config{seed = undefined, nodes = []}) ->
  {error, seed_is_not_provided_to_discover_nodes};
discover_nodes({error, _Reason} = Err) -> Err.

mk_perf_config(Profile) ->
  User = proplists:get_value(user, Profile),
  Password = proplists:get_value(password, Profile),
  Password_file = proplists:get_value(password_file, Profile),
  Seeds = proplists:get_value(seeds, Profile),
  Nodes = proplists:get_value(nodes, Profile),
  Connection_options = proplists:get_value(connection_options, Profile),
  Pool_options = proplists:get_value(pool_options, Profile),
  Socket_options = proplists:get_value(socket_options, Profile),

  case mk_user(User, Password, Password_file) of
    {error, _Reason} = Err -> Err;
    #aspike_user{} = User1 ->
      {seed, Seed} = normalize_seeds(Seeds),
      {nodes, Nodes1} = normalize_nodes(Nodes),
      Nodes2 = name_nodes(Nodes1, "X", 0),
      Socket_options1 = mk_socket_options(Socket_options),
      Connection_options1 = mk_connection_options(Connection_options, Socket_options1),
      Pool_options1 = mk_pool_options(Pool_options),
      {ok, #aspike_perf_config{user = User1,
        seed = Seed,
        nodes = Nodes2,
        connection_options = Connection_options1,
        pool_options = Pool_options1,
        socket_options = Socket_options
      }}
  end.

normalize_seeds([X|_]) ->
  {seed, to_endpoint(X)};
normalize_seeds({_,_} = X) ->
  {seed, to_endpoint(X)};
normalize_seeds(_) ->
  {seed, undefined}.

normalize_nodes({_,_} = X) ->
  case to_endpoint(X) of
    undefined -> {nodes, []};
    Endpoint -> {nodes, [Endpoint]}
  end;
normalize_nodes(Xs) when is_list(Xs) ->
  {nodes, lists:filtermap(fun (X) ->
            case to_endpoint(X) of
              undefined -> false;
              Endpoint -> {true, Endpoint}
            end
          end, Xs)};
normalize_nodes(_) ->
  {nodes, []}.

name_nodes(Nodes, Name_prefix, Start_index) when is_list(Nodes) ->
  {_, Named} = lists:foldl(fun (Node, {Index, Acc} = Accum) ->
      case Node of
        #aspike_endpoint{name = undefined} = Node ->
          {Index+1, [Node#aspike_endpoint{
              name = list_to_binary(Name_prefix ++ integer_to_list(Index))}
            | Acc]};
        #aspike_endpoint{name = Name} = Node when is_list(Name) ->
          {Index, [Node#aspike_endpoint{name = list_to_binary(Name)} | Acc]};
        #aspike_endpoint{} = Node ->
          {Index, [Node | Acc]};
        _ ->
          Accum
      end
    end, {Start_index, []}, Nodes),
  lists:reverse(Named);
name_nodes(_, _, _) ->
  [].

mk_user(undefined = _User, _Password, _Password_file) ->
  {error, no_user};
mk_user(_User, undefined = _Password, undefined = _Password_file) ->
  {error, no_password};
mk_user(_User, [] = _Password, undefined = _Password_file) ->
  {error, no_password};
mk_user(_User, <<>>, undefined = _Password_file) ->
  {error, no_password};
mk_user(User, [_|_] = Password, undefined = _Password_file) ->
  mk_user(User, Password);
mk_user(User, undefined = _Password, Password_file) ->
  case password(Password_file) of
    undefined -> {error, no_password};
    Encrypted ->
      mk_user(User, Encrypted)
  end.

mk_user(User, Password) ->
  U = if is_list(User) -> list_to_binary(User); true -> User end,
  P = if is_list(Password) -> list_to_binary(Password); true -> Password end,
  #aspike_user{name = U, credential = P}.

mk_connection_options(Connection_options, Socket_options) when is_list(Connection_options) ->
  #aspike_connection_options{
    reconnect = proplists:get_value(reconnect, Connection_options, ?DEFAULT_RECONNECT),
    reconnect_time_min = proplists:get_value(reconnect_time_min, Connection_options, ?DEFAULT_RECONNECT_TIME_MIN),
    reconnect_time_max = proplists:get_value(reconnect_time_max, Connection_options, ?DEFAULT_RECONNECT_TIME_MAX),
    socket_options = Socket_options};
mk_connection_options(_, Socket_options) ->
  mk_connection_options([], Socket_options).

mk_socket_options(Socket_options) when is_list(Socket_options) ->
  Mode = case proplists:is_defined(mode, Socket_options) of
      true -> [];
      _ -> [{mode, ?DEFAULT_SOCKET_MODE}]
    end,
  Packet = case proplists:is_defined(packet, Socket_options) of
       true -> [];
       _ -> [{packet, ?DEFAULT_SOCKET_PACKET}]
    end,
  Buffer = case proplists:is_defined(buffer, Socket_options) of
      true -> [];
      _ -> [{buffer, ?DEFAULT_SOCKET_BUFFER}]
    end,
  Nodelay = case proplists:is_defined(nodelay, Socket_options) of
      true -> [];
      _ -> [{nodelay, ?DEFAULT_SOCKET_NODELAY}]
    end,
  Send_timeout = case proplists:is_defined(send_timeout, Socket_options) of
      true -> [];
      _ -> [{send_timeout, ?DEFAULT_SOCKET_SEND_TIMEOUT}]
    end,
  Send_timeout_close = case proplists:is_defined(send_timeout_close, Socket_options) of
      true -> [];
      _ -> [{send_timeout_close, ?DEFAULT_SOCKET_SEND_TIMEOUT_CLOSE}]
    end,
  lists:append([
    Mode, Packet, Buffer, Nodelay,
    Send_timeout, Send_timeout_close, Socket_options]);
mk_socket_options(_) ->
  mk_socket_options([]).

mk_pool_options(Pool_options) when is_list(Pool_options) ->
  #aspike_pool_options{
    backlog_size = proplists:get_value(backlog_size, Pool_options, ?DEFAULT_BACKLOG_SIZE),
    max_retries = proplists:get_value(max_retries, Pool_options, ?DEFAULT_MAX_RETRIES),
    pool_size = proplists:get_value(pool_size, Pool_options, ?DEFAULT_POOL_SIZE),
    pool_strategy = proplists:get_value(pool_strategy, Pool_options, ?DEFAULT_POOL_STRATEGY)
  };
mk_pool_options(_) ->
  mk_pool_options([]).

is_profile(Term, Profile_name) when is_list(Term) ->
  case proplists:get_value(profile, Term) of
    undefined -> false;
    Value -> Value =:= Profile_name
  end.

to_endpoint({Address, Port})
  when is_integer(Port) andalso Port > 0 ->
  #aspike_endpoint{address = to_binary(Address), port = Port};
to_endpoint({Name, Address, Port})
  when is_integer(Port) andalso Port > 0 ->
  #aspike_endpoint{name = to_binary(Name),
    address = to_binary(Address), port = Port};
to_endpoint(_) -> undefined.

is_charlist([]) -> true;
is_charlist([H|T]) when is_integer(H) andalso H >= 0 ->
  is_charlist(T);
is_charlist(_) -> false.

to_binary(undefined) -> undefined;
to_binary(X) when is_binary(X) -> X;
%%to_binary(X) when is_list(X) -> list_to_binary(X);
%%to_binary(X) when is_charlist(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X);
to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_float(X) -> float_to_binary(X);
to_binary(X) ->
  case is_charlist(X) of
    true -> list_to_binary(X);
    _ -> list_to_binary(lists:flatten(io_lib:format("~tp", [X])))
  end.

to_binary(X, Default) ->
  case to_binary(X) of
    undefined -> Default;
    Binary -> Binary
  end.

output_config(#aspike_perf_config{
  user = User,
  seed = Seed,
  nodes = Nodes,
  connection_options = Connection_options,
  pool_options = Pool_options,
  socket_options = _Socket_options
}) ->
  O_user = output_user(User),
  O_seed = output_seed(Seed),
  O_nodes = output_nodes(Nodes),
  O_connection_options = output_connection_options(Connection_options),
  O_pool_options = output_pool_options(Pool_options),
  [O_user, O_seed, O_nodes, O_connection_options, O_pool_options];
output_config({error, _Reason} = Err) -> Err.

output_user(#aspike_user{name = Name, credential = undefined}) ->
  <<"User: ", Name/binary, ", no password.">>;
output_user(#aspike_user{name = Name, credential = _Credential}) ->
  <<"User: ", Name/binary, ", password is provided.">>;
output_user(_) ->
  <<"User: not provided.">>.

output_seed(#aspike_endpoint{} = Endpoint) ->
  O_endpoint = output_endpoint(Endpoint),
  <<"Seed: ", O_endpoint/binary, ".">>;
output_seed(_) ->
  <<"Seed: not provided.">>.

output_nodes(undefined) ->
  <<"Nodes: not provided.">>;
output_nodes([]) ->
  <<"Nodes: not provided.">>;
output_nodes([_|_] = Nodes) ->
  O_endpoints = [output_endpoint(Node) || Node <- Nodes],
  [<<"Nodes:">> | O_endpoints].

output_connection_options(#aspike_connection_options{
  reconnect = Reconnect,
  reconnect_time_min = Reconnect_time_min, reconnect_time_max = Reconnect_time_max,
  socket_options = Socket_options}) ->
  O_reconnect = to_binary(Reconnect, <<"not provided">>),
  O_reconnect_time_min = to_binary(Reconnect_time_min, <<"not provided">>),
  O_reconnect_time_max = to_binary(Reconnect_time_max, <<"not provided">>),
  O_socket_options = to_binary(Socket_options, <<"not provided">>),
  [<<"Reconnect: ", O_reconnect/binary,
    ", Reconnect time (min. max): (",
    O_reconnect_time_min/binary, ", ", O_reconnect_time_max/binary, ")">>,
    <<"Socket options: ", O_socket_options/binary>>];
output_connection_options(_) ->
  <<"Connection options: not provided">>.

output_pool_options(#aspike_pool_options{
  backlog_size = Backlog_size,
  max_retries = Max_retries,
  pool_size = Pool_size,
  pool_strategy = Pool_strategy}) ->
  O_backlog_size = to_binary(Backlog_size, <<"not provided">>),
  O_max_retries = to_binary(Max_retries, <<"not provided">>),
  O_pool_size = to_binary(Pool_size, <<"not provided">>),
  O_pool_strategy = to_binary(Pool_strategy, <<"not provided">>),
  <<"Backlog size: ", O_backlog_size/binary,
    ", Max retries: ", O_max_retries/binary,
    ", Pool size: ", O_pool_size/binary,
    ", Pool strategy: ", O_pool_strategy/binary>>;
output_pool_options(_) ->
  <<"Pool options: not provided">>.

output_endpoint(#aspike_endpoint{name = Name, address = Address, port = Port}) ->
  N = if Name =:= undefined -> <<"not provided">>; true -> Name end,
  A = if Address =:= undefined -> <<"not provided">>; true -> Address end,
  P = if Port =:= undefined -> <<"not provided">>; true -> integer_to_binary(Port) end,
  <<"Name: ", N/binary, ", Address: ", A/binary, ", Port: ", P/binary>>.

report_availability(Availabilities) when is_list(Availabilities) ->
  [report_availability(A) || {_Node_id, _Node_params, _Availability} = A <- Availabilities];
report_availability({Node_id,
  #aspike_node_params{endpoint = #aspike_endpoint{address = Address, port = Port}},
  Availability}) ->
  Id = to_binary(Node_id), A = to_binary(Address), P = to_binary(Port),
  Avail = if Availability -> <<"Available">>; true -> <<"NOT available">> end,
  <<Id/binary, ": ", A/binary, ":", P/binary, ": ", Avail/binary>>;
report_availability(X) ->
  B = to_binary(X),
  <<"Wrong format: ", B/binary>>.

save_flat_config(File, Flat_config) ->
  write_terms(File, Flat_config).

read_flat_config(File) ->
  read_terms(File).

write_terms(File, List_of_terms) ->
  file:write_file(File, lists:map(fun (Term) ->
      io_lib:format("~tp.~n", [Term]) end, List_of_terms)).

read_terms(File) ->
  case file:consult(File) of
    {ok, _List_of_terms} = Ret -> Ret;
    {error, Reason} when is_atom(Reason) ->
      {error, {file, File, Reason}};
    {error, {_Line, _Mod, _Term} = Reason} ->
      {error, file:format_error(Reason)}
  end.

password(File) ->
  case file:read_file(File) of
    {ok, <<Encrypted:60/binary, _/binary>>} -> Encrypted;
    _ -> undefined
  end.

shuffle(Xs) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].

rand(Lower_bound, Upper_bound) ->
  Lower_bound+ rand:uniform(Upper_bound-Lower_bound+1) - 1.

% Nodes = [{Name, Address, Port}]
sort_by_address(Nodes) ->
  lists:sort(fun ({_,A,_}, {_,B,_}) -> A =< B end, Nodes).

endpoint_by_address(#aspike_endpoint{address = A}, #aspike_endpoint{address = B}) ->
  A =< B.
