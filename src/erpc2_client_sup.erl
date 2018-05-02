-module(erpc2_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init_pool/0,
         get_tcp_sock/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%init([]) ->
%    {ok, { {one_for_one, 5, 10}, [?CHILD(erpc2_client_server, worker)]} }.

init([]) ->
    {ok, Pools} = application:get_env(erpc2_client, pools),
    lager:debug("Pools = ~p", [Pools]),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, erpc2_client}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    lager:debug("PoolSpecs = ~p", [PoolSpecs]),
    PoolSpecs1 = [],
    {ok, {{one_for_one, 10, 10}, PoolSpecs1}}.

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).


init_pool() ->
    ets:new(tcp_pool,[set,named_table,public]),
    Num = application:get_env(erpc2_client, tcp_pool_size, 10),
    F = fun(N) ->
                {ok, Pid} = erpc2_client:start_link([]),
                ets:insert(tcp_pool, {N, Pid})
        end,     
    [F(N)|| N<- lists:seq(0, Num)].

get_tcp_sock() ->
    Num = application:get_env(erpc2_client, tcp_pool_size, 10),
    rand:seed(exs64, os:timestamp()),
    N = rand:uniform(Num),
    ets:lookup(tcp_pool, N).





