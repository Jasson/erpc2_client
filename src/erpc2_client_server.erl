-module(erpc2_client_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         squery/2,
         start_link/0
        ]).

-record(state, {conn}).
-include_lib("eunit/include/eunit.hrl").
-define(K_TIMEOUT, 10000).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    %%gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
     proc_lib:start_link(?MODULE, init, []).

start_link(Args) ->
    lager:debug("Args = ~p", [Args]),
    proc_lib:start_link(?MODULE, init, [Args]).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ok = proc_lib:init_ack({ok, self()}),
    lager:debug("Args = ~p", [Args]),
    process_flag(trap_exit, true),
    Ip = proplists:get_value(ip, Args, "test.com"),
    Port = proplists:get_value(port, Args, 5554),
    %Conn = tcp_client:connect(Ip, Port),
    Conn = tcp_client:get_sock(Ip, Port),
    Reply = tcp_recv(Conn),
    lager:debug("check_auth= ~p, Conn = ~p", [Reply, Conn]),
    State = #state{conn=Conn},
    gen_server:enter_loop(?MODULE, [], State, ?K_TIMEOUT).

handle_call({squery, Arg}, _From, #state{conn=Conn}=State) ->
    tcp_client:call(Conn, Arg),
    Reply = tcp_recv(Conn),
    {reply, Reply, State, ?K_TIMEOUT};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    {reply, equery(Conn, Stmt, Params), State, ?K_TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?K_TIMEOUT}.

handle_cast(Msg, State) ->
    lager:debug("Msg = ~p", [Msg]),
    {noreply, State, ?K_TIMEOUT}.

handle_info(timeout, #state{conn = Conn} = State) ->
    lager:debug("Info = ~p", [timeout]),
    tcp_client:keepalive_sock(Conn),
    case tcp_recv(Conn) of
        {error, Reason} ->
            lager:error("Reason = ~p", [{error, Reason}]);
        Reply -> 
        lager:debug("keepalive= ~p", [Reply])
    end,
    {noreply, State, ?K_TIMEOUT};

handle_info(Info, State) ->
    lager:debug("Info = ~p", [Info]),
    {noreply, State, ?K_TIMEOUT}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


tcp_recv(Conn) -> 
    case gen_tcp:recv (Conn, 0, 30000) of                 
        {ok, Binary} -> decode(Binary);
        {error, Reason} -> 
            lager:error("Reason = ~p", [{error, Reason}]),
            {error, Reason}
    end.


tcp_recv(Conn, N, TimeoutMS) -> 
    case gen_tcp:recv (Conn, N, TimeoutMS) of                 
        {ok, Binary} -> decode(Binary);
        {error, Reason} -> throw (Reason) 
    end.

squery(PoolName, Data) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                            gen_server:call(Worker, {squery, Data})
                                  end).



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
decode(Msg) ->
    binary_to_term(Msg).


close(_Conn) ->
    ok.

squery() ->
    ok.

equery(_X, _Y, _Z) ->
    ok.

%% ------------------------------------------------------------------
%% eunit test 
%% ------------------------------------------------------------------
squery_test() ->
    M = io,
    F = format,
    A = ["~p lxw~n", [xxxx]],
    squery(pool1,{M, F, A}).
