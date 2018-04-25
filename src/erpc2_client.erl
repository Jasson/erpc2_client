-module(erpc2_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         squery/2
         squery/3
        ]).

-define(TIMEOUT, 600000).
-include_lib("eunit/include/eunit.hrl").
-define(K_TIMEOUT, 60000).
-define(READ_TIMEOUT,2500). % 2.5 sec  
-record(state, {sock, receiver, timeout = ?TIMEOUT}).


-%test  {now(),erpc2_client:squery(io,format, ["~p~n", [now()]])}.
squery(M, F, A) ->
    erpc2_client:squery(pool1,{M, F, A}).




%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

    % proc_lib:start_link(?MODULE, init, []).

start_link(Args) ->
    lager:debug("Args = ~p", [Args]),
    gen_server:start_link(?MODULE, [Args], []).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Ip = proplists:get_value(ip, Args, "test.com"),
    Port = proplists:get_value(port, Args, 5554),
    case connect_tcp(self(), Ip, Port) of
        {ok, State} -> 
            tcp_sock:send_auth(State#state.sock),
            case tcp_recv(State) of
                {error, Reason} ->
                    lager:error("Reason = ~p", [{error, Reason}]),
                    {stop, Reason};
                {ok, Reply} -> 
                    lager:debug("keepalive= ~p", [Reply]),
                    {ok, State}
            end;
        {error, Error} ->
            lager:error("Error = ~p", [Error]),
            {stop, Error}
    end.

handle_call({squery, Arg}, _From, #state{sock=Sock}=State) ->
    SendData = serialize(Arg),
    tcp_sock:send(Sock, SendData),
    Reply = case tcp_recv(State) of
                {error, Reason} ->
                    lager:error("Reason = ~p", [{error, Reason}]),
                    Reason;
                {ok, Data} -> 
                    lager:debug("keepalive= ~p", [Data]),
                    Data 
            end,
    {reply, Reply, State, ?K_TIMEOUT};
handle_call({equery, Stmt, Params}, _From, #state{sock=Sock}=State) ->
    {reply, equery(Sock, Stmt, Params), State, ?K_TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ok, State, ?K_TIMEOUT}.

handle_cast(Msg, State) ->
    lager:warning("Unexpected Msg= ~p", [Msg]),
    {noreply, State, ?K_TIMEOUT}.

handle_info(timeout, #state{sock= Sock} = State) ->
    lager:debug("Info = ~p", [timeout]),
    tcp_client:keepalive_sock(Sock),
    case tcp_recv(State) of
        {error, Reason} ->
            lager:error("Reason = ~p", [{error, Reason}]);
        {ok, Reply} -> 
            lager:debug("keepalive= ~p", [Reply])
    end,
    {noreply, State, ?K_TIMEOUT};

handle_info(Info, State) ->
    lager:warning("Unexpected Info = ~p", [Info]),
    {noreply, State, ?K_TIMEOUT}.

terminate(_Reason, #state{sock=Sock}) ->
    ok = close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

tcp_recv(#state{receiver = Receiver, timeout = Timeout}) ->
    receive
        {tcp_recv, Receiver, packet, Packet} ->
            lager:debug("Packet = ~p", [Packet]),
            {ok, Packet}
    after Timeout ->
        {error, tcp_timeout}
    end.

squery(Pid, Data) when is_pid(Pid) -> 
    gen_server:call(Pid, {squery, Data});
squery(PoolName, Data) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                            gen_server:call(Worker, {squery, Data})
                                  end).



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

close(_Sock) ->
    ok.

squery() ->
    ok.

equery(_X, _Y, _Z) ->
    ok.

connect_tcp(ClientId, Ip, Port) ->
    case tcp_sock:connect(ClientId, Ip, Port) of
        {ok, Sock, Receiver} ->
            {ok, #state{sock = Sock, receiver = Receiver}};
        {error, Reason} ->
            {error, Reason}
    end.

serialize({M, F, A}) ->
    SeqId = seq_id(),
    Msg = [{<<"msg_type">>, <<"execute">>},
           {<<"seq_id">>, SeqId},
           {<<"m">>, M},
           {<<"f">>, F},
           {<<"a">>, A}],
    encode(Msg).

seq_id() ->
    os:timestamp().

encode(Msg) ->
    term_to_binary(Msg).

decode(Msg) ->
    binary_to_term(Msg).




%% ------------------------------------------------------------------
%% eunit test 
%% ------------------------------------------------------------------
squery_test() ->
    M = io,
    F = format,
    A = ["~p lxw~n", [xxxx]],
    squery(pool1,{M, F, A}).


