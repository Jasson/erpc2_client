-module(tcp_client).

-export([client/0,
         client/1,
         execute/0,
         call/1,
         keepalive_sock/1,
         call/2,
         test/0,
         loop_true/1,
         loop/1,
         connect/0,
         connect/1,
         connect/2,
         get_sock/2,
         start/0,
         send/0,
         send/2,
         auth/0,
         client/2]).

-define(SEND_TIMEOUT,5000).  

client() ->
    client(5554, [{<<"title">>, <<"hisss">>}]).

client(Message) ->
    client(5554, Message).

client(PortNo, Message) ->
    {ok, Sock} = gen_tcp:connect("test.com", PortNo, [{active, false},
    %{ok, Sock} = gen_tcp:connect("11.120.22.112", PortNo, [{active, false}, 
                                                            {packet,2}]),
    gen_tcp:send(Sock,Message),
    A = gen_tcp:recv(Sock,0),
    B = gen_tcp:recv(Sock,0),
    gen_tcp:close(Sock),
    {A, B}.

connect() ->
    ClientId = binary_to_list(get_client_id()), 
    connect(ClientId).
connect(ClientId) ->
    Ip = get_ip(),
    Port = get_tcp_port(),
    {ok, Sock} = gen_tcp:connect(Ip, Port, [{active, false},
    %{ok, Sock} = gen_tcp:connect(Ip, Port, [{active, true},
                                            binary,
                                            {packet, 2}]),
    Name = list_to_atom(ClientId),
    case erlang:whereis(Name) of
            undefined ->
                ok; 
            _Pid->
                unregister(Name)
    end,
    register(Name, Sock),
    process_flag(trap_exit, true),
    Msg = [{<<"title">>, <<"hisss">>}],
    gen_tcp:send(Sock, encode(Msg)),
    spawn_link(?MODULE, loop, [{Sock, ClientId}]),
    Sock.

connect(Ip, Port) ->
    ClientId = ss,
    {ok, Sock} = gen_tcp:connect(Ip, Port, [{active, false},
    %{ok, Sock} = gen_tcp:connect(Ip, Port, [{active, true},
                                            binary,
                                            {packet, 2}]),
    process_flag(trap_exit, true),
    Msg = [{<<"title">>, <<"hisss">>}],
    gen_tcp:send(Sock, encode(Msg)),
    spawn_link(?MODULE, loop, [{Sock, ClientId}]),
    Sock.

get_sock(Ip, Port) ->
    {ok, Sock} = gen_tcp:connect(Ip, Port, [{active, false},
   %{ok, Sock} = gen_tcp:connect(Ip, Port, [{active, true},
                                            binary,
                                            {send_timeout, ?SEND_TIMEOUT},
                                            {keepalive, true},
                                            {packet, 2}]),
    %Msg = [{<<"title">>, <<"hisss">>}],
    %gen_tcp:send(Sock, encode(Msg)),
    %spawn_link(?MODULE, loop, [{Sock, ClientId}]),
    send_auth(Sock),
    Sock.

keepalive_sock(Sock) ->
    M = [
        {<<"message_trait">>, <<"keepalive">>},
        {<<"msg_type">>, <<"keepalive">>}],
    gen_tcp:send(Sock, encode(M)).





loop_true({Sock, ClientId}) ->
    ok.
loop({Sock, ClientId}) ->
    case gen_tcp:recv(Sock, 0, 30000) of
        {error, ebadf} ->
            lager:warning("error, abadf");
        {error, timeout} ->
            lager:warning("error, timeout"),
            keepalive(ClientId),
            loop({Sock, ClientId});
        {error, einval} ->
                lager:warning("error, einval");
        {error, closed} ->
                lager:warning("error, closed");
        {error, Error} ->
                lager:warning("error, Error");
        {ok, A} ->
            lager:debug("A = ~p", [A]),
            lager:debug("A = ~p", [decode(A)]),
            loop({Sock, ClientId});
        A ->
            lager:debug("A = ~p", [A]),
            loop({Sock, ClientId})
    end.


send() ->
    Msg = [{<<"title">>, <<"hisss">>}],
    ClientId = get_client_id(),
    send(ClientId, Msg).

send(ClientId, Msg) ->
    Sock = get_pid_by_client_id(ClientId),
    lager:debug("Sock = ~p", [Sock]),
    gen_tcp:send(Sock, encode(Msg)).

send_message(ClientId, Msg) ->
    send(ClientId, Msg).

get_default_client_pid() ->
    ClientId = get_client_id(),
    get_pid_by_client_id(binary_to_list(ClientId)).

-spec get_pid_by_client_id(ClientId::string()|binary()) -> pid().
get_pid_by_client_id(ClientId) when is_binary(ClientId) ->
    get_pid_by_client_id(binary_to_list(ClientId));
get_pid_by_client_id(ClientId) ->
    Name = list_to_atom(ClientId),
    Pid = erlang:whereis(Name),
    Pid.


auth() ->
    ClientId = get_client_id(),
    SeqId = seq_id(),
    M = [{<<"msg_type">>, <<"check_auth">>},
         {<<"seq_id">>, SeqId},
         {<<"key">>, <<"EFFC047E2A226960B3EF1E81992CBECD">>}
        ],
    send_message(ClientId, M).

send_auth(Sock) ->
    SeqId = seq_id(),
    M = [{<<"msg_type">>, <<"check_auth">>},
         {<<"seq_id">>, SeqId},
         {<<"key">>, <<"EFFC047E2A226960B3EF1E81992CBECD">>}
        ],
    gen_tcp:send(Sock, encode(M)).

call({M, F, A}) ->
    ClientId = get_client_id(),
    SeqId = seq_id(),
    Msg = [{<<"msg_type">>, <<"execute">>},
           {<<"seq_id">>, SeqId},
           {<<"m">>, M},
           {<<"f">>, F},
           {<<"a">>, A}],
    send_message(ClientId, Msg).

call(Conn, {M, F, A}) ->
    SeqId = seq_id(),
    Msg = [{<<"msg_type">>, <<"execute">>},
           {<<"seq_id">>, SeqId},
           {<<"m">>, M},
           {<<"f">>, F},
           {<<"a">>, A}],
    gen_tcp:send(Conn, encode(Msg)).


execute() ->
    ClientId = get_client_id(),
    SeqId = seq_id(),
    M = [{<<"msg_type">>, <<"execute">>},
         {<<"seq_id">>, SeqId},
         {<<"m">>, io},
         {<<"f">>, format},
         {<<"a">>, ["~p~n", [testsssss]]}
        ],
    send_message(ClientId, M).

seq_id() ->
    os:timestamp().


encode(Msg) ->
    term_to_binary(Msg).

decode(Msg) ->
    binary_to_term(Msg).

test() ->
    connect(),
    auth(),
    execute(),
    ok.
start() ->
    ok.

keepalive(ClientId) ->
    M = [
        {<<"device">>, list_to_binary(ClientId)},
        {<<"message_trait">>, <<"keepalive">>},
        {<<"msg_type">>, <<"keepalive">>}],
    send_message(ClientId, M).

get_client_id() ->
    <<"client_id">>.

get_tcp_port() ->
    file_config(ecache, dst_tcp_port, 5554).

get_ip() ->
    file_config(ecache, dst_ip, "test.com").

file_config(Application,Part,Default) ->
    case application:get_env(Application,Part) of
        {ok,Res} ->           
            Res;              
        _NoThisConfig ->
            Default
    end.


