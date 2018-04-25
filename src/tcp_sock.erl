-module(tcp_sock).
-export([receiver/2, 
         connect/3,
         send_auth/1,
         send/2,
         receiver_loop/3]).

%% 60 (secs)
-define(TIMEOUT, 60000).

-define(TCP_OPTIONS, [
    binary,
    {packet,    2},
    {reuseaddr, true},
    {nodelay,   true},
    {active,    false},
    {reuseaddr, true},
    {send_timeout,  ?TIMEOUT}]).


connect(ClientPid, Host, Port) when is_pid(ClientPid) ->
    case connect(Host, Port) of
        {ok, Sock} ->
            ReceiverPid = spawn_link(?MODULE, receiver, [ClientPid, Sock]),
            controlling_process(Sock, ReceiverPid),
            {ok, Sock, ReceiverPid};
        {error, Reason} ->
            {error, Reason}
    end.



connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, ?TIMEOUT) of
        {ok, Sock} -> tune_buffer(Sock),
                      {ok, Sock};
        Error      -> Error
    end.

controlling_process(Sock, Pid) when is_port(Sock) ->
    gen_tcp:controlling_process(Sock, Pid).

send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

tune_buffer(Sock) ->
    {ok, [{recbuf, RecBuf}, {sndbuf, SndBuf}]}
        = inet:getopts(Sock, [recbuf, sndbuf]),
    inet:setopts(Sock, [{buffer, max(RecBuf, SndBuf)}]).




%%% Receiver Loop
receiver(ClientPid, Sock) ->
    receiver_activate(ClientPid, Sock, parser()).

receiver_activate(ClientPid, Sock, ParserFun) ->
    setopts(Sock, [{active, once}]),
    erlang:hibernate(?MODULE, receiver_loop, [ClientPid, Sock, ParserFun]).

receiver_loop(ClientPid, Sock, ParserFun) ->
    receive
        {tcp, Sock, Data} ->
            lager:debug("[tcp~p] RECV: ~p", [ClientPid, Data]),
            parse_received_data(ClientPid, Data, ParserFun),
            receiver_activate(ClientPid, Sock, ParserFun );
        {tcp_error, Sock, Reason} ->
            exit({tcp_error, Reason});
        {tcp_closed, Sock} ->
            exit(tcp_closed);
        {ssl, _SslSock, Data} ->
            parse_received_data(ClientPid, Data, ParserFun),
            receiver_activate(ClientPid, Sock, ParserFun );
        {ssl_error, _SslSock, Reason} ->
            exit({ssl_error, Reason});
        {ssl_closed, _SslSock} ->
            exit(ssl_closed);
        stop ->
            close(Sock), exit(normal)
    end.

%% @doc Set socket options.
setopts(Sock, Opts) when is_port(Sock) ->
    inet:setopts(Sock, Opts).

close(Sock) when is_port(Sock) ->
        gen_tcp:close(Sock).


parser() ->
    fun(Bin) -> binary_to_term(Bin) end.

parse_received_data(ClientPid, Data, ParserFun) ->
    Packet = ParserFun(Data),
    ClientPid ! {tcp_recv, self(), packet, Packet}.



keepalive_sock(Sock) ->
    M = [
        {<<"message_trait">>, <<"keepalive">>},
        {<<"msg_type">>, <<"keepalive">>}],
    gen_tcp:send(Sock, encode(M)).




send_auth(Sock) ->
    SeqId = seq_id(),
    M = [{<<"msg_type">>, <<"check_auth">>},
         {<<"seq_id">>, SeqId},
         {<<"key">>, <<"EFFC047E2A226960B3EF1E81992CBECD">>}
        ],
    gen_tcp:send(Sock, encode(M)).

call(Conn, {M, F, A}) ->
    SeqId = seq_id(),
    Msg = [{<<"msg_type">>, <<"execute">>},
           {<<"seq_id">>, SeqId},
           {<<"m">>, M},
           {<<"f">>, F},
           {<<"a">>, A}],
    gen_tcp:send(Conn, encode(Msg)).


seq_id() ->
    os:timestamp().


encode(Msg) ->
    term_to_binary(Msg).

decode(Msg) ->
    binary_to_term(Msg).

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





