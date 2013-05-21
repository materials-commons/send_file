-module(sslserver).
-export([start/0]).

start() ->
	ssl:start(),
    {ok, ListenSocket} = gen_tcp:listen(9999, [binary, {packet, raw},
                        {active, true}, {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, false}]),
    {ok, SSLSocket} = ssl:ssl_accept(Socket, [{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
    											{keyfile, "/usr/local/etc/sf/cert/key.pem"}]),
    {ok, Data} = recv_all(SSLSocket),
    io:format("Data = ~p~n", [Data]),
    io:format("As binary ~p~n", [binary_to_term(Data)]),
    {ok, Fd} = file:open("/tmp/test.txt", [raw, binary, write]),
    recv_data(SSLSocket, Fd),
    file:close(Fd),
    ssl:close(SSLSocket).
    %%ssl:setopts(SSLSocket, [{active, true}]).

recv_all(SSLSocket) ->
    {ok, Data} = ssl:recv(SSLSocket, 0),
    recv_rest(SSLSocket, Data).

recv_rest(SSLSocket, AlreadyReceived) ->
    case ssl:recv(SSLSocket, 0, 100) of
        {ok, Data} -> recv_rest(SSLSocket, <<AlreadyReceived/binary, Data/binary>>);
        {error, timeout} -> {ok, AlreadyReceived};
        _ -> error
    end.

recv_data(SSLSocket, Fd) ->
    case ssl:recv(SSLSocket, 0) of
        {ok, Data} ->
            file:write(Fd, Data),
            recv_data(SSLSocket, Fd);
        Other ->
            io:format("Other ~p~n", [Other])
    end.
