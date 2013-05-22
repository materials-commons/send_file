-module(sslserver).
-export([start/0, start2/0]).

start() ->
    ssl:start(),
    {ok, ListenSocket} = gen_tcp:listen(9999, [binary,
                        {active, false}, {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %%inet:setopts(Socket, [{active, false}]),
    {ok, SSLSocket} = ssl:ssl_accept(Socket, [{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
                                                {keyfile, "/usr/local/etc/sf/cert/key.pem"}]),
    ssl:setopts(SSLSocket, [{active, false}]),
    io:format("Server waiting on ok~n"),
    {ok, Data} = sslutil:recv_all2(SSLSocket),
    io:format(" Got ~p~n", [Data]),
    io:format(" As ~p~n", [binary_to_term(Data)]),
    io:format("Server Sending ok~n"),
    ssl:send(SSLSocket, term_to_binary(ok)),
    io:format("Server receiving size"),
    {ok, Data2} = sslutil:recv_all2(SSLSocket),
    io:format(" Got ~p~n", [Data2]),
    io:format(" As ~p~n", [binary_to_term(Data2)]),
    io:format("Receiving file~n"),
    {ok, Fd} = file:open("/tmp/test.txt", [raw, binary, write]),
    recv_data(SSLSocket, Fd),
    io:format("Server Closing connection~n"),
    file:close(Fd),
    ssl:close(SSLSocket).

start2() ->
	ssl:start(),
    {ok, ListenSocket} = gen_tcp:listen(9999, [binary,
                        {active, false}, {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %%inet:setopts(Socket, [{active, false}]),
    {ok, SSLSocket} = ssl:ssl_accept(Socket, [{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
    											{keyfile, "/usr/local/etc/sf/cert/key.pem"}]),
    ssl:setopts(SSLSocket, [{active, false}]),
    io:format("Server waiting on ok~n"),
    {ok, Data} = sslutil:recv_all(SSLSocket),
    io:format("  Data = ~p~n", [Data]),
    io:format("Server sending ok~n"),
    ssl:send(SSLSocket, <<"ok">>),
    {ok, Fd} = file:open("/tmp/test.txt", [raw, binary, write]),
    io:format("Server receiving file~n"),
    recv_data(SSLSocket, Fd),
    io:format("Server sending ok~n"),
    ssl:send(SSLSocket, term_to_binary(ok)),
    io:format("Server waiting on close~n"),
    sslutil:recv_all(SSLSocket),
    file:close(Fd),
    ssl:close(SSLSocket).
    %%ssl:setopts(SSLSocket, [{active, true}]).

recv_data(SSLSocket, Fd) ->
    io:format("In recv_data~n"),
    case ssl:recv(SSLSocket, 0) of
        {ok, Data} ->
            io:format("Received data~n", []),
            file:write(Fd, Data),
            recv_data(SSLSocket, Fd);
        Other ->
            io:format("Other ~p~n", [Other])
    end.


