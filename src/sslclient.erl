-module(sslclient).
-export([start/0]).

start() ->
	ssl:start(),
	{ok, Socket} = gen_tcp:connect("localhost", 9999, [binary, {packet, raw}], infinity),
    RV1 = gen_tcp:controlling_process(Socket, self()),
    io:format("RV1 = ~p~n", [RV1]),
    {ok, SSLSocket} = ssl:connect(Socket, [binary, {packet, raw},
    						{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
    						{keyfile, "/usr/local/etc/sf/cert/key.pem"}], infinity),
    RV2 = ssl:controlling_process(SSLSocket, self()),
    io:format("RV2 = ~p~n", [RV2]),
    io:format("as binary = ~p~n", [term_to_binary(ok)]),
    ssl:send(SSLSocket, term_to_binary(ok)),
    %RV2 = ssl:controlling_process(SSLSocket, self()),
    {ok, Fd} = file:open("/home/gtarcea/test.txt", [raw, binary, read]),
    SendFileRV = file:sendfile(Fd, SSLSocket, 0, 0, []),
    io:format("SendFileRV = ~p~n", [SendFileRV]),
    ok.
    %ssl:send(SSLSocket, term_to_binary(ok)).