-module(sslclient).
-export([start/0, start2/0]).

-include_lib("kernel/include/file.hrl").

start() ->
    ssl:start(),
    {ok, Socket} = gen_tcp:connect("localhost", 9999, [binary, {packet, raw}, {active, false}], infinity),
    %RV1 = gen_tcp:controlling_process(Socket, self()),
    %io:format("RV1 = ~p~n", [RV1]),
    {ok, SSLSocket} = ssl:connect(Socket, [binary, {packet, raw}, {active, false},
                            {certfile, "/usr/local/etc/sf/cert/certificate.pem"},
                            {keyfile, "/usr/local/etc/sf/cert/key.pem"}], infinity),
    ssl:setopts(SSLSocket, [{active, false}]),
    %RV2 = ssl:controlling_process(SSLSocket, self()),
    io:format("Client Sending ok~n"),
    ssl:send(SSLSocket, term_to_binary(ok)),
    io:format("Client waiting on ok~n"),
    {ok, Data} = sslutil:recv_all2(SSLSocket),
    io:format("  Got ~p~n", [Data]),
    io:format("  As ~p~n", [binary_to_term(Data)]),
    io:format("Client sending size~n"),
    FileSize = file_size("/home/gtarcea/test.txt"),
    ssl:send(SSLSocket, term_to_binary({bytes, 10000})),
    {ok, Fd} = file:open("/home/gtarcea/test.txt", [raw, binary, read]),
    io:format("Client sending file~n"),
    sendfile(Fd, SSLSocket, 0, []),
    io:format("Client closing socket~n"),
    ssl:close(SSLSocket).

start2() ->
	ssl:start(),
	{ok, Socket} = gen_tcp:connect("localhost", 9999, [binary, {active, false}], infinity),
    RV1 = gen_tcp:controlling_process(Socket, self()),
    io:format("RV1 = ~p~n", [RV1]),
    {ok, SSLSocket} = ssl:connect(Socket, [binary, {packet, raw}, {active, false},
    						{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
    						{keyfile, "/usr/local/etc/sf/cert/key.pem"}], infinity),
    ssl:setopts(SSLSocket, [{active, false}]),
    RV2 = ssl:controlling_process(SSLSocket, self()),
    io:format("RV2 = ~p~n", [RV2]),
    io:format("client sending ok~n"),
    ssl:send(SSLSocket, term_to_binary(ok)),
    io:format("client waiting on ok~n"),
    {ok, Data} = sslutil:recv_all(SSLSocket),
    io:format("  Got ~p~n", [Data]),
    io:format("  As ~p~n", [binary_to_term(Data)]),
    %RV2 = ssl:controlling_process(SSLSocket, self()),
    io:format("client sending file~n"),
    {ok, Fd} = file:open("/home/gtarcea/test.txt", [raw, binary, read]),
    SendFileRV = sendfile(Fd, SSLSocket, 0, []),
    io:format("SendFileRV = ~p~n", [SendFileRV]),
    io:format("client waiting on ok~n"),
    {ok, Data2} = sslutil:recv_all(SSLSocket),
    io:format("as binary ~p~n", [term_to_binary(Data2)]),
    ok.
    %ssl:send(SSLSocket, term_to_binary(ok)).

sendfile(Fd, Socket, BytesOffset, _Options) ->
    file:position(Fd, {bof, BytesOffset}),
    send_file_contents(Fd, Socket).

send_file_contents(Fd, Socket) ->
    case file:read(Fd, 20000000) of
        {ok, Data} ->
            io:format("Sending ~p~n", [Data]),
            ssl:send(Socket, Data),
            send_file_contents(Fd, Socket);
        eof -> ok;
        ebadf -> {error, ebadf};
        {error, Reason} -> {error, Reason}
    end.

file_size(Filepath) ->
    {ok, #file_info{size = FileSize}} = file:read_file_info(Filepath),
    FileSize.