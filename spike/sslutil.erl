-module(sslutil).

-export([recv_all2/1, recv_all/1, recv_rest/2]).

recv_all2(SSLSocket) ->
    {ok, Data} = ssl:recv(SSLSocket, 0),
    % {ok, Data2} = ssl:recv(SSLSocket, 0),
    % All = <<Data/binary, Data2/binary>>,
    {ok, Data}.

recv_all(SSLSocket) ->
    case ssl:recv(SSLSocket, 0) of
        {ok, Data} -> recv_rest(SSLSocket, Data);
        Reason -> io:format("recv_all error ~p~n", [Reason]), error
    end.

recv_rest(SSLSocket, AlreadyReceived) ->
    case ssl:recv(SSLSocket, 0, 10000) of
        {ok, Data} -> recv_rest(SSLSocket, <<AlreadyReceived/binary, Data/binary>>);
        {error, timeout} -> {ok, AlreadyReceived};
        _ -> error
    end.