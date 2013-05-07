%%%-------------------------------------------------------------------
%%% @author V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%% @copyright (C) 2012, V. Glenn Tarcea
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2012 by V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%%-------------------------------------------------------------------
-module(sf_client).

-include_lib("kernel/include/file.hrl").

%% API
-export([send_file/1]).

%% Macros
-define(DEFAULT_PORT, 1055).


%%%===================================================================
%%% API
%%%===================================================================

send_file(Filepath) ->
    %{ok, Fd} = file:open(Filepath, [raw, binary, read]),
    {ok, FileInfo} = file:read_file_info(Filepath),
    io:format("Filesize: ~w~n", [FileInfo#file_info.size]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?DEFAULT_PORT, [binary, {packet, raw}, {active, false}]),
    Basename = filename:basename(Filepath),
    BinTerm = term_to_binary([{filename, Basename}, {uuid, "abc123"}, {size, 23}, {checksum, "abc123"}]),
    %gen_tcp:send(Socket, Basename),
    gen_tcp:send(Socket, BinTerm),
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    %timer:sleep(timer:seconds(10)),
    {ok, Size} = binary_to_term(Packet),
    io:format("Size = ~p~n", [Size]),
    %io:format("Sending file~n", []),
    {ok, Bytes} = file:sendfile(Filepath, Socket),
    Bytes = FileInfo#file_info.size,
    io:format("Sent: ~w~n", [Bytes]),
    %io:format("Sent file~n"),
    gen_tcp:close(Socket),
    ok.

file_exists(Filepath) ->
    case file:read_file_info(Filepath) of
        {ok, FileInfo} ->
            io:format("Size of file: ~w~n", [FileInfo#file_info.size]),
            ok;
        {error, enoent} ->
            not_exist
    end.
