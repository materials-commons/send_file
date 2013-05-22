%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc The server for accepting and writing file transfers.
%%%
%%% @copyright Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================
-module(sf_server).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BASE_PATH, "/tmp").

-record(state, {lsocket, fd}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start_link(port()) -> {ok, pid()} | ignore | {error, string()}.
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%% @doc Initialize state. Most setup work is done in handle_info timeout.
init([LSocket]) ->
    {ok, #state{lsocket = LSocket, fd = not_set}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ssl, Socket, Request}, #state{fd = Fd} = State) when Fd =:= not_set ->
    {ok, Filename, Destination, Size, Checksum} = splitout_request_data(Request),
    Filepath = construct_file_path(Destination, Filename),
    DownloadedSize = get_file_size(Filepath),
    case size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) of
        true ->
            send_already_downloaded(Socket),
            {stop, normal, State};
        false ->
            NewState = proceed_with_download(Filepath, DownloadedSize, State, Socket),
            {noreply, NewState}
    end;
handle_info({ssl, _Socket, RawData}, #state{fd = Fd} = State) ->
    ok = file:write(Fd, RawData),
    {noreply, State};
handle_info(timeout, #state{lsocket = LSocket} = State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    ok = inet:setopts(Socket, [{active, false}]),
    {ok, SSLSocket} = ssl:ssl_accept(Socket,
                        [{certfile, "/usr/local/etc/sf/cert/certificate.pem"},
                         {keyfile, "/usr/local/etc/sf/cert/key.pem"}]),
    inet:setopts(Socket, [{active, true}]),
    ok = ssl:setopts(SSLSocket, [{active, true}]),
    sf_sup:start_child(),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, Socket}, State) ->
    ssl:close(Socket),
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("handle_info fallthrough Info ~p~n", [Info]),
    {noreply, State}.

%% @private
%% @doc If a file was opened then close it before terminating.
terminate(_Reason, #state{fd = Fd}) when Fd =:= not_set ->
    ok;
terminate(_Reason, #state{fd = Fd}) ->
    file:close(Fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_already_downloaded(Socket) ->
    ssl:send(Socket, term_to_binary(already_downloaded)).

proceed_with_download(Filepath, FileSize, State, Socket) ->
    case open_file(Filepath, FileSize) of
        {ok, Fd} ->
            ssl:send(Socket, term_to_binary({ok, FileSize})),
            State#state{fd = Fd};
        {error, eacces} ->
            ssl:send(Socket, term_to_binary({error, eacces})),
            State;
        _Error ->
            ssl:send(Socket, term_to_binary({error, other})),
            State
    end.

size_and_checksum_match(_Filepath, Size, DownloadedSize, _Checksum) when Size =/= DownloadedSize ->
    false;
size_and_checksum_match(Filepath, _Size, _DownloadedSize, Checksum) ->
    DownloadedChecksum = checksums:md5sum(Filepath),
    Checksum =:= DownloadedChecksum.

open_file(Filepath, FileSize) when FileSize =:= 0 ->
    file:open(Filepath, [raw, binary, write]);
open_file(Filepath, _FileSize) ->
    file:open(Filepath, [raw, binary, append]).

get_file_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size = Size}} -> Size;
        {error, enoent} -> 0
    end.

splitout_request_data(RequestData) ->
    [{_, Filename}, Destination, {_, Size}, {_, Checksum}] =
            binary_to_term(RequestData),
    {ok, Filename, Destination, Size, Checksum}.

construct_file_path({uuid, Uuid}, Filename) ->
    filename:join(["/tmp/t", Uuid ++ "_" ++ Filename]);
construct_file_path({destination, Filepath}, _Filename) ->
    Filepath;
construct_file_path({directory, Directory}, Filename) ->
    filename:join([Directory, Filename]).
