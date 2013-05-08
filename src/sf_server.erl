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

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(port()) -> {ok, pid()} | ignore | {error, string()}.
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize state. Most setup work is done in handle_info
%%      timeout.
%%--------------------------------------------------------------------
init([LSocket]) ->
    {ok, #state{lsocket = LSocket, fd = not_set}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
%handle_info({tcp, Socket, Filename}, State) when State#state.fd =:= not_set ->
handle_info({tcp, Socket, Request}, State) when State#state.fd =:= not_set ->
    {ok, Filename, Destination, Size, Checksum} = splitout_request_data(Request),
    Filepath = construct_file_path(Destination, Filename),
    DownloadedSize = get_file_size(Filepath),
    case size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) of
        true ->
            send_already_downloaded(Socket),
            RV = {stop, normal, State};
        false ->
            NewState = prepare_download(Filepath, DownloadedSize, State, Socket),
            RV = {noreply, NewState}
    end,
    RV;
handle_info({tcp, _Socket, RawData}, #state{fd = Fd} = State) ->
    ok = file:write(Fd, RawData),
    {noreply, State};
handle_info(timeout, #state{lsocket = LSocket} = State) ->
    {ok, _Socket} = gen_tcp:accept(LSocket),
    sf_sup:start_child(),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
terminate(_Reason, #state{fd = Fd}) when Fd =:= not_set ->
    ok;
terminate(_Reason, #state{fd = Fd}) ->
    file:close(Fd),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_already_downloaded(Socket) ->
    gen_tcp:send(Socket, term_to_binary(already_downloaded)).

prepare_download(Filepath, FileSize, State, Socket) ->
    {ok, Fd} = open_file(Filepath, FileSize),
    gen_tcp:send(Socket, term_to_binary({ok, FileSize})),
    State#state{fd = Fd}.

size_and_checksum_match(Filepath, Size, DownloadedSize, Checksum) ->
    case Size =:= DownloadedSize of
        true ->
            DownloadedChecksum = checksums:md5sum(Filepath),
            Checksum =:= DownloadedChecksum;
        false ->
            false
    end.

open_file(Filepath, FileSize) ->
    case FileSize of
        0 ->
            file:open(Filepath, [raw, binary, write]);
        _ ->
            file:open(Filepath, [raw, binary, append])
    end.

get_file_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo} ->
            FileInfo#file_info.size;
        {error, enoent} ->
            0
    end.

splitout_request_data(RequestData) ->
    [{_, Filename}, Destination, {_, Size}, {_, Checksum}] =
            binary_to_term(RequestData),
    {ok, Filename, Destination, Size, Checksum}.

construct_file_path({uuid, Uuid}, Filename) ->
    filename:join(["/tmp/t", Uuid ++ "_" ++ Filename]);
construct_file_path({destination, Filepath}, _Filename) -> Filepath.
