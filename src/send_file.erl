%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc API to transfer a file. Handles restarts (partial downloads)
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

-module(send_file).

-include_lib("kernel/include/file.hrl").

%% API
-export([send_file/4]).


%%%===================================================================
%%% API
%%%===================================================================


%% @doc send file to server returns {ok, BytesSent, FileSize}
-spec send_file(string(), integer(), string(),
        {destination, string()} | {uuid, string()} | {directory, string()})
        -> {ok, integer(), integer()}.
send_file(Host, Port, Filepath, {destination, _To} = Destination) ->
    do_send_file(Host, Port, Filepath, Destination);
send_file(Host, Port, Filepath, {uuid, _Uuid} = Destination) ->
    do_send_file(Host, Port, Filepath, Destination);
send_file(Host, Port, Filepath, {directory, _Directory} = Destination) ->
    do_send_file(Host, Port, Filepath, Destination).

%%%===================================================================
%%% Local functions
%%%===================================================================

%% Sends a file to the server. Handles previous partial attempts.
do_send_file(Host, Port, Filepath, Destination) ->
    try
        {ok, FileSize, Checksum, Basename} = get_file_attributes(Filepath),
        ServerMessage = construct_message_to_server(Basename, Checksum, Destination, FileSize),
        communicate_with_server(Host, Port, ServerMessage, Filepath, FileSize)
    catch
        _Exception:Reason -> map_error_return(Reason)
    end.

%% Get the attributes we need, including computed attributes such as checksum
get_file_attributes(Filepath) ->
    {ok, #file_info{size = FileSize}} = file:read_file_info(Filepath),
    {ok, Checksum} = checksum(Filepath),
    Basename = filename:basename(Filepath),
    {ok, FileSize, Checksum, Basename}.

%% @doc Create message to server
construct_message_to_server(Basename, Checksum, Destination, FileSize) ->
    [{filename, Basename}, Destination, {size, FileSize}, {checksum, Checksum}].

%% Open socket to server and send/receive messages.
communicate_with_server(Host, Port, ServerMessage, Filepath, FileSize) ->
    {ok, Socket} = gen_tcp:connect(Host, Port,
                        [binary, {packet, raw}, {active, false}]),
    gen_tcp:send(Socket, term_to_binary(ServerMessage)),
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    RV = handle_response_packet(binary_to_term(Packet), Socket, Filepath, FileSize),
    gen_tcp:close(Socket),
    RV.

%% @doc Handle response and perform appropriate action
handle_response_packet(already_downloaded, _Socket, _Filepath, FileSize) ->
    {ok, 0, FileSize};
handle_response_packet({ok, ExistingSize}, Socket, Filepath, FileSize) ->
    {ok, Fd} = file:open(Filepath, [raw, binary, read]),
    {ok, BytesSent} = file:sendfile(Fd, Socket, ExistingSize, 0, []),
    file:close(Fd),
    {ok, BytesSent, FileSize}.

%% Compute checksum
checksum(Filepath) ->
    checksum_rv(checksums:md5sum(Filepath)).

%% Handles creating a return value (rv) from checksums:md5sum()
checksum_rv({error, _Reason}) -> error;
checksum_rv(Checksum) -> {ok, Checksum}.

%% Map error to a return value
map_error_return({badmatch, {error, econnrefused} = Error}) -> Error;
map_error_return({badmatch, {error, nxdomain}}) -> {error, unknown_host};
map_error_return({badmatch, {error, enoent} = Error}) -> Error;
map_error_return(Reason) ->
    io:format("~p~n", [Reason]), %% Switch to error logging.
    {error, unknown}.
