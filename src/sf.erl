%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Command line utility for send_file.
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

-module(sf).

%% API main for script
-export([main/1]).

-define(OPTSPEC, [
            {host, $h, "host", {string, "materialscommons.org"},   "Host to send files to."},
            {port, $p, "port", {integer, 1055},         "Port to connect to."},
            {dir,  $d, "dir",  {string, "/tmp"},        "Directory to put files in."},
            {user, $u, "user", {string, username()}, "Username to login as"},
            {password, $p, "password", string, "Password to use"},
            {help, $?, "help", undefined,               "Show usage."}
        ]).

%%%===================================================================
%%% API
%%%===================================================================
main([]) -> usage();
main(Args) ->
    ssl:start(),
    {Host, Port, Directory, Files} = parse_results(getopt:parse(?OPTSPEC, Args)),
    send_files(Host, Port, Directory, Files).

%%%===================================================================
%%% Local
%%%===================================================================

usage({error, {Error, Description}}) ->
    io:format(standard_error, "~nError: ~p ~p~n~n", [Error, Description]),
    usage().

usage() ->
    getopt:usage(?OPTSPEC, "sf", "files"),
    halt().

parse_results({error, {_Error, _Description}} = ErrorValue) ->
    usage(ErrorValue);
parse_results({ok, {Values, Files}}) ->
    case lists:member(help, Values) of
        true -> usage();
        false -> options(Values, Files)
    end.

options(Values, Files) ->
    {host, Host} = lists:keyfind(host, 1, Values),
    {port, Port} = lists:keyfind(port, 1, Values),
    {dir, Dir} = lists:keyfind(dir, 1, Values),
    {Host, Port, Dir, Files}.

send_files(_Host, _Port, _Directory, []) -> ok;
send_files(Host, Port, Directory, [File|RemainingFiles]) ->
    io:format("Sending file ~s to host ~s:~p directory ~s~n", [File, Host, Port, Directory]),
    Status = send_file:send_file(Host, Port, File, {directory, Directory}),
    case success(Status, Host, File, Directory) of
        unrecoverable -> ok;
        _ -> send_files(Host, Port, Directory, RemainingFiles)
    end.

success({ok, _BytesSent, _FileSize}, _Host, _File, _Dir) -> ok;
success({error, econnrefused}, Host, _File, _Dir) ->
    not_recoverable("Unable to connect to host: " ++ Host);
success({error, unknown_host}, Host, _File, _Dir) -> not_recoverable("Unknown host: " ++ Host);
success({error, enoent}, _Host, File, _Dir) ->
    error_message("File is not readable or does not exist: '" ++ File ++ "'");
success({error, eacces}, _Host, File, Dir) ->
    error_message("Unable to write to destination '" ++ filename:join([Dir, File]) ++"' on remote host");
success(_Error, _Host, _File, _Dir) -> not_recoverable("An unknown error occurred").

not_recoverable(Message) ->
    error_message(Message),
    unrecoverable.

error_message(Message) ->
    io:format(standard_error, "  Transfer failed - ~s.~n", [Message]).

username() ->
    username(os:type()).

username({unix, _Os}) ->
    os:getenv("USER");
username({win32, _Os}) ->
    throw(notimplemented).

