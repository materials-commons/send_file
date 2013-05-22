%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc The supervisor for sf_server.
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

-module(sf_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_cert_dir/0]).

%% API
-export([start/0]).

-define(DEFAULT_PORT, 1055).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Short cut to start application. Handles starting dependencies.
start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(handyman),
    application:start(send_file).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ssl:start(),
    Port = handyconfig:get_env_default(send_file, port, ?DEFAULT_PORT),
    Allowed = handyconfig:get_env_default(send_file, allowed_users, all),
    case get_cert_dir() of
        {error, _Reason} = Error -> Error;
        CertDir ->
            startup(Port, Allowed, CertDir)
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Local functions
%%%===================================================================

get_cert_dir() ->
    try
        validate_cert_files(application:get_env(send_file, certdir))
    catch
        Exception:Reason -> {error, enoent}
    end.

validate_cert_files({ok, Dir}) ->
    file_exists(filename:join([Dir, "certificate.pem"])),
    file_exists(filename:join([Dir, "key.pem"])),
    Dir;
validate_cert_files(undefined) ->
    io:format("No certfiles defined."),
    error(enoent).

file_exists(FilePath) ->
    case handyfile:file_exists(FilePath) of
        true -> ok;
        false ->
            io:format("File does not exist: ~s~n", [FilePath]),
            error(enoent)
    end.

startup(Port, _Allowed, _CertDir) ->
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {packet, 4},
                        {active, true}, {reuseaddr, true}]),
    case sf_sup:start_link(LSocket) of
        {ok, Pid} ->
            sf_sup:start_child(),
            {ok, Pid};
        Error ->
            {error, Error}
    end.
