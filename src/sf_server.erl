%%%-------------------------------------------------------------------
%%% @author V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%% @copyright (C) 2012, V. Glenn Tarcea
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2012 by V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%%-------------------------------------------------------------------
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
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSocket]) ->
    {ok, #state{lsocket = LSocket, fd = not_set}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%handle_info({tcp, Socket, Filename}, State) when State#state.fd =:= not_set ->
handle_info({tcp, Socket, Request}, State) when State#state.fd =:= not_set ->
    %RequestBin = list_to_binary(Request),
    %io:format("RequestBin =")
    {ok, Filename, Uuid, Size, Checksum} = splitout_request_data(Request),
    Filepath = filename:join(?BASE_PATH, Filename),
    {ok, Fd} = file:open(Filepath, [raw, binary, write]),
    gen_tcp:send(Socket, term_to_binary({ok, 23})),
    {noreply, State#state{fd = Fd}};
handle_info({tcp, _Socket, RawData}, #state{fd = Fd} = State) ->
    ok = file:write(Fd, RawData),
    {noreply, State};
handle_info(timeout, #state{lsocket = LSocket} = State) ->
    {ok, _Socket} = gen_tcp:accept(LSocket),
    sf_sup:start_child(),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{fd = Fd} = State) ->
    file:close(Fd),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_already_downloaded(Socket) ->
    gen_tcp:send(Socket, term_to_binary(already_downloaded)).

prepare_download(Filepath, FileSize, #state{lsocket = Socket} = State) ->
    {ok, Fd} = open_file(Filepath, FileSize),
    io:format("~p Sending {ok, FileSize} on socket as ~p~n", [self(), term_to_binary({ok, FileSize})]),
    gen_tcp:send(Socket, term_to_binary({ok, FileSize})),
    io:format("~p Done sending {ok, FileSize}~n", [self()]),
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
    Term = binary_to_term(RequestData),
    io:format("splitout_request = ~p~n", [Term]),
    [{_, Filename}, {_, Uuid}, {_, Size}, {_, Checksum}] = binary_to_term(RequestData),
    {ok, Filename, Uuid, Size, Checksum}.

construct_file_path(Uuid, Filename) ->
    filename:join(["/tmp", Filename]).
    %filename:join(["/tmp", Uuid, Filename]).
