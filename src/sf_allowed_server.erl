%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Server for allowed users and directories.
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
-module(sf_allowed_server).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/1, allowed_users_updated/0, authenticate_user/2,
            directory_allowed/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
            allowed_users
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(AllowedUsers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AllowedUsers], []).

allowed_users_updated() ->
    gen_server:cast(?SERVER, allowed_users_updated).

authenticate_user(Username, Password) ->
    gen_server:call(?SERVER, {authenticate_user, Username, Password}).

directory_allowed(Username, Directory) ->
    gen_server:call(?SERVER, {directory_allowed, Username, Directory}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([AllowedUsers]) ->
    watch_allowed_users(AllowedUsers, 5),
    {ok, #state{allowed_users = AllowedUsers}}.

%% @private
handle_call({authenticate_user, _Username, _Password}, _From, State) ->
    {reply, ok, State};
handle_call({directory_allowed, _Username, _Directory}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(allowed_users_updated, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Local
%%%===================================================================
watch_allowed_users(FilePath, SleepInSeconds) ->
    {ok, #file_info{mtime = CurrentMTime}} = file:read_file_info(FilePath),
    spawn(fun() -> watcher(FilePath, CurrentMTime, SleepInSeconds) end),
    ok.

watcher(FilePath, LastMTime, SleepInSeconds) ->
    timer:sleep(SleepInSeconds * 1000), % convert to milliseconds
    {ok, #file_info{mtime = CurrentMTime}} = file:read_file_info(FilePath),
    allowed_users_updated(CurrentMTime =:= LastMTime),
    watcher(FilePath, CurrentMTime, SleepInSeconds).

allowed_users_updated(true) -> allowed_users_updated();
allowed_users_updated(false) -> ok.

