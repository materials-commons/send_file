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
-module(sf_sup).

-behaviour(supervisor).

%% API
-export([start_link/3, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Args), {I, {I, start_link, Args}, temporary, 1000, worker, [I]}).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
start_link(LSocket, AllowedUsers, CertDir) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSocket, AllowedUsers, CertDir]).

%% @doc start a new child.
start_child() ->
    supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize supervisor
init([LSocket, AllowedUsers, CertDir]) ->
    SfServer = ?CHILD(sf_server, [LSocket, AllowedUsers, CertDir]),
    {ok, { {simple_one_for_one, 0, 1}, [SfServer] } }.

