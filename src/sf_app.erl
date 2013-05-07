%%%-------------------------------------------------------------------
%%% @author V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%% @copyright (C) 2012, V. Glenn Tarcea
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2012 by V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%%-------------------------------------------------------------------
-module(sf_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1055).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = get_port(),
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {packet, raw}, {active, true}, {reuseaddr, true}]),
    case sf_sup:start_link(LSocket) of
        {ok, Pid} ->
            sf_sup:start_child(),
            {ok, Pid};
        Error ->
            {error, Error}
    end.

get_port() ->
    case application:get_env(sf, port) of
        {ok, Port} ->
            Port;
        undefined ->
            ?DEFAULT_PORT
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

