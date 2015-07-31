%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_lpr_reader).
-behaviour(gen_server2).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("rabbit_lpr.hrl").

start_link() ->
    gen_server2:start_link(?MODULE, [], []).

init([]) ->
    {ok, undefined, hibernate, {backoff, 1000, 1000, 10000}}.

handle_call({go, Sock0, SockTransform}, _From, undefined) ->
    process_flag(trap_exit, true),
    {ok, Sock} = SockTransform(Sock0),
    case rabbit_net:connection_string(Sock, inbound) of
        {ok, ConnStr} ->
            rabbit_log:info("accepting connection (~s)", [ConnStr]),
            rabbit_net:async_recv(Sock, 0, infinity),
            {reply, ok, #state{socket=Sock, expect=command, buffer= <<>>}, hibernate};
        {error, enotconn} ->
            rabbit_net:fast_close(Sock),
            {stop, shutdown, undefined};
        {error, Reason} ->
            rabbit_net:fast_close(Sock),
            {stop, {network_error, Reason}, undefined}
    end;

handle_call(Msg, From, State) ->
    {stop, {lpr_unexpected_call, Msg, From}, State}.

handle_cast(Msg, State) ->
    {stop, {lpr_unexpected_cast, Msg}, State}.

handle_info({'EXIT', _Conn, Reason}, State) ->
    rabbit_log:debug("connection died"),
    {stop, {connection_died, Reason}, State};

handle_info({inet_reply, _Ref, ok}, State) ->
    {noreply, State, hibernate};

handle_info({inet_async, _Sock, _Ref, {ok, Data}}, State) ->
    process_received_bytes(Data, State);

handle_info({inet_async, _Sock, _Ref, {error, closed}}, #state{expect=Expect}=State) ->
    rabbit_log:debug("connection closed"),
    rabbit_lpr_parser:conclude(Expect, State),
    {stop, {shutdown, conn_closed}, State};

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
    rabbit_log:debug("~w", [Reason]),
    network_error(Reason, State);

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
    network_error(Reason, State);

handle_info({bump_credit, Msg}, State) ->
    credit_flow:handle_bump_msg(Msg),
    {noreply, State, hibernate};

handle_info(Msg, State) ->
    rabbit_log:error("unexpected message: ~w", [Msg]),
    {stop, {lpr_unexpected_msg, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal

process_received_bytes(<<>>, State) ->
    {noreply, State, hibernate};

process_received_bytes(Bytes, #state{socket=Sock, expect=Expect}=State) ->
    % rabbit_log:debug("bytes arrived ~w", [Bytes]),
    case rabbit_lpr_parser:parse(Expect, Bytes, State) of
        {more, Expect2, State2} ->
            rabbit_net:async_recv(Sock, 0, infinity),
            {noreply, State2#state{expect=Expect2}, hibernate};
        {stop, State2} ->
            {stop, normal, State2};
        {error, Reason, State2} ->
            rabbit_log:error("framing error: ~p", [Reason]),
            rabbit_net:port_command(Sock, <<16#FF>>),
            {stop, {shutdown, Reason}, State2}
    end.

network_error(Reason, State) ->
    rabbit_log:info("network error: ~p", [Reason]),
    {stop, {shutdown, conn_closed}, State}.

% end of file
