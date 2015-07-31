%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_lpr_sup).
-behaviour(supervisor2).

-export([start_link/2, init/1]).
-export([start_client/1]).

start_link(Listeners, []) ->
    {ok, SupPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Listeners]),
    {ok, SupPid}.

init([{Listeners}]) ->
    {ok, SocketOpts} = application:get_env(rabbitmq_printer, tcp_listen_options),
    {ok, {{one_for_all, 10, 10},
        [{rabbit_lpr_client_sup,
            {rabbit_client_sup, start_link_worker,
                [{local, rabbit_lpr_client_sup}, {rabbit_lpr_reader, start_link, []}]},
            transient, infinity, supervisor, [rabbit_client_sup]} |
        listener_specs(fun tcp_listener_spec/1, [SocketOpts], Listeners)]
    }}.

listener_specs(Fun, Args, Listeners) ->
    [Fun([Address | Args]) ||
        Listener <- Listeners,
        Address  <- rabbit_networking:tcp_listener_addresses(Listener)].

tcp_listener_spec([Address, SocketOpts]) ->
    rabbit_networking:tcp_listener_spec(
        rabbit_lpr_listener_sup, Address, SocketOpts,
        printer, "Printer TCP Listener",
        {?MODULE, start_client, []}).

start_client(Sock, SockTransform) ->
    {ok, Reader} = supervisor:start_child(rabbit_lpr_client_sup, []),
    ok = rabbit_net:controlling_process(Sock, Reader),
    ok = gen_server2:call(Reader, {go, Sock, SockTransform}, infinity),
    gen_event:which_handlers(error_logger),
    Reader.

start_client(Sock) ->
    start_client(Sock, fun (S) -> {ok, S} end).

% end of file
