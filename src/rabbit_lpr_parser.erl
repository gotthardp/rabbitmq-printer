%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_lpr_parser).

-export([parse/3, conclude/2]).

-include("rabbit_lpr.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

parse(command, Bytes, State) ->
    case parse_command(Bytes, State) of
        {ok, {C, Operands}, State2} ->
            % rabbit_log:debug("command: ~w ~w", [C, Operands]),
            command(C, Operands, State2);

        {more, State2} ->
            {more, command, State2}
    end;

parse(subcommand, Bytes, State) ->
    case parse_command(Bytes, State) of
        {ok, {C, Operands}, State2} ->
            % rabbit_log:debug("subcommand: ~w ~w", [C, Operands]),
            subcommand(C, Operands, State2);

        {more, State2} ->
            {more, subcommand, State2}
    end;

parse(control_file, Bytes, #state{socket=Sock}=State) ->
    case parse_file(Bytes, State) of
        {ok, File, State2} ->
            % rabbit_log:debug("control file: ~s", [File]),
            Metadata = parse_control_file(binary:split(File, [<<"\r\n">>, <<"\n">>], [global])),
            rabbit_net:port_command(Sock, <<0>>),
            {more, subcommand, State2#state{metadata=Metadata}};

        {more, State2} ->
            {more, control_file, State2}
    end;

parse(data_file, Bytes, #state{socket=Sock}=State) ->
    case parse_file(Bytes, State) of
        {ok, Text, State2} ->
            rabbit_net:port_command(Sock, <<0>>),
            {more, subcommand, State2#state{text=Text}};

        {more, State2} ->
            {more, data_file, State2}
    end.

parse_command(Bytes, #state{buffer=Buffer}=State) ->
    Buffer2 = <<Buffer/binary, Bytes/binary>>,
    case binary:match(Buffer2, [<<$\r,$\n>>, <<$\n>>]) of
        nomatch ->
            {more, State#state{buffer=Buffer2}};
        {N,_} when N > 0 ->
            <<C, Frame/binary>> = binary:part(Buffer2, 0, N),
            Operands = string:tokens(binary_to_list(Frame), " \t\r\n"),
            Rest = binary:part(Buffer2, N, byte_size(Buffer2)-N-1),
            {ok, {C, Operands}, State#state{buffer=Rest}}
    end.

parse_file(Bytes, #state{buffer=Buffer, count=Count}=State) ->
    Buffer2 = <<Buffer/binary, Bytes/binary>>,
    case byte_size(Buffer2) of
        N when N >= Count+1 ->
            File = binary:part(Buffer2, 0, Count),
            Rest = binary:part(Buffer2, Count+1, byte_size(Buffer2)-(Count+1)),
            {ok, File, State#state{buffer=Rest}};
        _ ->
            {more, State#state{buffer=Buffer2}}
    end.

conclude(subcommand, #state{job=Job, metadata=_Metadata, text=Text}) ->
    {XName, RK} = parse_job(re:split(Job, "[/]")),
    case rabbit_lpr_exchange:publish(XName, RK, #'P_basic'{}, Text) of
        {ok, _} -> ok;
        Err ->
            rabbit_log:debug("cannot publish: ~p", [XName]),
            Err
    end;
conclude(_Expecting, _State) ->
    ok.

% Print any waiting jobs
command(01, [Queue], State) ->
    rabbit_log:debug("print any waiting jobs: ~s", [Queue]),
    {stop, State};

% Receive a printer job
command(02, [], #state{socket=Sock}=State) ->
    rabbit_log:debug("receive printer job: default queue"),
    rabbit_net:port_command(Sock, <<0>>),
    {more, subcommand, State#state{job=undefined}};

command(02, [Queue], #state{socket=Sock}=State) ->
    rabbit_log:debug("receive printer job: ~s", [Queue]),
    rabbit_net:port_command(Sock, <<0>>),
    {more, subcommand, State#state{job=Queue}};

% Send queue state (short)
command(03, [Queue|List], State) ->
    rabbit_log:debug("send queue state: ~s ~w", [Queue, List]),
    {stop, State};

% Send queue state (long)
command(04, [Queue|List], State) ->
    rabbit_log:debug("send queue state: ~s ~w", [Queue, List]),
    {stop, State};

% Remove jobs
command(05, [Queue, Agent|List], State) ->
    rabbit_log:debug("remove jobs: ~s ~s ~w", [Queue, Agent, List]),
    {stop, State};

command(Cmd, Operands, State) ->
    rabbit_log:warning("unknown command: ~w ~w", [Cmd, Operands]),
    {error, "unknown command", State}.

% Abort job
subcommand(01, [], State) ->
    rabbit_log:debug("abort job"),
    {more, subcommand, State};

% Receive control file
subcommand(02, [Count, Name], #state{socket=Sock}=State) ->
    rabbit_log:debug("receive control file: ~s ~s", [Count, Name]),
    rabbit_net:port_command(Sock, <<0>>),
    {more, control_file, State#state{count=list_to_integer(Count)}};

% Receive data file
subcommand(03, [Count, Name], #state{socket=Sock}=State) ->
    rabbit_log:debug("receive data file: ~s ~s", [Count, Name]),
    rabbit_net:port_command(Sock, <<0>>),
    {more, data_file, State#state{count=list_to_integer(Count)}};

subcommand(Cmd, Operands, #state{socket=Sock}=State) ->
    rabbit_log:warning("unknown subcommand: ~w ~w", [Cmd, Operands]),
    rabbit_net:port_command(Sock, <<16#FE>>),
    {more, subcommand, State}.

parse_control_file([]) -> [];
parse_control_file([<<>>]) -> [];
parse_control_file([<<C, Line/binary>>|Rest]) ->
    Command = binary_to_list(Line),
    rabbit_log:debug("control line ~c: ~s", [C, Command]),
    [{C, Command}|parse_control_file(Rest)].

parse_job([X]) -> {rabbit_misc:r(<<"/">>, exchange, X), undefined};
parse_job([X, RK]) -> {rabbit_misc:r(<<"/">>, exchange, X), RK};
parse_job([<<>>, X, RK]) -> {rabbit_misc:r(<<"/">>, exchange, X), RK};
parse_job([V, X, RK]) -> {rabbit_misc:r(V, exchange, X), RK}.

% end of file
