%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_lpr).

-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    {ok, Listeners} = application:get_env(tcp_listeners),
    rabbit_lpr_sup:start_link({Listeners}, []).

stop(_State) ->
    ok.

% end of file
