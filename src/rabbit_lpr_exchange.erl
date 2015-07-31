%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_lpr_exchange).
-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(rabbit_exchange_type).

-export([description/0, serialise_events/0, route/2]).
-export([validate/1, validate_binding/2,
         create/2, delete/3, policy_changed/2,
         add_binding/3, remove_bindings/3, assert_args_equivalence/2]).
-export([publish/4]).

-rabbit_boot_step(
    {?MODULE,
     [{description, "exchange type x-printer"},
      {requires,    rabbit_registry},
      {enables,     kernel_ready},
      {mfa,         {rabbit_registry, register,
                     [exchange, <<"x-printer">>, ?MODULE]}},
      {cleanup,     {rabbit_registry, unregister,
                     [exchange, <<"x-printer">>]}}
     ]}).

description() ->
    [{description, <<"Printer Exchange">>}].

serialise_events() -> false.

route(#exchange{name      = Name,
                arguments = Args},
      #delivery{message = Msg}) ->
    % printing not implemented yet
    [].

validate(_X) -> ok.

validate_binding(_X, _B) -> ok.

create(_Tx, _X) -> ok.

delete(_Tx, _X, _Bs) -> ok.

policy_changed(_X1, _X2) -> ok.

add_binding(Tx, X, B) ->
    rabbit_exchange_type_topic:add_binding(Tx, X, B).

remove_bindings(Tx, X, Bs) ->
    rabbit_exchange_type_topic:remove_bindings(Tx, X, Bs).

assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).

% internal

publish(XName, RKey, Props, Body) ->
    case rabbit_exchange:lookup(XName) of
        {ok, X=#exchange{type='x-printer'}} ->
            Message = rabbit_basic:message(XName, RKey, Props, Body),
            publish(X, rabbit_basic:delivery(false, false, Message, undefined));
        {ok, _} -> {error, not_printer_exchange};
        Err -> Err
    end.

publish(X, Delivery) ->
    Qs = rabbit_amqqueue:lookup(rabbit_exchange_type_topic:route(X, Delivery)),
    DeliveredQPids = rabbit_amqqueue:deliver(Qs, Delivery),
    {ok, DeliveredQPids}.

% end of file
