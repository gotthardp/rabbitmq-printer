# Line Printer Gateway Plugin for RabbitMQ
Maps [LPR](http://www.rfc-editor.org/rfc/rfc1179.txt) to AMQP and allows users
to print a message to the RabbitMQ broker.

This plugin is experimental. The described functionality is fully implemented
and partially tested. I seek feature requests and early adopters.

## Mapping between SMTP and AMQP

## Installation

### Setup LPR printer in Windows 7

### RabbitMQ Configuration
Add the plug-in configuration section. See
[RabbitMQ Configuration](https://www.rabbitmq.com/configure.html) for more details.

For example:
```erlang
{rabbitmq_printer, [
    {tcp_listeners, [515]}
    ]}
    ...
]}
```

To enable erlang to listen on ports < 1024 you may need to do

    $ setcap cap_net_bind_service+ep /usr/lib/erlang/erts-5.10.3/bin/beam


### Installation from source

Build and activate the RabbitMQ plug-in `rabbitmq-printer`. See the
[Plugin Development Guide](http://www.rabbitmq.com/plugin-development.html)
for more details.

    $ git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git
    $ cd rabbitmq-public-umbrella
    $ make co
    $ ./foreachrepo git checkout <tag>
    $ git clone https://github.com/gotthardp/rabbitmq-printer.git
    $ cd rabbitmq-printer
    $ make


## Copyright and Licensing

Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>

This package is subject to the Mozilla Public License Version 2.0 (the "License");
you may not use this file except in compliance with the License. You may obtain a
copy of the License at http://mozilla.org/MPL/2.0/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.
