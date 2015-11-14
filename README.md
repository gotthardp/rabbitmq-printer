# Line Printer Gateway Plugin for RabbitMQ
Allows users to print a message to the RabbitMQ broker. In other words,
maps LPR (per [RFC 1179](http://www.rfc-editor.org/rfc/rfc1179.txt)) to AMQP.

This plugin is experimental. The described functionality is fully implemented
and partially tested. I seek feature requests and early adopters.

## Mapping between LPR and AMQP

The plugin acts as a LPR daemon (LPD) and (by default) listens on the port 515. To listen on another port modify the `tcp_listeners` configuration entry.

The plugin introduces a new exchange type 'x-printer'. Each print queue maps to a particular 'x-printer' exchange as explained in the table below. The exchange receives incoming prints and routes them to user queues.

Print Queue | Behaviour
:---        | :---
`X`         | prints to the exchange X in the default vhost
`V/X`       | prints to the exchange X in the vhost V
`V/X/K`     | prints to the exchange X in the vhost V using routing key K
`/X/K`      | prints to the exchange X in the default vhost using routing key K

For security reasons it is not possible to print to exchanges other than those of the type 'x-printer'.


## Installation

### Setup LPR printer in Windows 7

First, enable LPR printing in Windows
 - Go to Control Panel/Programs and Features
 - Click "Turn Windows features on or off", then expand "Print and Document Services"
 - Check the LPR Port Monitor box, then click OK

![alt tag](https://raw.github.com/gotthardp/rabbitmq-printer/master/doc/install-win-printer1.png)

Then, install a LPR printer
 - Choose "Devices and Printers" from the Control Panel
 - Click the "Add a printer" button in the menu bar at the top
 - Click the Add a local printer
 - Choose "Create a new port", then select "LPR Port", then click Next
 - Enter the printer name and the print queue name, then click "OK"
 - Select the "Generic" manufacturer and the "Generic / Text Only" printer from the list, then click "Next"

![alt tag](https://raw.github.com/gotthardp/rabbitmq-printer/master/doc/install-win-printer2.png)


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
