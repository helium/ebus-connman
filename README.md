[![Travis][travis badge]][travis]
[![Hex.pm Version][hex version badge]][hex]
[![Hex.pm License][hex license badge]][hex]
[![Erlang Versions][erlang version badge]][erlang]
[![Build Tool][build tool]][rebar]

# ebus-connman


The `connman` applicaiton provides an Erlang binding for the [connman]
connection manager.


  * [Features](#features)
  * [Install](#install)
  * [Examples](#examples)
  * [Building](#building)


## Features

  * Get current available services
  * Connect to an SSID by name instead of Service ID
  * Notifications on connectivity state changes.

## Install

Add `ebus-connman` to your `deps` section in `rebar.config`:

``` shell
{deps, [connman]}.
```

To ensure that connman is tarted when your application starts also add
it to the `applications` section in your application's `.app.src`
file. For example:


``` erlang
{application, my_app,
 [{description, "An application using connman"},
  {vsn, "git"},
  {registered, []},
  {applications,
   [kernel,
    stdlib,
    connman
   ]},
  {env,[]}
 ]}.

```

## Examples

### Get Services

Here's an example of talking to [connman] to get the current list of
connectivity related technologies:

```erlang
Eshell V10.1  (abort with ^G)E
1>  {ok, C} = connman:connman()
{ok,<0.221.0>}
2>  connman:services(C).
{ok,[{"/net/connman/service/wifi_a0c589a1baee_425452205570706572204150_managed_psk",
      #{"AutoConnect" => true,
        "Domains" => ["Home"],
        "Domains.Configuration" => [],
        "Ethernet" =>
            #{"Address" => "A0:C5:89:A1:BA:EE","Interface" => "wlp2s0",
              "MTU" => 1500,"Method" => "auto"},
        "Favorite" => true,
        "IPv4" =>
            #{"Address" => "192.168.2.60","Gateway" => "192.168.2.1",
              "Method" => "dhcp","Netmask" => "255.255.255.0"},
        "IPv4.Configuration" => #{"Method" => "dhcp"},
        "IPv6" => #{},
        "IPv6.Configuration" =>
            #{"Method" => "auto","Privacy" => "disabled"},
        "Immutable" => false,"Name" => "BTR Upper AP",
        "Nameservers" => ["192.168.2.1"],
        "Nameservers.Configuration" => [],"Provider" => #{},
        "Proxy" => #{"Method" => "direct"},
        "Proxy.Configuration" => #{},
        "Security" => ["psk"],
        "State" => "online","Strength" => 58,
        "Timeservers" => ["192.168.2.1"],
        "Timeservers.Configuration" => [],"Type" => "wifi",
        "mDNS" => false,"mDNS.Configuration" => false}}]}
```

### State Change Notifications

To get notified when connectivity of the system changes, call
`register_state_notify`. As an example:

```erlang
Eshell V10.1  (abort with ^G)E
1> {ok, C} = connman:connman()
{ok,<0.221.0>}
2> connman:register_state_notify(self()).
ok
```

Now when the state of the network changes, the given handler pid will
receive a message:

``` erlang
10> flush().
{state_changed, online}
```

The values for state are the atom versions of the connman connectivity
names: `offline`, `idle`, `ready`, `online`. Refer to the connman
documentation for the latestdocumentation on the meaning of the state
names.


## Building

Fork the repo and simply use `make` to build the library.


<!-- Badges -->
[travis]: https://travis-ci.org/helium/ebus-connman
[travis badge]: https://img.shields.io/travis/helium/ebus-connman/master.svg?style=flat-square
[hex]: https://hex.pm/packages/connman
[hex version badge]: https://img.shields.io/hexpm/v/connman.svg?style=flat-square
[hex license badge]: https://img.shields.io/hexpm/l/connman.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-21.1-blue.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg?style=flat-square

<!-- Links -->
[connman]: https://01.org/connman
[rebar]: http://rebar3.org
[erlang]: http://erlang.org
