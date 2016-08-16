# Weechat Relay

Weechat Relay is a simple script to provide a representation of irssi's data
structures using the [weechat relay
protocol](https://www.weechat.org/files/doc/devel/weechat_relay_protocol.en.html).

Simply put, it allows you to use the same IRC connection you know and love from
other user interfaces, like [glowing-bear](https://github.com/glowing-bear/glowing-bear).

# Contact

Get in touch with us, on IRC of course. ;) We hang out on ##irssi-relay on freenode.

# Patches

Got an itch to scratch? That's great. Feel free to submit a pull request! :)

# Use

* Put the script in your .irssi/scripts/autorun directory.
* Configure it (see the wcrelay_ settings, most interesting ones are wcrelay_port,
  wcrelay_host and wcrelay_password)
* Load it (using `/script load autorun/weechat_relay.pl`)
* Connect your client

# Authors

Weechat Relay is the culmination of a tireless amount of effort from (but not
limited to) the following people:

* Thomas Stagner (large amounts of implementation, reverse-engineering)
* Robin Burchell (initial ideas & implementation, reverse-engineering)
* Timothy J Fontaine (irssi_proxy_websocket, the basis for this work)

