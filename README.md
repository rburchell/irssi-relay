# irssi-relay

irssi-relay is a simple script to provide a representation of irssi's data
structures using the [weechat relay
protocol](https://www.weechat.org/files/doc/devel/weechat_relay_protocol.en.html).

simply put, it allows you to use the same irc connection you know and love from
other user interfaces, like [glowing-bear](https://github.com/glowing-bear/glowing-bear).

# status

things can connect, exchange data, and hopefully not blow up your irssi, but we
provide no guarentees. if you aren't willing to get your hands dirty, use is not
recommended at this point.

# use

* put the script in your .irssi/scripts/autorun directory.
* configure it (see the ipw_ settings, most interesting ones are ipw_port,
  ipw_host and ipw_password)
* load it (using `/script load autorun/weechat-relay.pl`)
* connect your client

# authors

irssi-relay is the culmination of a tireless amount of effort from (but not
limited to) the following people:

* Thomas Stagner (large amounts of implementation, reverse-engineering)
* Robin Burchell (initial ideas & implementation, reverse-engineering)
* Timothy J Fontaine (irssi_proxy_websocket, the basis for this work)

