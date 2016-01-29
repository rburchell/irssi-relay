#!/usr/bin/env perl
# script providing support for weechat's relay protocol in an irssi host
# written by Robin Burchell, originally based on work from irssi_proxy_websocket
# by Timothy J Fontaine

use v5.12;

use strict;
use warnings 'all';

use Irssi;
use Irssi::TextUI;

use JSON;

use Mojolicious::Lite;
use Mojo::Server::Daemon;

use File::Basename 'dirname';
use File::Spec;

my $have_compression = 0;
BEGIN {
	eval {
		require Compress::Zlib;
		import Compress::Zlib;
		$have_compression = 1;
	};
}

our $VERSION = '0.0.1';
our %IRSSI = (
  authors => 'Timothy J Fontaine',
  contact => 'tjfontaine@gmail.com',
  name    => 'irssi_proxy_websocket',
  license => 'MIT/X11',
  description => 'Proxy module that listens on a WebSocket',
);

Irssi::theme_register([
 'irssi_proxy_websocket',
 '{line_start}{hilight ' . $IRSSI{'name'} . ':} $0'
]);

Irssi::settings_add_str('irssi_proxy_websocket', 'ipw_host', 'localhost');
Irssi::settings_add_int('irssi_proxy_websocket', 'ipw_port', 9001);
Irssi::settings_add_bool('irssi_proxy_websocket', 'ipw_ssl', 0);
Irssi::settings_add_str('irssi_proxy_websocket', 'ipw_cert', '');
Irssi::settings_add_str('irssi_proxy_websocket', 'ipw_key', '');
Irssi::settings_add_str('irssi_proxy_websocket', 'ipw_password', '');
Irssi::settings_add_str('irssi_proxy_websocket', 'ipw_docroot', File::Spec->catdir(dirname(__FILE__), 'client'));

my $daemon;
my $loop_id;

sub mojoify {
    $ENV{MOJO_REUSE} = 1;

    # Mojo likes to spew, this makes irssi mostly unsuable
    app->log->level('fatal');

    app->static->paths->[0] = Irssi::settings_get_str('ipw_docroot');
    my $listen_url;

    my $host = Irssi::settings_get_str('ipw_host');
    my $port = Irssi::settings_get_int('ipw_port');
    my $cert = Irssi::settings_get_str('ipw_cert');
    my $key  = Irssi::settings_get_str('ipw_key');

    if(Irssi::settings_get_bool('ipw_ssl') && -e $cert && -e $key) {
        $listen_url = sprintf("https://%s:%d:%s:%s", $host, $port, $cert, $key);
    } else {
        $listen_url = sprintf("http://%s:%d", $host, $port);
    }

    logmsg("listen on $listen_url");
    $daemon = Mojo::Server::Daemon->new(app => app, listen => [$listen_url])->start;

    #TODO XXX FIXME we may be able to up this to 1000 or higher if abuse
    # mojo ->{handle} into the input_add system
    $loop_id = Irssi::timeout_add(100, \&ws_loop, 0);
}

mojoify();

sub setup_changed {
    my ($cert, $key);
    $cert = Irssi::settings_get_str('ipw_cert');
    $key  = Irssi::settings_get_str('ipw_key');

    if(length($cert) && !-e $cert) {
        logmsg("Certificate file doesn't exist: $cert");
    }
    if(length($key) && !-e $key) {
        logmsg("Key file doesn't exist: $key");
    }

    # TODO XXX FIXME
    # we should probably check that it was us that changed
    mojoify();
};

sub ws_loop {
    if($daemon) {
        my $id = Mojo::IOLoop->timer(0.0 => sub {});
        Mojo::IOLoop->one_tick;
        Mojo::IOLoop->remove($id);
    }
}

my $json = JSON->new->allow_nonref;
$json->allow_blessed(1);

my %clients = ();

sub logmsg {
    my $msg = shift;
    Irssi::printformat(MSGLEVEL_CLIENTCRAP, 'irssi_proxy_websocket', $msg);
}

websocket '/weechat' => sub {
    my $client = shift;
    logmsg("Client connected:" . $client->tx->remote_address);
    $clients{$client} = {
        client => $client,
        color => 0,
        authenticated => 0,
    };
    $client->on(message => \&process_message);
    $client->on(finish => sub {
            # delete first, otherwise we'll try tell the client about their disconnect,
            # which will be rather painful.
            delete $clients{$client};
            logmsg("Client disconnected: " . $client->tx->remote_address);
    });
};

get '/' => sub {
    my $client = shift;
    logmsg("Something made a HTTP request: " . $client->tx->remote_address);
    $client->render(text => "")
};

sub sendto_client {
    my ($client, $msg) = @_;
    if($clients{$client}->{'authenticated'}) {
        $client->send({binary => $msg});
    }
}

sub sendto_all_clients {
    my $msg = shift;

    while (my ($client, $chash) = each %clients) {
        sendto_client($chash->{'client'}, $msg);
    }
}

sub parse_init {
    my ($client, $id, $arguments) = @_;
    my @kvpairs = split(',', $arguments);
    foreach my $kvpair (@kvpairs) {
        my ($key, $value) = split('=', $kvpair);

        if ($key eq 'compression') {
            # ignore, we aren't going to support this
        } elsif ($key eq 'password') {
            my $chash = $clients{$client};

            # TODO
            #if ($value eq Irssi::settings_get_str('ipw_password')) {
            $chash->{'authenticated'} = 1;
            logmsg("Client has successfully authenticated");
            #}
        } else {
            logmsg("Client sent unknown init key: $key = $value")
        }
    }
}

package WeechatMessage {
    sub new {
        my $self = bless {};
        return $self->init(@_);
    }

    sub init {
        my $self = shift;
	#$self->{buf} = "\0"; # we don't support compression
        return $self;
    }

    sub add_int {
        my ($self, $int) = @_;
        $self->{buf} .= pack("i>", $int);
	return $self;
    }

    sub add_uint {
        my ($self, $uint) = @_;
        $self->{buf} .= pack("N", $uint);
	return $self;
    }

    sub add_string {
        my ($self, $string) = @_;
        $self->{buf} .= pack("i>/a", $string);
	return $self;
    }

    sub add_irssi_ptr {
	my ($self, $obj) = @_;
	my $ptr = $obj->{_irssi};
	add_string($self, sprintf("0x%016x", $ptr);
	return $self;
    }

    sub add_type {
        my ($self, $type) = @_;
        $self->{buf} .= $type;
	return $self;
    }

    sub add_info {
        my ($self, $name, $value) = @_;
        add_string($self, $name);
        add_string($self, $value);
	return $self;
    }

    sub concat {
	my ($self, $other) = @_;
	$self->{buf} .= $other;
	return $self;
    }

    sub get_buffer {
        my ($self) = @_;
	$retval = "\0" + $self->{buf};
        my $retbuf = pack("N", 4+length($retval)) . $retval;
        return $retbuf;
    }

    sub get_raw_buffer {
	    my ($self) = @_;
	    return $self->{buf};
    }
}

sub parse_info {
    my ($client, $id, $arguments) = @_;
    if ($arguments eq 'version') {
        my $obj = WeechatMessage::new();
        $obj->add_string($id);
        $obj->add_type("inf");
        $obj->add_info("version", "Irssi 1.0");
        sendto_client($client, $obj->get_buffer());
    } else {
        logmsg("Unknown INFO requested: $arguments");
    }
}


# Splits off one piece of an hpath. It takes off the first segment, skipping a leading /.
# Splits into object name, count, and then the rest of the path with leading /.
# Count is 0 if there is no count supplied. "Rest of path" is empty string (with not even a /) if this is the end of the path.
sub hpath_tok {
	my ($hpath)= @_;

	if ($hpath =~ m[^/?(?'obj'[^/]+)(?:\((?'ct'(?:[+-]\d+|\*))\))?(?'rest'/.*)?$]) {
		($obj, $count, $rest) = ($1, $2, $3);
		$count //= 0;
		$rest //= "";
	}
}

# Basic signature for an hdata handler:
# Input: ($type, $ptr, $subpath, @keys)
# $type: If 1 this is a key type query, no iteration, just dig down to the data type of whatever is returned and return the relevant strings, using
#        keyname:type notation in a comma-separated list.
#        If keys is empty, they want all keys.
#        You don't have to do the keys in the same order as what is asked for.
# $ptr: The current object under examination. It will be undefined at the root. At other points it will be some sort of Irssi object.
#       If $ptr is defined the function should prepend its value to the returned message.
# $subpath: The subpath below this point. If it has a leading /, it is asking for a subject. If not, this is a root list.
#           If it's empty, this is the last object and is where the keys should be taken from.
# @keys: The keys to request.
# Note: defined($ptr) if and only if (!$type && $subpath =~ m[^/])
# Output:
# If $type, return a type name and a name:type string describing the keys (or all keys).
# Otherwise,
# Return a string which represents the key data. If this is the toplevel it probably should be returning one string.
# A list will return multiple strings which is the result of deep calls.
my %hdata_classes = (
	buffer => sub {
		my ($type, $ptr, $subpath, @keys) = @_;
		if ($subpath =~ m[^/]) {
			my ($item, $count, $rest) = hpath_tok($subpath);
			my $cls;
			given ($item)
			{
				when ("plugin") {
				}
				when ("own_lines") {
				}
				when ("mixed_lines") {
				}
				when ("lines") {
					if ($type) {
						my ($cls, $val) = $hdata_classes{"lines"}->(1, undef, $rest, @keys);
						return "buffer/$cls", $val;
					}
					my (@os) = $hdata_classes{"lines"}->(0, $w, $rest, @keys);
					my @r;
					foreach (my $o in @os)
					{
						my $m = new WeechatMessage;
						$m->add_irssi_ptr($w);
						$m->concat($o);
						push @r, $m->get_raw_buffer();
					}
					return $r;
				}
				when ("nicklist_root") {
				}
				when ("input_undo_snap") {
				}
				when ("input_undo") {
				}
				when ("last_input_undo") {
				}
				when ("ptr_input_undo") {
				}
				when ("completion") {
				}
				when ("history") {
				}
				when ("last_history") {
				}
				when ("ptr_history") {
				}
				when ("keys") {
				}
				when ("last_key") {
				}
				when ("prev_buffer") {
				}
				when ("next_buffer") {
				}
			}
		}
		elsif ($subpath =~ m[^[^/]]) {
			my ($list, $count, $rest) = hpath_tok($subpath);
			if ($type) { return $hdata_classes{buffer}->(1, undef, $rest, @keys); # Don't prepend because we're just stripping the list
			given $list
			{
				when ("gui_buffer_last_displayed") {
				}
				when ("gui_buffers") {
					# A weechat "buffer" is what irssi calls a window, but weechat also merges in stuff from window item.
					# Solution: we return the window and the info of the active item (if there is one)
					# And we will have to push things like nicklist changes if they do /window item next
					my @results;
					foreach (my $w in Irssi::windows())
					{
						push @results, $hdata_classes{buffer}->(0, $w, $rest, @keys);
					}
					return @results;
				}
				when ("last_gui_buffer") {
				}
			}
		}
		else {
			$ptr//die "Bad hpath";
			if ($type) {
				my %key_types = (
					number => 'int',
					layout_number => 'int',
					layout_number_merge_order => 'int',
					name => 'str',
					full_name => 'str',
					short_name => 'str',
					type => 'int',
					notify => 'int',
					num_displayed => 'int',
					active => 'int',
					hidden => 'int',
					zoomed => 'int',
					print_hooks_enabled => 'int',
					day_change => 'int',
					clear => 'int',
					filter => 'int',
					closing => 'int',
					title => 'str',
					time_for_each_line => 'int',
					chat_refresh_needed => 'int',
					nicklist => 'int',
					nicklist_case_sensitive => 'int',
					nicklist_max_length => 'int',
					nicklist_display_groups => 'int',
					nicklist_count => 'int',
					nicklist_groups_count => 'int',
					nicklist_nicks_count => 'int',
					nicklist_visible_count => 'int',
					input => 'int',
					input_get_unknown_commands => 'int',
					input_buffer => 'str',
					input_buffer_alloc => 'int',
					input_buffer_size => 'int',
					input_buffer_length => 'int',
					input_buffer_pos => 'int',
					input_buffer_1st_display => 'int',
					input_undo_count => 'int',
					num_history => 'int',
					text_search => 'int',
					text_search_exact => 'int',
					text_search_regex => 'int',
					text_search_regex_compiled => 'int',
					text_search_where => 'int',
					text_search_found => 'int',
					text_search_input => 'str',
					highlight_words => 'str',
					highlight_regex => 'int',
					highlight_regex_compiled => 'int',
					highlight_tags_restrict => 'str',
					highlight_tags_restrict_count => 'int',
					highlight_tags_restrict_array => 'arr',
					highlight_tags => 'str',
					highlight_tags_count => 'int',
					highlight_tags_array => 'arr',
					hotlist_max_level_nicks => 'int',
					keys_count => 'int',
					local_variables => 'htb',
				);
				if (!@keys) { @keys = sort keys $key_types; }
				$t = join ",", map { $_ . ":" . $key_types{$_} } @keys;
				return "buffer", $t;
			}
			else {

			}
		}
	}
);

sub parse_hdata {
    my ($client, $id, $arguments) = @_;

    # $arguments = "hotlist:gui_hotlist(*)"
    # hdata_head here will be "hotlist"
    # everything after the : is split on '/' and put into list_path
    # list_path[0] is the important bit

    # OK Gory details of what an hdata path looks like:
    # The first token before the : is the "root class"
    #   For example, buffer is the buffer class which describes window contents, input, nicklist
    # Right after comes either a pointer giving a specific object or a list
    #   For example, the gui_buffers list is all buffers currently in view
    # Then, comes a /-delimited list of sub-members to drill down an object tree. For example,
    #   /lines/first_line(*)/data
    #   lines is the lines member of the buffer class, which is of type lines, then
    #   first_line is the first_line member of the lines class which is of type line, then
    #   data is the data member of the line class which is of type line_data
    # After that is a space and a list of properties.
    # In place of the root list, a pointer can be used to start from a particular object.
    # For a list, an integer in () implements list iteration.
    #   +N gives the next N items,
    #   -N gives the previous N items,
    #   * is basically "the rest of the list". The example shows ussing (*) on first_line but
    #   first_line isn't a list, so really it gets treated like "a list of one"

    my $count = () = $arguments =~ /(.+):([^ ]+)( (.+))?/;
    if ($count eq 0) {
        logmsg("Bad HDATA request: $arguments");
        return;
    }

    my $hdata_head = $1;
    my $hdata_tail = $2;
    my $keys = "";
    if (defined($4)) {
        $keys = $4
    }
    my @list_path = split('/', $hdata_tail);

    if ($list_path[0] =~ /\(?((0x[0-9A-Fa-f]+))\)?/) {
        logmsg("Got a HDATA for pointer $1 of $hdata_head for keys $keys");
        return;
    } else {
        logmsg("Got a HDATA for HEAD of $hdata_head for keys $keys");
        return;
    }

    # build return path..
    my $return_path = $hdata_head;
    for (my $i = 1; $i < length(scalar @list_path); $i++) {
        # TODO: skip ()s, verify it exists
        $return_path .= "/" . $list_path[$i];
    }

    my $hdata_types {

    }

 #   if ($keys eq "") {
 #       # TODO: get keys from hdata_head definition
 #       buffer:
 #       htb:local_variables
 #       notify:int
 #       number:int
 #       full_name:string
 #       short_name:string
 #       title:string
 #       hidden:int
 #       type:int
#    }

    my $klist = split(",", $keys);

    # build list of key types..
    my $key_types = "";
#    for (my $i = 0; $i < length($klist); $i++) {
#        $key_types .= $klist[$i] . ":" . "str"; # TODO: proper type
#    }

    my $obj = WeechatMessage::new();
    $obj->add_string($id);
    $obj->add_type("hda");
    $obj->add_string($return_path);
    $obj->add_string($key_types); # keys
    $obj->add_int(0); # count

    sendto_client($client, $obj->get_buffer());
}

sub process_message {
    my ($client, $message) = @_;

    my ($id, $command, $arguments) = ("", "", "");

    $message =~ s/\n$//;
    #logmsg("Processing: $message");

    # Commands have format: "(id) command arguments\n".
    if ($message =~ /^\(([^ ]+)\) ([^ ]+) (.+)$/) {
        $id = $1;
        $command = $2;
        $arguments = $3;
    } elsif ($message =~ /^\(([^ ]+)\) ([^ ]+)$/) {
        # also handle optional arguments :)
        $id = $1;
        $command = $2;
    } else {
        logmsg("Got a bad message: $message");
        return
    }

    if ($command eq 'init') {
        parse_init($client, $id, $arguments);
    } elsif ($command eq 'info') {
        parse_info($client, $id, $arguments);
    } elsif ($command eq 'hdata') {
        logmsg("HDATA: $message");
        parse_hdata($client, $id, $arguments);
    } else {
        logmsg("Unhandled: $message");
        logmsg("ID: $id COMMAND: $command ARGS: $arguments");
    }
}

my $wants_hilight_message = {};

sub gui_print_text_finished {
    my ($window) = @_;
    my $ref = $window->{'refnum'}; 
    my $color_line = $window->view->{buffer}->{cur_line}->get_text(1);
    my $plain_line = $window->view->{buffer}->{cur_line}->get_text(0);

    while (my ($client, $chash) = each %clients) {
        my $line = $plain_line;

        if($chash->{'color'}) {
            $line = $color_line;
        }

        if ($wants_hilight_message->{$ref}) {
            #sendto_client($chash->{'client'}, {
            #  event => 'hilight',
            #   window => $ref,
            #    line => $line,
            #  });
        }

        #sendto_client($chash->{'client'}, {
        #   event => 'addline',
        #    window => $ref,
        #     line => $line,
#    });
    }

    if ($wants_hilight_message->{$ref}) {
        delete $wants_hilight_message->{$ref};
    }
}

sub configure {
    my ($client, $event) = @_;
    my $chash = $clients{$client};

    for my $key (keys %{$event}) {
        if($key ne 'event') {
            $chash->{$key} = $event->{$key};
        }
    }
}

sub window_created {
    my $window = shift;

    #sendto_all_clients({
    #  event => 'addwindow',
    #  window => "$window->{'refnum'}",
    #  name => $window->{name},
    #});
}

sub window_destroyed {
    my $window = shift;

    # sendto_all_clients({
    #   event => 'delwindow',
    #   window => "$window->{'refnum'}",
    # });
}

sub window_activity {
    my ($window, $oldlevel) = @_;

    while (my ($client, $chash) = each %clients) {
        #sendto_client($chash->{'client'}, {
        #   event => 'activity',
        #    window => "$window->{'refnum'}",
        #     level => $window->{data_level},
#      oldlevel => $oldlevel,
#    });
    }
}

sub window_hilight {
    my $window = shift;
    $wants_hilight_message->{$window->{'refnum'}} = 1;
}

sub window_refnum_changed {
    my ($window, $oldnum) = @_;

    #sendto_all_clients({
    #  event => 'renumber',
    #  old => $oldnum,
    #  cur => $window->{'refnum'},
#  });
}

Irssi::signal_add("gui print text finished", "gui_print_text_finished");

Irssi::signal_add("window created", "window_created");
Irssi::signal_add("window destroyed", "window_destroyed");
Irssi::signal_add("window activity", "window_activity");
Irssi::signal_add_first("window hilight", "window_hilight");
Irssi::signal_add("window refnum changed", "window_refnum_changed");

Irssi::signal_add("setup changed", "setup_changed");

sub UNLOAD {
    Irssi::timeout_remove($loop_id);
    Irssi::signal_remove("gui print text finished", "gui_print_text_finished");
    Irssi::signal_remove("window created", "window_created");
    Irssi::signal_remove("window destroyed", "window_destroyed");
    Irssi::signal_remove("window activity", "window_activity");
    # TODO: is daemon cleared up properly? and finish this
    # Symbol::delete_package("WeechatMessage");
}


