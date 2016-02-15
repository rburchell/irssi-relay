#!/usr/bin/env perl
# script providing support for weechat's relay protocol in an irssi host
# written by Robin Burchell, originally based on work from irssi_proxy_websocket
# by Timothy J Fontaine

use v5.12;

use strict;
use warnings 'all';

no warnings 'portable';

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
    my $logdir = Irssi::get_irssi_dir() . "/log";
    unless (-e $logdir) { mkdir $logdir; }
    if (!-d $logdir) { warn "Log directory is not a directory"; app->log(Mojo::Log->new("/dev/null")); }
    else {
	    app->log(Mojo::Log->new(path => "$logdir/weechat-relay.log"));
    }

    app->log->level('debug');

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
#    Irssi::printformat(MSGLEVEL_CLIENTCRAP, 'irssi_proxy_websocket', $msg);
    app->log->info($msg);
}

my $logmsg = \&logmsg; # For WeechatMessage

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
	    desync_client($client);
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

    sub set_int {
	    my ($self, $pos, $int) = @_;
	    substr($self->{buf}, $pos, 4) = pack("i>", $int);
	    return $self;
    }

    sub add_uint {
        my ($self, $uint) = @_;
        $self->{buf} .= pack("N", $uint);
	return $self;
    }

    sub add_chr {
        my ($self, $chr) = @_;
	$self->{buf} .= pack("c", $chr);
	return $self;
    }

    sub add_string {
        my ($self, $string) = @_;
	if (defined($string)) {
	        $self->{buf} .= pack("i>/a", $string);
	} else {
		$self->add_int(-1); # sz 0xFFFFFFFF == NULL string.
	}
	return $self;
    }

    sub add_string_shortlength {
        my ($self, $string) = @_;
	$self->{buf} .= pack("c/a", $string);
	return $self;
    }

    sub add_ptr {
	my ($self, $ptr) = @_;
	#$self->{buf} .= pack("c/a", sprintf("%016x", $ptr));
	use integer;
	$self->add_string_shortlength(sprintf("%016x", $ptr));
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
	my $retval = "\0" . $self->{buf};
        my $retbuf = pack("N", 4+length($retval)) . $retval;
	local $Data::Dumper::Terse = 1;
	local $Data::Dumper::Useqq = 1;
	$logmsg->("Buffer contents are: " . Data::Dumper->Dump([$retbuf], ['retbuf']));
        return $retbuf;
    }

    sub get_raw_buffer {
	    my ($self) = @_;
	    return $self->{buf};
    }

    sub get_length {
	    my ($self) = @_;
	    return length $self->{buf};
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

	if ($hpath =~ m[^/?(?'obj'[^/]+?)(?:\((?'ct'(?:[+-]?\d+|\*))\))?(?'rest'/.*)?$]) {
		my ($obj, $count, $rest) = ($1, $2, $3);
		$count //= 0;
		$rest //= "";
		return $obj, $count, $rest;
	}
}

# Basic signature for an hdata handler:
# list_<n> : Subroutine called when requesting top-level list <n>, the counter value will be passed.
#   Return value should be the list of objects from the list.
# sublist_<n> : Subroutine called when requesting member list <n>, the object and counter value will be passed.
#   Return value should be the object or list of objects in that member.
# type_sublist_<n> : String indicating the class of the sublist, which must be some other class in the hdata_classes hash.
# key_<n> : Subroutine called when requesting key <n>. The object to retrieve the key from will be passed, as well as a WeechatMessage instance.
#   The subroutine should encode the object value into the WeechatMessage instance. It should not return anything.
# type_key_<n> : String which is the 3-letter code type of the key.
# from_pointer : Subroutine called when the top-level list item is a pointer address. The pointer (as an integer) is passed.
#   Return value should be the object in question. Return undef if no such object.
# get_pointer : Subroutine called to get a pointer value from an object.
my %line_ptr_cache; # The from_pointer algo for line is stupid because we have to literally do a mass search of all lines in all buffers.
	# this hash exists so that gui_print_line_finished can short-circuit the whole damn thing.
my %hdata_classes = (
	buffer => {
		list_gui_buffers => sub {
			my ($ct) = @_;
			# A weechat "buffer" is what irssi calls a window, but weechat also merges in stuff from window item.
			# Solution: we return the window and the info of the active item (if there is one)
			# And we will have to push things like nicklist changes if they do /window item next
			my @w = Irssi::windows();
			if ($ct eq '*' || $ct > $#w) { return @w; }
			elsif ($ct <= 0) { return $w[0]; }
			else { return $w[0 .. $ct]; }
		},
		sublist_lines => sub {
			my ($w, $ct) = @_; # Irssi::Window
			return $w->view()->{buffer};
		},
		type_sublist_lines => 'lines',
		get_pointer => sub {
			my ($w) = @_; # Irssi::Window
			return $w->{_irssi};
		},
		from_pointer => sub {
			my ($p) = @_;
			my @w = Irssi::windows();
			my ($w) = grep { $_->{_irssi} == $p } @w;
			return $w;
		},
		sublist_plugin => sub { },
		sublist_own_lines => sub { my ($w, $ct) = @_; return $w->view()->{buffer}; },
		type_sublist_own_lines => 'lines',
		sublist_mixed_lines => sub { my ($w, $ct) = @_; return $w->view()->{buffer}; },
		type_sublist_mixed_lines => 'lines',
		sublist_nicklist_root => sub { },
		sublist_input_undo_snap => sub { },
		sublist_input_undo => sub { },
		sublist_last_input_undo => sub { },
		sublist_ptr_input_undo => sub { },
		sublist_completion => sub { },
		sublist_history => sub { },
		sublist_last_history => sub { },
		sublist_ptr_history => sub { },
		sublist_keys => sub { },
		sublist_last_key => sub { },
		sublist_prev_buffer => sub { },
		sublist_next_buffer => sub { },
		list_gui_buffer_last_displayed => sub { },
		list_last_gui_buffer => sub { },
		type_key_number => 'int',
		key_number => sub { my ($w, $m) = @_; $m->add_int($w->{refnum}); },
		type_key_layout_number => 'int',
		key_layout_number => sub { my ($w, $m) = @_; $m->add_int($w->{refnum}); },
		type_key_layout_number_merge_order => 'int',
		type_key_name => 'str',
		key_name => sub { my ($w, $m) = @_; my ($wi) = $w->{active}; if(defined($wi)) { my $s = $wi->{server}; $m->add_string($s->{address} . "." . $wi->{name}); } else { $m->add_string($w->{name}); } },
		type_key_full_name => 'str',
		key_full_name => sub { my ($w, $m) = @_; my ($wi) = $w->{active}; if(defined($wi)) { my $s = $wi->{server}; $m->add_string('irc.' . $s->{address} . "." . $wi->{name}); } else { $m->add_string('irc.' . $w->{name}); } },
		type_key_short_name => 'str',
		key_short_name => sub { my ($w, $m) = @_; my ($wi) = $w->{active}; if(defined($wi)) { $m->add_string($wi->{name}); } else { $m->add_string($w->{name}); } },
		type_key_type => 'int',
		key_type => sub { my ($w, $m) = @_; my ($wi) = $w->{active}; $m->add_int(1); }, # GUI_BUFFER_TYPE_FREE
		type_key_notify => 'int',
		key_notify => sub {
			my ($w, $m) = @_;
			$m->add_int(3); # GUI_BUFFER_NOTIFY_ALL
			return;
=if0
			given ($w->{hilight_color}) {
				when(0) { $m->add_int(0); } # DATA_LEVEL_NONE => GUI_BUFFER_NOTIFY_NONE
				when(1) { $m->add_int(3); } # DATA_LEVEL_TEXT => GUI_BUFFER_NOTIFY_ALL
				when(2) { $m->add_int(2); } # DATA_LEVEL_MSG => GUI_BUFFER_NOTIFY_MESSAGE
				when(3) { $m->add_int(1); } # DATA_LEVEL_HILIGHT => GUI_BUFFER_NOTIFY_HIGHLIGHT
				default { $m->add_int(3); } # Send any other value as GUI_BUFFER_NOTIFY_ALL
			},
=cut
		},
		type_key_num_displayed => 'int',
		key_num_displayed => sub { my ($w, $m) = @_; $m->add_int(1); },
		type_key_active => 'int',
		key_active => sub { my ($w, $m) = @_; $m->add_int(2); }, # Only active (not merged)
		type_key_hidden => 'int',
		key_hidden => sub { my ($w, $m) = @_; $m->add_int(0); }, # not hidden
		type_key_zoomed => 'int',
		key_zoomed => sub { my ($w, $m) = @_; $m->add_int(0); }, # not zoomed
		type_key_print_hooks_enabled => 'int',
		key_print_hooks_enabled => sub { my ($w, $m) = @_; $m->add_int(0); }, # No hooks
		type_key_day_change => 'int',
		key_day_change => sub { my ($w, $m) = @_; $m->add_int(1); }, # Yes irssi prints "Day changed" lines
		type_key_clear => 'int',
		key_clear => sub { my ($w, $m) = @_; $m->add_int(1); }, # /clear allowed
		type_key_filter => 'int',
		key_filter => sub { my ($w, $m) = @_; $m->add_int(0); }, # no filters
		type_key_closing => 'int',
		key_closing => sub { my ($w, $m) = @_; $m->add_int(0); }, # not closing
		type_key_title => 'str',
		key_title => sub { my ($w, $m) = @_; my ($wi) = $w->items(); if (defined($wi)) { $m->add_string($wi->parse_special('$topic')); } else { $m->add_string(Irssi::parse_special('Irssi v$J - http://www.irssi.org')); } },
		type_key_time_for_each_line => 'int',
		key_time_for_each_lilne => sub { my ($w, $m) = @_; $m->add_int(Irssi::settings_get_bool("timestamps")); },
		type_key_chat_refresh_needed => 'int',
		key_chat_refresh_needed => sub { my ($w, $m) = @_; $m->add_int(0); }, # Not sure what to do here
		type_key_nicklist => 'int',
		key_nicklist => sub {
			my ($w, $m) = @_;
			my ($wi) = $w->items();
			if (defined($wi) && $wi->{type} eq "CHANNEL") { $m->add_int(1); }
			else { $m->add_int(0); }
		},
		type_key_nicklist_case_sensitive => 'int',
		key_nicklist_case_sensitive => sub { my ($w, $m) = @_; $m->add_int(0); },
		type_key_nicklist_max_length => 'int',
		key_nicklist_max_length => sub { my ($w, $m) = @_; $m->add_int(65535); }, # Theoretically unlimited, since irssi knows how malloc works
		type_key_nicklist_display_groups => 'int',
		key_nicklist_display_groups => sub { my ($w, $m) = @_; $m->add_int(0); },
		type_key_nicklist_count => 'int',
		key_nicklist_count => sub {
			my ($w, $m) = @_;
			my ($wi) = $w->items();
			if (defined($wi) && $wi->DOES("Irssi::Irc::Channel")) {
				$m->add_int(scalar(@{[$wi->nicks()]}));
			}
			else {
				$m->add_int(0);
			}
		},
		type_key_nicklist_groups_count => 'int',
		key_nicklist_groups_count => sub { my ($w, $m) = @_; $m->add_int(0); },
		type_key_nicklist_nicks_count => 'int',
		key_nicklist_nicks_count => sub {
			my ($w, $m) = @_;
			my ($wi) = $w->items();
			if (defined($wi) && $wi->DOES("Irssi::Irc::Channel")) {
				$m->add_int(scalar(@{[$wi->nicks()]}));
			}
			else {
				$m->add_int(0);
			}
		},
		type_key_nicklist_visible_count => 'int',
		key_nicklist_visible_count => sub {
			my ($w, $m) = @_;
			my ($wi) = $w->items();
			if (defined($wi) && $wi->DOES("Irssi::Irc::Channel")) {
				$m->add_int(scalar(@{[$wi->nicks()]}));
			}
			else {
				$m->add_int(0);
			}
		},
		type_key_input => 'int',
		key_input => sub { my ($w, $m) = @_; $m->add_int(1); },
		type_key_input_get_unknown_commands => 'int',
		key_input_get_unknown_commands => sub { my ($w, $m) = @_; $m->add_int(0); },
		type_key_input_buffer => 'str',
		key_input_buffer => sub { my ($w, $m) = @_; $m->add_string(Irssi::parse_special('$L')); },
		type_key_input_buffer_alloc => 'int',
		type_key_input_buffer_size => 'int',
		type_key_input_buffer_length => 'int',
		type_key_input_buffer_pos => 'int',
		type_key_input_buffer_1st_display => 'int',
		type_key_input_undo_count => 'int',
		type_key_num_history => 'int',
		type_key_text_search => 'int',
		type_key_text_search_exact => 'int',
		type_key_text_search_regex => 'int',
		type_key_text_search_regex_compiled => 'int',
		type_key_text_search_where => 'int',
		type_key_text_search_found => 'int',
		type_key_text_search_input => 'str',
		type_key_highlight_words => 'str',
		type_key_highlight_regex => 'int',
		type_key_highlight_regex_compiled => 'int',
		type_key_highlight_tags_restrict => 'str',
		type_key_highlight_tags_restrict_count => 'int',
		type_key_highlight_tags_restrict_array => 'arr',
		type_key_highlight_tags => 'str',
		type_key_highlight_tags_count => 'int',
		type_key_highlight_tags_array => 'arr',
		type_key_hotlist_max_level_nicks => 'int',
		type_key_keys_count => 'int',
		type_key_local_variables => 'htb',
		key_local_variables => sub {
			my ($w, $m) = @_;
			$m->add_type("str");
			$m->add_type("str");
			$m->add_int(0);
		},
	},
	# TODO TODO TODO What the heck is hotlist? For now returning empties.
	hotlist => {
		list_gui_hotlist => sub {
			my ($ct) = @_;
			return ();
		},
		type_key_priority => 'int',
		key_priority => sub {
			my ($o, $m) = @_;
			$m->add_int(0);
		},
		type_key_creation_time => 'tim',
		key_creation_time => sub {
			my ($o, $m) = @_;
			$m->add_string_shortlength("0");
		},
		type_key_buffer => 'ptr',
		key_buffer => sub {
			my ($o, $m) = @_;
			$m->add_ptr(0);
		},
		type_key_count => 'arr',
		key_count => sub {
			my ($o, $m) = @_;
			$m->add_type("int");
			$m->add_int(0);
		},
	},
	lines => {
		sublist_first_line => sub {
			my ($buf, $ct) = @_;
			my $l = $buf->{first_line};
			my @l = ([$buf, $l]);
			if ($ct eq '*')
			{
				while (defined($l = $l->next()))
				{
					push @l, ([$buf, $l]);
				}
			}
			elsif ($ct < 0)
			{
				while ($ct < 0)
				{
					++$ct;
					$l = $l->prev();
					$l//last;
					push @l, ([$buf, $l]);
				}
			}
			else
			{
				while ($ct > 0)
				{
					$l = $l->next();
					$l//last;
					--$ct;
					push @l, ([$buf, $l]);
				}
			}
			return @l;
		},
		get_pointer => sub {
			my ($v) = @_;
			return $v->{_irssi};
		},
		from_pointer => sub {
			my ($ptr) = @_;
			my @w = Irssi::windows();
			my @v = map { $_->view(); } @w;
			my ($v) = grep { $_->{_irssi} == $ptr } @v;
			return $v;
		},
		type_sublist_first_line => 'line',
		sublist_last_line => sub {
			my ($buf, $ct) = @_;
			my $l = $buf->{cur_line};
			my @l = ([$buf, $l]);
			if ($ct eq '*')
			{
				while (defined($l = $l->next()))
				{
					push @l, ([$buf, $l]);
				}
			}
			elsif ($ct < 0)
			{
				while ($ct < 0)
				{
					++$ct;
					$l = $l->prev();
					$l//last;
					push @l, ([$buf, $l]);
				}
			}
			else
			{
				while ($ct > 0)
				{
					$l = $l->next();
					$l//last;
					--$ct;
					push @l, ([$buf, $l]);
				}
			}
			return @l;
		},
		type_sublist_last_line => 'line',
		sublist_last_read_line => sub {
			my ($buf, $ct) = @_;
			my $l = $buf->{cur_line};
			my @l = ([$buf, $l]);
			if ($ct eq '*')
			{
				while (defined($l = $l->next()))
				{
					push @l, ([$buf, $l]);
				}
			}
			elsif ($ct < 0)
			{
				while ($ct < 0)
				{
					++$ct;
					$l = $l->prev();
					$l//last;
					push @l, ([$buf, $l]);
				}
			}
			else
			{
				while ($ct > 0)
				{
					$l = $l->next();
					$l//last;
					--$ct;
					push @l, ([$buf, $l]);
				}
			}
			return @l;
		},
		type_sublist_last_read_line => 'line',
		type_key_lines_count => 'int',
		key_lines_count => sub {
			my ($buf, $m) = @_;
			$m->add_int($buf->{lines_count});
			return;
		},
		type_key_first_line_not_read => 'int',
		key_first_line_not_read => sub {
			my ($buf, $m) = @_;
			$m->add_int(0);
		},
		type_key_lines_hidden => 'int',
		key_lines_hidden => sub {
			my ($buf, $m) = @_;
			$m->add_int(0);
		},
		type_key_buffer_max_length => 'int',
		key_buffer_max_length => sub {
			my ($buf, $m) = @_;
			$m->add_int(65536); # Arbitrary
		},
		type_key_buffer_max_length_refresh => 'int',
		type_key_prefix_max_length => 'int',
		key_prefix_max_length => sub {
			my ($buf, $m) = @_;
			$m->add_int(65536); # Arbitrary
		},
		type_key_prefix_max_length_refresh => 'int',
	},
	line => {
		sublist_data => sub {
			my ($bl, $ct) = @_;
			return $bl;
		},
		get_pointer => sub {
			my ($bl) = @_;
			my ($buf, $l) = @$bl;
			return $l->{_irssi};
		},
		from_pointer => sub {
			my ($ptr) = @_;
			if (exists($line_ptr_cache{$ptr})) { return $line_ptr_cache{$ptr}; }
			my @w = Irssi::windows();
			my @b = map { $_->view()->{buffer} } @w;
			for my $buf (@b) {
				for (my $l = $b->{first_line}; defined($l); $l = $l->next()) {
					if ($l->{_irssi} eq $ptr) {
						return [$buf, $l];
					}
				}
			}
			return undef;
		},
		type_sublist_data => 'line_data',
	},
	line_data => {
		get_pointer => sub {
			my ($bl, $ct) = @_;
			my ($buf, $l) = @$bl;
			return $l->{_irssi};
		},
		from_pointer => sub {
			my ($ptr) = @_;
			if (exists($line_ptr_cache{$ptr})) { return $line_ptr_cache{$ptr}; }
			my @w = Irssi::windows();
			my @b = map { $_->view()->{buffer} } @w;
			for my $buf (@b) {
				for (my $l = $b->{first_line}; defined($l); $l = $l->next()) {
					if ($l->{_irssi} eq $ptr) {
						return [$buf, $l];
					}
				}
			}
			return undef;
		},
		type_key_buffer => 'ptr',
		key_buffer => sub {
			my ($bl, $m) = @_;
			my ($buf, $l) = @$bl;
			my @w = Irssi::windows();
			my ($w) = grep { $_->view()->{buffer}->{_irssi} == $buf->{_irssi} } @w;
			$m->add_ptr($w->{_irssi});
		},
		#type_key_y => 'int',
		#key_y => sub { my ($l, $m) = @_; $m->add_int(uhIdunno); },
		type_key_date => 'tim',
		key_date => sub {
			my ($bl, $m) = @_;
			my ($buf, $l) = @$bl;
			$m->add_string_shortlength($l->{info}->{time});
		},
		type_key_date_printed => 'tim',
		key_date_printed => sub {
			my ($bl, $m) = @_;
			my ($buf, $l) = @$bl;
			$m->add_string_shortlength($l->{info}->{time});
		},
		#type_key_str_time => 'str',
		#type_key_tags_count => 'int',
		#type_key_tags => 'arr',
		type_key_displayed => 'chr',
		key_displayed => sub { my ($bl, $m) = @_; $m->add_chr(1); },
		type_key_highlight => 'chr',
		key_highlight => sub { my ($bl, $m) = @_; $m->add_chr(0); },
		type_key_refresh_needed => 'chr',
		key_refresh_needed => sub { my ($bl, $m) = @_; $m->add_chr(0); },
		type_key_prefix => 'str',
		key_prefix => sub { my ($bl, $m) = @_; $m->add_string(''); },
		type_key_prefix_length => 'int',
		key_prefix_length => sub { my ($bl, $m) = @_; $m->add_int(0); },
		type_key_message => 'str',
		key_message => sub {
			my ($bl, $m) = @_;
			my ($buf, $l) = @$bl;
		       	$m->add_string($l->get_text(0));
		},
	},
);

sub parse_hdata {
	use integer;
	# IMPORTANT: $client CAN BE UNDEFINED - THIS IS DONE IN THE EVENT HANDLERS
	# WHEN THIS HAPPENS WE *RETURN* THE BUILT RESPONSE MESSAGE INSTEAD OF SENDING IT.
	my ($client, $id, $arguments) = @_;

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

	my $hclass = $1;
	my $path = $2;
	my @keys = grep /./, split /,/, ($4//"");

	my $cls = $hdata_classes{$hclass};

	$cls//do{
		    logmsg("Unknown HDATA class: $hclass");
		    return;
	};

	my ($objstr, $ct);
	($objstr, $ct, $path) = hpath_tok($path);

	my @objs;

	if ($objstr =~ m/^0x/)
	{
		logmsg("Got a HDATA for $path from pointer $objstr of $hclass with keys @keys");
		# Pointer value
		no warnings 'portable'; # I MEAN IT
		my $objptr = hex($objstr);
		exists $cls->{from_pointer} or do {
			logmsg("Class $hclass can't retrieve objects from pointers.");
			return;
		};
		my $obj = ($cls->{from_pointer}->($objptr));
		$obj//do{
			logmsg("Object reference $objstr was not recognized by $hclass.");
			return;
		};
		my $ptr = sprintf("%016x", ($cls->{get_pointer}->($obj)));
		@objs = ($ptr => $obj);
	}
	else
	{
		exists $cls->{"list_$objstr"} or do {
			logmsg("List $objstr not defined for $hclass");
			return;
		};
		#logmsg("Got a HDATA for $ct $path from list $objstr from $hclass with keys @keys");
		my @obj = ($cls->{"list_$objstr"}->($ct));
		# We can technically legitimately return no objects on some lists.
		#unless (@obj) {
		#		logmsg("No objects returned for list $objstr from $hclass");
		#		return;
		#}
		for my $obj (@obj) {
			my $ptr = sprintf("%016x", ($cls->{get_pointer}->($obj)));
			push @objs, ($ptr => $obj);
		}
	}

	while ($path ne '')
	{
		my @results;
		($objstr, $ct, $path) = hpath_tok($path);
		my $s = $cls->{"sublist_$objstr"};
		my $st = $cls->{"type_sublist_$objstr"};
		$s//do {
			logmsg("No sublist $objstr in $hclass");
			return;
		};
		$st//do {
			logmsg("Don't know type of items in sublist $objstr in $hclass");
			return;
		};
		$hclass .= "/" . $st;
		my $newcls = $hdata_classes{$st};
		$newcls//do {
			logmsg("Don't recognize type $st of items in sublist $objstr in $hclass");
			return;
		};
		for (my $oix = 0; $oix < scalar(@objs); $oix += 2)
		{
			my $ptr = $objs[$oix];
			my $obj = $objs[$oix + 1];
			my @r = $s->($obj, $ct);
			for my $r (@r)
			{
				my $newp = $ptr . "/" . sprintf("%016x", ($newcls->{get_pointer}->($r)));
				#logmsg($newp);
				push @results, ($newp => $r);
			}
		}
		$cls = $newcls;
		@objs = @results;
	}

	my @keytypes;

	if (@keys < 1)
	{
		#logmsg("Getting all keys from $hclass");
		@keys = map { /^key_(.*)$/ && $1 } grep { /^key_/ && exists $cls->{"type_$_"} } keys %$cls;
		#logmsg("Keys: @keys");
	}
	else
	{
		@keys = grep { exists $cls->{"key_$_"} && exists $cls->{"type_key_$_"} } @keys;
		#logmsg("Actual defined keys: @keys");
	}

	@keytypes = map { $_ . ":" . $cls->{"type_key_$_"} } @keys;

	my $m = new WeechatMessage;

	$m->add_string($id);
	$m->add_type("hda");
	$m->add_string($hclass);
	$m->add_string(join ",", @keytypes);
	$m->add_int((scalar @objs)/2);

	for (my $oix = 0; $oix < scalar(@objs); $oix += 2)
	#for my $ptr (keys %objs)
	{
		my $ptr = $objs[$oix];
		# Add the p-path
		my @ppath = split /\//, $ptr;
		my $obj = $objs[$oix + 1];
		for my $pptr (@ppath)
		{
			no warnings 'portable'; # I MEAN IT.
			$m->add_ptr(hex($pptr));
		}
		for my $k (@keys)
		{
			$cls->{"key_$k"}->($obj, $m);
		}
	}

	if (defined($client))
	{
		sendto_client($client, $m->get_buffer());
		return;
	}
	else
	{
		return $m;
	}
}

sub get_window_from_weechat_name
{
	my ($name) = @_;
	#key_full_name => sub { my ($w, $m) = @_; my ($wi) = $w->items(); if(defined($wi)) { my $s = $wi->{server}; $m->add_string('irc.' . $s->{address} . "." . $wi->{name}); } else { $m->add_string('irc.' . $w->{name}); } },
	for my $w (Irssi::windows())
	{
		my @wi = $w->items();
		for my $wi (@wi)
		{
			my $s = $wi->{server};
			if ($name eq 'irc.' . $s->{address} . "." . $wi->{name}) {
				return $w;
			}
		}
		if ($name eq 'irc.' . $w->{name})
		{
			return $w;
		}
	}
	return undef;
}

sub parse_input
{
	my ($client, $id, $arguments) = @_;
	my ($target, $input);
	if ($arguments =~ m/^([^ ]+) (.*)$/)
	{
		($target, $input) = ($1, $2);
	}
	else
	{
		logmsg("Bad INPUT message");
	}
	my $buf;
	if ($target =~ m/^0x/)
	{
		use integer;
		no warnings 'portable'; # I MEAN IT
		my $ptr = hex($target);
		$buf = $hdata_classes{buffer}->{from_pointer}->($ptr);
	}
	else
	{
		$buf = get_window_from_weechat_name($target);
	}
	$buf//return;
	#$buf->command($input);
	my $s = $buf->{active_server};
	my $wi = $buf->{active};
	Irssi::signal_emit("send command", $input, $s, $wi);
}

my %subscribers;

sub parse_sync {
	my ($client, $id, $arguments) = @_;
	my ($buffer, %events);
	if ($arguments =~ m/^\s*$/)
	{
		$buffer = '*';
		%events = (buffers => 1, upgrade => 1, buffer => 1, nicklist => 1);
	}
	elsif ($arguments =~ m/^([^ ]+)$/)
	{
		$buffer = $1;
		if ($buffer eq '*')
		{
			%events = (buffers => 1, upgrade => 1, buffer => 1, nicklist => 1);
		}
		else
		{
			%events = (buffer => 1, nicklist => 1);
		}
	}
	elsif ($arguments =~ m/^([^ ]+) ([^ ]+)/)
	{
		$buffer = $1;
		%events = map { $_ => 1 } split /,/, $2;
	}
	if ($buffer ne "*")
	{
		my $w = get_window_from_weechat_name($buffer);
		$w//return;
		$buffer = $w->{_irssi};
	}

	for my $evt (keys %events)
	{
		if ($events{$evt})
		{
			logmsg("Subscribing $client to $evt from $buffer");
			$subscribers{$evt}->{$buffer}->{$client} = $client;
		}
	}
}

sub parse_desync {
	my ($client, $id, $arguments) = @_;
	my ($buffer, %events);
	if ($arguments =~ m/^\s*$/)
	{
		$buffer = '*';
		%events = (buffers => 1, upgrade => 1, buffer => 1, nicklist => 1);
	}
	elsif ($arguments =~ m/^([^ ]+)$/)
	{
		$buffer = $1;
		if ($buffer eq '*')
		{
			%events = (buffers => 1, upgrade => 1, buffer => 1, nicklist => 1);
		}
		else
		{
			%events = (buffer => 1, nicklist => 1);
		}
	}
	elsif ($arguments =~ m/^([^ ]+) (.*)$/)
	{
		$buffer = $1;
		%events = map { $_ => 1 } split /,/, $2;
	}
	if ($buffer ne "*")
	{
		my $w = get_window_from_weechat_name($buffer);
		$w//return;
		$buffer = $w->{_irssi};
	}

	for my $evt (keys %events)
	{
		if ($events{$evt})
		{
			logmsg("Unsubscribing $client from $evt from $buffer");
			delete $subscribers{$evt}->{$buffer}->{$client};
		}
	}
}

sub dispatch_event_message
{
	my ($event, $buffer, $data) = @_;
	$buffer//="*";

	my %clients = ();

	if (exists $subscribers{$event})
	{
		if (exists $subscribers{$event}->{"*"})
		{
			%clients = %{$subscribers{$event}->{"*"}};
		}
		if ($buffer ne '*' && exists $subscribers{$event}->{$buffer})
		{
			for my $k (keys %{$subscribers{$event}->{$buffer}}) {
				$clients{$k} = $subscribers{$event}->{$buffer}->{$k};
			}
		}
	}
	
	logmsg("Dispatching to " . scalar(keys %clients) . " subscribers");

	for my $k (keys %clients)
	{
		my $cli = $clients{$k};
		sendto_client($cli, $data);
	}
}

sub desync_client {
	my ($client) = @_;
	for my $event (keys %subscribers)
	{
		for my $buffer (keys %{$subscribers{$event}})
		{
			delete $subscribers{$event}->{$buffer}->{$client};
		}
	}
}

sub parse_nicklist {
	use integer;
	my ($client, $id, $arguments) = @_;
	my @buf;
	my $bufarg;
	logmsg("Got NICKLIST ($id) for $arguments");
	if (ref($arguments) && $arguments->DOES("Irssi::Window"))
	{
		@buf = [$arguments, $arguments->{active}//return];
	}
	elsif (($bufarg) = ($arguments =~ m/^([^ ]+)/)) {
		if ($bufarg =~ m/^0x/) {
			use integer;
			no warnings 'portable'; # I MEAN IT
			my $w = $hdata_classes{buffer}->{from_pointer}->(hex($bufarg));
			@buf = [$w, $w->{active}//return];
		} else {
			use integer;
			my $w = get_window_from_weechat_name($bufarg);
			@buf = [$w, $w->{active}//return];
		}
	}
	else
	{
		@buf = grep { defined($_->[1]) } map { [$_, $_->{active}] } Irssi::windows();
	}
	my $m = WeechatMessage->new();
	$m->add_string($id);
	$m->add_type('hda');
	$m->add_string("buffer/nicklist_item");
	$m->add_string("group:chr,visible:chr,level:int,name:str,color:str,prefix:str,prefix_color:str");
	my $ctpos = $m->get_length();
	my $objct = 0;
	$m->add_int(0);
	for my $buf (@buf)
	{
		my ($w, $wi) = @$buf;
		$wi->DOES("Irssi::Irc::Channel") or next;
		my %nicks = map { $_->{nick} => $_ } $wi->nicks();
		my $pfxraw = $wi->{server}->isupport("PREFIX")//"(ov)@+";
		my (@pfx) = ($pfxraw =~ m/^\(([[:alpha:]]+)\)(.+)$/);
		length $pfx[0] == length $pfx[1] or logmsg("Imbalanced PREFIX alert!: $pfxraw");
		my $grpct = 1;
		# Add nicklist root
		++$objct;
		$m->add_ptr($w->{_irssi})->add_ptr($wi->{_irssi}); # path
		$m->add_chr(1); # group
		$m->add_chr(0); # visible
		$m->add_int(0); # level (0 for root and all nicks, 1 for all other groups)
		$m->add_string("root"); # name
		$m->add_string(undef); # color
		$m->add_string(undef); # prefix
		$m->add_string(undef); # prefix_color
		for (my $pfxidx = 0; $pfxidx < length $pfx[0]; ++$pfxidx) {
			my ($pltr, $psym) = map { substr $_, $pfxidx, 1 } @pfx;
			++$objct;
			$m->add_ptr($w->{_irssi})->add_ptr($wi->{_irssi} + $grpct);
			$m->add_chr(1); # group
			$m->add_chr(1); # visible
			$m->add_int(1); # level
			$m->add_string(sprintf("%03d|%s", $grpct, $pltr)); # name
			$m->add_string("weechat.color.nicklist_group"); # color
			$m->add_string(undef); # prefix
			$m->add_string(undef); # prefix_color
			$grpct++;
			my @pfxd = sort { $a cmp $b } keys %nicks;
			for my $n (@pfxd)
			{
				my $nick = $nicks{$n};
				$nick->{prefixes} =~ m/\Q$psym\E/ or next;
				delete $nicks{$n};
				++$objct;
				$m->add_ptr($w->{_irssi})->add_ptr($nick->{_irssi});
				$m->add_chr(0); # group
				$m->add_chr(1); # visible
				$m->add_int(0); # level
				$m->add_string($nick->{nick}); # name
				$m->add_string(undef); # color
				$m->add_string($psym); # prefix
				$m->add_string(''); # prefix_color
			}
		}
		my @nopfx = sort { $a->{nick} cmp $b->{nick} } map { $nicks{$_} } keys %nicks;
		++$objct;
		$m->add_ptr($w->{_irssi})->add_ptr($wi->{_irssi} + $grpct);
		$m->add_chr(1); # group
		$m->add_chr(1); # visible
		$m->add_int(1); # level
		$m->add_string("999|..."); # name
		$m->add_string("weechat.color.nicklist_group"); # color
		$m->add_string(undef); # prefix
		$m->add_string(undef); # prefix_color
		for my $nick (@nopfx)
		{
			++$objct;
			$m->add_ptr($w->{_irssi})->add_ptr($nick->{_irssi});
			$m->add_chr(0); # group
			$m->add_chr(1); # visible
			$m->add_int(0); # level
			$m->add_string($nick->{nick}); # name
			$m->add_string(undef); # color
			$m->add_string(''); # prefix
			$m->add_string(''); # prefix_color
		}
	}
	$m->set_int($ctpos, $objct);
	if (defined($client)) {
		sendto_client($client, $m->get_buffer());
	} else {
		return $m;
	}
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
		}
		elsif ($command eq 'info') {
			parse_info($client, $id, $arguments);
		}
		elsif ($command eq 'hdata') {
			logmsg("HDATA: $message");
			parse_hdata($client, $id, $arguments);
		}
		elsif ($command eq 'input') {
			parse_input($client, $id, $arguments);
		}
		elsif ($command eq 'sync') {
			parse_sync($client, $id, $arguments);
		}
		elsif ($command eq 'desync') {
			parse_desync($client, $id, $arguments);
		}
		elsif ($command eq 'quit') {
			desync_client($client);
			$client->finish();
			delete $clients{$client};
		}
		elsif ($command eq 'test') {
			my $m = WeechatMessage->new();
			$m->add_string($id);
			$m->add_type('chr')->add_chr(65);
			$m->add_type('int')->add_int(123456);
			$m->add_type('int')->add_int(-123456);
			$m->add_type('lon')->add_string_shortlength("1234567890");
			$m->add_type('lon')->add_string_shortlength("1234567890");
			$m->add_type('str')->add_string("a string");
			$m->add_type('str')->add_string("");
			$m->add_type('str')->add_string(undef);
			$m->add_type('buf')->add_string("buffer");
			$m->add_type('buf')->add_string(undef);
			$m->add_type('ptr')->add_string_shortlength("0x1234abcd");
			$m->add_type('ptr')->add_string_shortlength("0x0");
			$m->add_type('tim')->add_string_shortlength("1321993456");
			$m->add_type('arr')->add_type('str')->add_int(2)->add_string("abc")->add_string("def");
			$m->add_type('arr')->add_type('int')->add_int(3)->add_int(123)->add_int(456)->add_int(789);
			sendto_client($client, $m->get_buffer());
		}
		elsif ($command eq 'ping') {
			my $m = WeechatMessage->new();
			$m->add_string("_pong");
			$m->add_type('str');
			$m->add_string($arguments);
			sendto_client($client, $m->get_buffer());
		}
		elsif ($command eq 'nicklist') {
			parse_nicklist($client, $id, $arguments);
		}
		else {
			logmsg("Unhandled: $message");
			logmsg("ID: $id COMMAND: $command ARGS: $arguments");
		}
}

my $wants_hilight_message = {};

sub gui_print_text_finished {
    my ($window) = @_;
    my $ref = $window->{'refnum'}; 
    my $buf = $window->view()->{buffer};
    my $line = $buf->{cur_line};

    my $ptr = sprintf("%016x", $line->{_irssi});

    # local not my since we're doing it to a hash element
    local $line_ptr_cache{$line->{_irssi}} = [$buf, $line];

    my $m = parse_hdata(undef, "_buffer_line_added", "line_data:0x$ptr");
    dispatch_event_message("buffer" => $window->{_irssi}, $m->get_buffer());
}

=pod
sub configure {
    my ($client, $event) = @_;
    my $chash = $clients{$client};

    for my $key (keys %{$event}) {
        if($key ne 'event') {
            $chash->{$key} = $event->{$key};
        }
    }
}
=cut

sub window_created {
    my $window = shift;
    use integer;

    my $ptr = sprintf("%016x", $window->{_irssi});

    my $m = parse_hdata(undef, "_buffer_opened", "buffer:0x$ptr");

    dispatch_event_message(buffers => "*", $m->get_buffer());

    #sendto_all_clients({
    #  event => 'addwindow',
    #  window => "$window->{'refnum'}",
    #  name => $window->{name},
    #});
}

sub window_destroyed {
    my $window = shift;
    use integer;

    my $ptr = sprintf("%016x", $window->{_irssi});
    my $m = parse_hdata(undef, "_buffer_closing", "buffer:0x$ptr");

    dispatch_event_message(buffers => "*", $m->get_buffer());

    for my $evt (keys %subscribers)
    {
	    delete $subscribers{$evt}->{$window->{_irssi}}; # Auto-remove all window subscriptions
    }

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

    my $ptr = sprintf("%016x", $window->{_irssi});
    my $m = parse_hdata(undef, "_buffer_moved", "buffer:0x$ptr");
    dispatch_event_message(buffers => "*", $m->get_buffer());

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
    $daemon->stop();
    # Symbol::delete_package("WeechatMessage");
}


