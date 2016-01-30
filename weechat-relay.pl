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

    sub add_chr {
        my ($self, $chr) = @_;
	$self->{buf} .= pack("c", $chr);
    }

    sub add_string {
        my ($self, $string) = @_;
        $self->{buf} .= pack("i>/a", $string);
	return $self;
    }

    sub add_ptr {
	my ($self, $ptr) = @_;
	$self->{buf} .= pack("c/a", sprintf("%016x", $ptr));
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
			return $w->view();
		},
		get_pointer => sub {
			my ($w) = @_; # Irssi::Window
			return $w->{_irssi};
		},
		type_sublist_lines => lines,
		sublist_plugin => sub { },
		sublist_own_lines => sub { },
		sublist_mixed_lines => sub { },
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
		type_key_layout_number => 'int',
		type_key_layout_number_merge_order => 'int',
		type_key_name => 'str',
		type_key_full_name => 'str',
		type_key_short_name => 'str',
		type_key_type => 'int',
		type_key_notify => 'int',
		type_key_num_displayed => 'int',
		type_key_active => 'int',
		type_key_hidden => 'int',
		type_key_zoomed => 'int',
		type_key_print_hooks_enabled => 'int',
		type_key_day_change => 'int',
		type_key_clear => 'int',
		type_key_filter => 'int',
		type_key_closing => 'int',
		type_key_title => 'str',
		type_key_time_for_each_line => 'int',
		type_key_chat_refresh_needed => 'int',
		type_key_nicklist => 'int',
		type_key_nicklist_case_sensitive => 'int',
		type_key_nicklist_max_length => 'int',
		type_key_nicklist_display_groups => 'int',
		type_key_nicklist_count => 'int',
		type_key_nicklist_groups_count => 'int',
		type_key_nicklist_nicks_count => 'int',
		type_key_nicklist_visible_count => 'int',
		type_key_input => 'int',
		type_key_input_get_unknown_commands => 'int',
		type_key_input_buffer => 'str',
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
	},
);

sub parse_hdata {
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
	my @keys = split /,/, ($4//"");

	my $cls = $hdata_classes{$hclass};

	$cls//do{
		    logmsg("Unknown HDATA class: $hclass");
		    return;
	};

	my ($objstr, $ct);
	($objstr, $ct, $path) = hdata_tok $path;

	my %objs;

	if ($objstr =~ m/^0x/)
	{
		logmsg("Got a HDATA for $path from pointer $objstr of $hclass with keys @keys");
		# Pointer value
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
		$objs{$ptr} = $obj;
	}
	else
	{
		exists $cls->{"list_$objstr"} or do {
			logmsg("List $objstr not defined for $hclass");
			return;
		};
		logmsg("Got a HDATA for $ct $path from list $objstr from $hclass with keys @keys");
		my @obj = ($cls->{"list_$objstr"}->($ct));
		unless (@obj) {
			logmsg("No objects returned for list $objstr from $hclass");
			return;
		}
		for my $obj (@obj) {
			my $ptr = sprintf("%016x", ($cls->{get_pointer}->($obj)));
			$objs{$ptr} = $obj;
		}
	}

	while ($path ne '')
	{
		my %results;
		($objstr, $ct, $path) = hdata_tok $path;
		$s = $cls->{"sublist_$obstr"};
		$st = $cls->{"type_sublist_obstr"};
		$s//do {
			logmsg("No sublist $objstr in $hclass");
			return;
		};
		$st//do {
			logmsg("Don't know type of items in sublist $objstr in $hclass");
			return;
		};
		$hclass .= "/" . $st;
		$newcls = $hdata_classes{$st};
		$newcls//do {
			logmsg("Don't recognize type $st of items in sublist $objstr in $hclass");
			return;
		}
		for my $ptr (keys %objs)
		{
			my $obj = $objs{$ptr};
			my @r = $s->($obj, $ct);
			for my $r (@r)
			{
				my $newp = $ptr . "/" . sprintf("%016x", ($newcls->{get_pointer}->($obj)));
				$results{$newp} = $r;
			}
		}
		$cls = $newcls;
		%objs = %results;
	}

	my @keytypes;

	if (!@keys)
	{
		@keys = map { /^key_(.*)$/ && $1 } grep { /^key_/ && exists $cls->{"type_key_$_"} } keys $cls;
	}
	else
	{
		@keys = grep { exists $cls->{"key_$_"} && exists $cls->{"type_key_$_"} } @keys;
	}

	@keytypes = map { $_ . ":" . $cls->{"type_key_$_"} } @keys;

	my $m = new WeechatMessage;

	$m->add_string($id);
	$m->add_type("hda");
	$m->add_string($hclass);
	$m->add_string(join ",", @keytypes);
	$m->add_int(length keys %objs);

	for my $ptr (%objs)
	{
		# Add the p-path
		my @ppath = split /\//, $ptr;
		my $obj = $objs{$ptr};
		for my $pptr (@ppath)
		{
			$m->add_ptr($pptr);
		}
		for my $k (@keys)
		{
			$cls->{"key_$k"}->($obj, $m);
		}
	}

	sendto_client($client, $m->get_buffer());

	return;

    # POD out w00t's version for mine
=pod
    # $arguments = "hotlist:gui_hotlist(*)"
    # hdata_head here will be "hotlist"
    # everything after the : is split on '/' and put into list_path
    # list_path[0] is the important bit


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

=cut
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


