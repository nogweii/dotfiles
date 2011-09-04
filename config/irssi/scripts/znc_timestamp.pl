use Irssi;
use Irssi::Irc;
use DateTime;
use strict;

use vars qw($VERSION %IRSSI);

$VERSION = "0.01";
%IRSSI = (
	authors     => 'Domen Puncer',
	contact     => 'domen@cba.si',
	name        => 'znc_timestamp',
	description => 'Replace znc timestamps with native irssi ones',
	license     => 'GPLv2',
);

# this script was based on bitlbee_timestamp by Tijmen "timing" Ruizendaal

my $tf = Irssi::settings_get_str('timestamp_format');

my $prev_date = '';

sub privmsg {
	my ($server, $data, $nick, $address) = @_;


	my ($target, $text) = split(/ :/, $data, 2);
	action($server, $text, $nick, $address, $target, 1);
}

sub action {
	my ($server, $text, $nick, $address, $target, $privmsg) = @_;

	# What we need to match: ^[17:05:40]
	if ($text =~ /^(\x01ACTION )?\[..:..:..\]/) {
		my $window;
		my $time = $text;
		my $date;
		$time =~ s/^(\x01ACTION )?\[(..:..:..)\] .*/$2/g;
		$text =~ s/\[(..:..:..)\] //;

		# use today as date
		$date = DateTime->now->ymd;

		if( $date ne $prev_date ){
			if( $target =~ /#|&/ ){ # find channel window based on target
				$window = Irssi::window_find_item($target);
			} else { # find query window based on nick
				$window = Irssi::window_find_item($nick);
			}
			if( $window != undef ){
				my($year, $month, $day) = split(/-/, $date);
				my $dt = DateTime->new(year => $year, month => $month, day => $day);
				my $formatted_date = $day.' '.$dt->month_abbr.' '.$year;
				
				$window->print('Day changed to '.$formatted_date, MSGLEVEL_NEVER);
			}
		}
		$prev_date = $date;
		
		#print "change timestamp to |$time| because of:".$server->{tag}.$target." <$nick> $text";
		Irssi::settings_set_str('timestamp_format', $time);

		# different arguments for privmsg vs. action
		if ($privmsg) {
			Irssi::signal_continue($server, $target . ' :' . $text, $nick, $address);
		} else {
			Irssi::signal_continue($server, $text, $nick, $address, $target);
		}

		my $escaped = $tf;
		$escaped =~ s/%/%%/g;
		#print "change back to $escaped";
		Irssi::settings_set_str('timestamp_format', $tf);
	}
}

Irssi::signal_add('event privmsg', 'privmsg');
Irssi::signal_add('event notice', 'privmsg');
Irssi::signal_add('ctcp action', 'action');
