# Perform pdl demos on terminals

package PDL::Demos::Routines;

# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

use Carp;
use PDL;

@ISA="Exporter";
@EXPORT = qw/comment act actnw output/;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

sub home() {
	if(-e '/usr/bin/tput') {
		system 'tput clear';
	}
}

sub comment($) {
	home();
	print "----\n";
	print $_[0];
	print "---- (press enter)";
	<>
}

sub act($) {
	home();
	my $script = $_[0];
	$script =~ s/^(\s*)output/$1print/mg;
	print "---- Code:";
	print $script;
	print "---- Output:\n";
	my $pack = (caller)[0];
#	eval "package $pack; use PDL; $_[0]";
	eval "package $pack; use PDL; $_[0]";
	print "---- (press enter)";
	print "----\nOOPS!!! Something went wrong, please make a bug report!: $@\n----\n" if $@;
	<>
}

sub actnw($) {
	home();
	my $script = $_[0];
	$script =~ s/^(\s*)output/$1print/mg;
	print "---- Code:";
	print $script;
	print "---- Output:\n";
	my $pack = (caller)[0];
#	eval "package $pack; use PDL; $_[0]";
	eval "package $pack; use PDL; $_[0]";
	print "----\n";
	print "----\nOOPS!!! Something went wrong, please make a bug report!: $@\n----\n" if $@;
}

sub output {print @_}
