#
#	Makefile.PL for Term::ReadLine::Gnu
#
#	$Id: Makefile.PL,v 1.32 2009/03/01 02:57:15 hiroo Exp $
#
#	Copyright (c) 2009 Hiroo Hayashi.  All rights reserved.
#		<hiroo.hayashi@computer.org>
#
#	This program is free software; you can redistribute it and/or
#	modify it under the same terms as Perl itself.
#
#	OS/2 support was contributed by Ilya Zakharevich.
#		<ilya@math.ohio-state.edu>
#
# Usage: perl Makefile.PL [--prefix=...] [--includedir=...] [--libdir=...]
#		[OPTIMIZE=...]
#
#	Read INSTALL for more details.
########################################################################
use strict;
use ExtUtils::MakeMaker;
use Config;
use Getopt::Long;
use 5.007; use 5.7.0;		# use version 1.09 for older Perl
my ($defs, $libs, $lddflags, $RLLIB, $RLINC);

# exit 0 before creating the Makefile to be CPAN Testers friendly
# see http://wiki.cpantester.org/wiki/CPANAuthorNotes
my $err = $ENV{AUTOMATED_TESTING} ? 0 : 1;
# test /dev/tty for CPAN Testers
if ($ENV{AUTOMATED_TESTING} && !open(SESAMI, '/dev/tty')) {
    warn "cannot open /dev/tty\n";
    exit 0;
}

$defs = ($Config{strings} =~ m|/string.h$|) ? '-DHAVE_STRING_H' : '';

# Parse command line to specify paths for the GNU Readline Library
{
    my ($prefix, $libdir, $incdir);
    GetOptions("prefix=s" => \$prefix,
	       "libdir=s" => \$libdir,
	       "includedir=s" => \$incdir);
    $RLLIB = defined $libdir
	? "-L$libdir" : (defined $prefix ? "-L$prefix/lib" : '');
    $RLINC = defined $incdir
	? "-I$incdir" : (defined $prefix ? "-I$prefix/include" : '');
}

if ($Config{osname} eq 'os2') {
    # Check ftp://ftp.math.ohio-state.edu/pub/users/ilya/os2/
    $libs = '-lreadline_import';
    $defs .= ' -DOS2_USEDLL';
    $lddflags = '';
} else {
    # Search libtermcap, libncurses, or libcurses in this order.
    # I emulate the behavior of the configure script for bash, and don't
    # know why AIX prefers curses.
    # libtermcap.a on HPUX cannot be used for dynamically linked binary.
    # Old Cygwin may require setting false (0).
    my $PREFER_CURSES = $Config{osname} eq 'aix' || $Config{osname} eq 'hpux'
			|| $Config{osname} eq 'cygwin';
    my $TERMCAP_LIB = (! $PREFER_CURSES && &search_lib('-ltermcap'))
	|| &search_lib('-lncurses')
	    || &search_lib('-lcurses');

    unless ($TERMCAP_LIB) {
	warn "Could not find neither libtermcap.a, libncurses.a, or libcurses.\n";
	# exit $err;
    } 
 
    $libs = "-lreadline $TERMCAP_LIB";
    # Latest Perl in FreeBSD does not need this hack. (Dec.2002)
    $libs .= ' -lcrypt' if ($Config{osname} =~ /freebsd/i);
    $lddflags = '';
    # If you are using old Cygwin, enable the following line.
    #$lddflags = ($Config{osname} =~ /cygwin/i) ? '-static' : '';
}

# Check version of  GNU Readline Library (for version 4.2 and before)
{
    my ($rlmajorver, $rlminorver) =
	check_readline_version($RLINC, $RLLIB, $defs, $lddflags, $libs);

    if ($rlmajorver < 4 || $rlmajorver == 4 && $rlminorver <= 2) {
	$defs .= " -DRL_READLINE_VERSION=" .
	    sprintf("0x%02x%02x", $rlmajorver, $rlminorver);
	$defs .= " -DRL_VERSION_MAJOR=$rlmajorver";
	$defs .= " -DRL_VERSION_MINOR=$rlminorver";
    }
}

# generate a Makefile
WriteMakefile
    (
     NAME	  => 'Term::ReadLine::Gnu',
     VERSION_FROM => 'Gnu.pm',
     LIBS	  => [ "$RLLIB $libs" ],
     LDDLFLAGS	  => "$RLLIB $Config{lddlflags}",
     dynamic_lib  => { OTHERLDFLAGS => $lddflags },
     DEFINE	  => $defs,
     ($Config{osname} eq 'os2' ?
      (
       IMPORTS	  => { xfree => 'emxlibcm.401' }, # Yuck!
      ) : () ),
     INC	  => $RLINC,
     dist	  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz' },
     clean	  => { FILES => "rlver.c rlver$Config{_exe}" },
);

if ($Config{usesfio} eq 'true') {
    warn <<'EOM';

******************** !!!Warning!!! *********************
** Your Perl is configured as `usesfio' equals true.  **
** Term::ReadLine::Gnu may not work with your Perl.   **
** If it works, let me know your result of `perl -V'. **
********************************************************
EOM
}

exit(0);

########################################################################
# Search a library '$lib' in $Config{libpth} directories, and return
# $lib if exist or undef unless exist.

# ExtUtils::Liblist::ext() do similar job as this subroutine, but it
# warns unnecessary messages.
sub search_lib {
    my ($lib) = @_;
    unless ($lib =~ /^-l/) {
	warn "search_lib: illegal arguments, \`$lib\'.\n";
	return undef;
    }
    my $libbase = 'lib' . substr($lib, 2) . $Config{lib_ext};
    my $libbase_so = 'lib' . substr($lib, 2) . "." . $Config{so};
    foreach (split(' ', $Config{libpth})) {
	if (-f $_ . '/' . $libbase) {
#	    print "$_/$libbase\n";
	    print "Found \`$_/$libbase\'.\n";
	    return $lib;
	} elsif (-f $_ . '/' . $libbase_so) {
#	    print "$_/$libbase_so\n";
	    print "Found \`$_/$libbase_so\'.\n";
	    return $lib;
	}
    }
    return undef;
}

########################################################################
# Check libreadline.a version
#
# Readline 4.2a introduced the macro
#	RL_READLINE_VERSION
#	RL_VERSION_MAJOR
#	RL_VERSION_MINOR
# Someday we don't need this subroutine..
sub check_readline_version {
    my ($RLINC, $RLLIB, $defs, $lddflags, $libs) = @_;
    my $frlver = 'rlver.c';

    # make temp file
    open(F, ">$frlver") || die "Cannot open $frlver:$!\n";
    print F <<'EOF';
/* used by Makefile.pl to check the version of the GNU Readline Library */
#include <stdio.h>
#include <readline/readline.h>
main() { puts(rl_library_version); }
EOF
    close(F);

    # compile it
    my $comp_cmd = "$Config{cc} $RLINC $Config{ccflags} $defs $frlver -o rlver $RLLIB $lddflags $Config{ldflags} $libs";
    print $comp_cmd, "\n";
    system($comp_cmd);
    if ($?) {
	warn <<EOM;
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Could not compile $frlver.

If you have installed the GNU Readline Library (libreadline.{a,so} and
readline/readline.h, etc.) on directories for which your perl is not
configured to search (refer the value of `ccflags' and `libpath' in
the output of `perl -V'), specify the paths as follows;

	perl Makefile.PL --includedir=/yourdir/include --libdir=/yourdir/lib
or
	perl Makefile.PL --prefix=/yourdir

Note that the GNU Readline Library version 2.0 and earlier causes error
here.  Update it to version 2.1 and/or later.

Read INSTALL for more details.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOM
    exit $err;
    }

    # execute it and get version
    my $rlver;
    chomp($rlver = `./rlver`);
    # MacOS X 10.4 has libedit by default.
    if ($rlver =~ /EditLine/) {
	warn <<EOM;
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
The libreadline you are using is the libedit library.  Use the GNU Readline Library.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOM
	exit $err;
    }
    print "It seems that you have the GNU Readline Library version $rlver.\n";
    # $rlver may be '8.21-beta3' or '4.2a'
    return $rlver =~ /(\d+)\.(\d+)/;
}
# End of Makefile.PL
