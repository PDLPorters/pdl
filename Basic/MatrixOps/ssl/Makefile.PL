#
# Can we not use ExtUtils::MakeMaker here to create the
# make file and so avoid having to add in all the work arounds
# for various system/OS combinations? Or perhaps the work
# should be done in the parent Makefile.PL and get it to
# compile these files automatically (so no need for a
# Makefile.PL here)?
#

use strict;
use Config;
use IO::File;

sub write_mswin32 ()
{
    my $cc = $Config{cc};

    my $opt;
    my $obj;
    if($cc eq 'cl') {
	$opt = '-O2';
	$obj = '.obj';
    } else {
	$opt = '-O3';
	$obj = '.o';
    }

    my $line;
    if($Config{make} eq 'nmake') {
	$line = ".c$obj:\n\t$cc -c $opt \$*.c";
    } else {
	$line = "\%.o: \%.c Makefile\n\t$cc -c $opt \$<";
    }

    my $fh = IO::File (">Makefile")
	or die "Can't open ssl/Makefile for writing: $!\n";
    $fh->print ("

all: complex$obj eigen$obj matrix$obj sslib$obj

$line

clean ::
	-ren Makefile Makefile.old <NUL
	-del /F /Q complex$obj eigen$obj matrix$obj sslib$obj <NUL

realclean ::
	del /F /Q Makefile Makefile.old complex$obj eigen$obj matrix$obj sslib$obj <NUL

");

    $fh->close
	or die "Can't close ssl/Makefile after writing: $!\n";

} # write_mswin32()

sub write_other ()
{
    my $ccflags = $Config{cflags} || "";

    # At the moment there is no real point in this line
    # since -fPIC has been hard-coded into the cc line
    # in the Makefile.
    #
    $ccflags .= " -fPIC"
	if $ENV{HOSTTYPE} =~ m/x86_64/;

    my $fh = IO::File->new (">Makefile")
	or die "Unable to create ssl/Makefile: $!\n";

    $fh->print ("

all: complex.o eigen.o matrix.o sslib.o

%.o: %.c Makefile
	cc $ccflags -fPIC -c -O3 \$<

clean:
	-mv Makefile Makefile.old
	rm -f complex.o eigen.o matrix.o sslib.o

realclean:
	rm -f Makefile.old Makefile complex.o eigen.o matrix.o sslib.o

");

    $fh->close
	or die "Unable to close ssl/Makefile: $!\n";
}

# Write the makefile
#
if ($^O =~ /mswin32/i) { write_mswin32 (); }
else                   { write_other (); }

# End
#
