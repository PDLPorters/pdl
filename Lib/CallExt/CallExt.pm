
package PDL::CallExt;

@EXPORT_OK  = qw( callext callext_cc );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);
@EXPORT = @EXPORT_OK;

use Config;
use PDL::Core;
use PDL::Exporter;
use DynaLoader;
use Carp;
@ISA    = qw( PDL::Exporter DynaLoader );

bootstrap PDL::CallExt;

=head1 NAME

PDL::CallExt - call functions in external shared libraries

=head1 SYNOPSIS

 use PDL::CallExt;
 callext('file.so', 'foofunc', $x, $y); # pass piddles to foofunc()

 % perl -MPDL::CallExt -e callext_cc file.c

=head1 DESCRIPTION

callext() loads in a shareable object (i.e. compiled code) using Perl's
dynamic loader, calls the named function and passes a list of piddle arguments
to it.

It provides a reasonably portable way of doing this, including compiling the
code with the right flags, though it requires simple perl and C wrapper
routines to be written. You may prefer to use PP, which is much more
portable. See L<PDL::PP>. You should definitely use the latter for a 'proper'
PDL module, or if you run in to the limitations of this module.

=head1 API

callext_cc() allows one to compile the shared objects using Perl's knowledge
of compiler flags.

The named function (e.g. 'foofunc') must take a list of piddle structures as
arguments, there is now way of doing portable general argument construction
hence this limitation.

In detail the code in the original file.c would look like this:

 #include "pdlsimple.h" /* Declare simple piddle structs - note this .h file
 			   contains NO perl/PDL dependencies so can be used
 			   standalone */

 int foofunc(int nargs, pdlsimple **args); /* foofunc prototype */


i.e. foofunc() takes an array of pointers to pdlsimple structs. The use is
similar to that of C<main(int nargs, char **argv)> in UNIX C applications.

pdlsimple.h defines a simple N-dimensional data structure which looks like this:

  struct pdlsimple {
     int    datatype;  /* whether byte/int/float etc. */
     void  *data;      /* Generic pointer to the data block */
     int    nvals;     /* Number of data values */
     PDL_Long *dims;   /* Array of data dimensions */
     int    ndims;     /* Number of data dimensions */
  };

(PDL_Long is always a 4 byte int and is defined in pdlsimple.h)

This is a simplification of the internal reprensation of piddles in PDL which is
more complicated because of threading, dataflow, etc. It will usually be found
somewhere like /usr/local/lib/perl5/site_perl/PDL/pdlsimple.h

Thus to actually use this to call real functions one would need to right a wrapper.
e.g. to call a 2D image processing routine:

 void myimage_processer(double* image, int nx, int ny);

 int foofunc(int nargs, pdlsimple **args) {
    pdlsimple* image = pdlsimple[0];
    myimage_processer( image->data, *(image->dims), *(image->dims+1) );
    ...
 }

Obviously a real wrapper would include more error and argument checking.

This might be compiled (e.g. Linux):

 cc -shared -o mycode.so mycode.c

In general Perl knows how to do this, so you should be able to get
away with:

 perl -MPDL::CallExt -e callext_cc file.c

callext_cc() is a function defined in PDL::CallExt to generate the
correct compilation flags for shared objects.

If their are problems you will need to refer to you C compiler manual to find
out how to generate shared libraries.

See t/callext.t in the distribution for a working example.

It is up to the caller to ensure datatypes of piddles are correct - if not
peculiar results or SEGVs will result.


=head1 FUNCTIONS

=head2 callext

=for ref

Call a function in an external library using Perl dynamic loading

=for usage

  callext('file.so', 'foofunc', $x, $y); # pass piddles to foofunc()

The file must be compiled with dynamic loading options
(see C<callext_cc>). See the module docs C<PDL::Callext>
for a description of the API.

=head2 callext_cc

=for ref

Compile external C code for dynamic loading

=for usage

Usage:

 % perl -MPDL::CallExt -e callext_cc file.c -o file.so

This works portably because when Perl has built in knowledge of how to do
dynamic loading on the system on which it was installed.
See the module docs C<PDL::Callext> for a description of
the API.

=cut

sub callext{
    die "Usage: callext(\$file,\$symbol, \@pdl_args)" if scalar(@_)<2;
    my($file,$symbol, @pdl_args) = @_;

    my $libref = DynaLoader::dl_load_file($file);
    my $err    = DynaLoader::dl_error(); barf $err if !defined $libref;
    my $symref = DynaLoader::dl_find_symbol($libref, $symbol);
    $err       = DynaLoader::dl_error(); barf $err if !defined $symref;

    _callext_int($symref, @pdl_args);
1;}

# Compile external C program correctly

#
# callext_cc
#
# The old version of this routine was taking unstructured arguments and
# happily passed this though the C compiler. Unfortunately, on platforms
# like HP-UX, we need to make separate cc and ld runs in order to create the
# shared objects.
#
# The signature of the function was therefore changed starting at PDL 2.0.
# It is now:
#
#   ($src, $ccflags, $ldflags, $output)
#
# In its simplest invocation, it can be just $src, and the output will be
# derived from the source file. Otherwise, $ccflags add extra C flags, $ldflags
# adds extra ld flags, and $output specifies the final target output file name.
# If left blank, it will be in the same directory where $src lied.
#
sub callext_cc {
	my @args = @_>0 ? @_ : @ARGV;
	my ($src, $ccflags, $ldflags, $output) = @args;
	my $cc_obj;
	($cc_obj = $src) =~ s/\.c$/$Config{_o}/;
	my $ld_obj = $output;
	($ld_obj = $cc_obj) =~ s/\.o$/\.$Config{dlext}/ unless defined $output;

	# Output flags for compiler depend on os.
	# -o on unix or /Fo" " on WindowsNT
	# Need a start and end string
	my $do = ( $^O =~ /MSWin/i ? '/Fo"' : '-o ');
	my $eo = ( $^O =~ /MSWin/i ? '"' : '' );

	# Compiler command
	my $cc_cmd = join(' ', map { $Config{$_} } qw(cc ccflags cccdlflags)) .
		" -I$Config{installsitelib}/PDL/Core $ccflags -c $src $do$cc_obj$eo";

	# The linker output flag is -o on unix and -out: on Windows
	my $o = ( $^O =~ /MSWin/i ? '-out:' : '-o ');

	# Setup the LD command. Do not want the env var on Windows
	my $ld_cmd = ( $^O =~ /MSWin/i ? ' ' : 'LD_RUN_PATH="" ');

	my $libs = ''; # used to be $Config{libs} but that bombs
	               # on recent debian platforms
	$ld_cmd .=
		join(' ', map { $Config{$_} } qw(ld lddlflags)) .
		" $libs $ldflags $o$ld_obj $cc_obj";
	my $cmd = "$cc_cmd; $ld_cmd";
	print $cmd,"\n";

	# Run the command in two steps so that we can check status
	# of each and also so that we dont have to rely on ';' command
	# separator
	system $cc_cmd and croak "Error compiling $src";
	system $ld_cmd and croak "Error linking $cc_obj";
	return 1;
}

=head1 AUTHORS

Copyright (C) Karl Glazebrook 1997.
All rights reserved. There is no warranty. You are allowed
to redistribute this software / documentation under certain
conditions. For details, see the file COPYING in the PDL
distribution. If this file is separated from the PDL distribution,
the copyright notice should be included in the file.


=cut

# Exit with OK status

1;

