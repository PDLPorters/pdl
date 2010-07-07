=head1 NAME

PDL - the Perl Data Language

=head1 DESCRIPTION

(For the exported PDL constructor, pdl(), see L<PDL::Core|PDL::Core>)

PDL is the Perl Data Language, a perl extension that is designed for
scientific and bulk numeric data processing and display.  It extends
perl's syntax and includes fully vectorized, multidimensional array
handling, plus several paths for device-independent graphics output.

PDL is fast, comparable and often outperforming IDL and MATLAB in real
world applications. PDL allows large N-dimensional data sets such as large
images, spectra, etc to be stored efficiently and manipulated quickly. 

=head1 INTERACTIVE SHELL

The PDL package includes an interactive shell. You can learn about it,
run C<perldoc perldl>, or run the shell C<perldl> and type C<help>.

=head1 LANGUAGE DOCUMENTATION

To learn about the PDL language see:

=over 5

=item L<PDL::Intro|PDL::Intro>

Starting place for PDL language documentation.

=item L<PDL::QuickStart|PDL::QuickStart>

A guide to get you quickly started with PDL.

=item L<PDL::Philosophy|PDL::Philosophy>

Why did we write PDL? Learn about what PDL has to offer.

=item L<PDL::Index|PDL::Index>

List of all available documentation.

=back


=head1 MODULES

PDL includes about a dozen perl modules that form the core of the
language, plus additional modules that add further functionality.
The perl module "PDL" loads all of the core modules automatically,
making their functions available in the current perl namespace.
Some notes:

=over 5

=item SYNOPSIS

See the SYNOPSIS section at the end of this document for a list of
modules loaded by default.

=item L<PDL::Lite|PDL::Lite> and L<PDL::LiteF|PDL::LiteF>

These are lighter-weight alternatives to the standard PDL module.
Consider using these modules if startup time becomes an issue.

=item Exports

C<use PDL;> exports a large number of routines into the calling
namespace.  If you want to avoid namespace pollution, you must instead 
C<use PDL::Lite>, and include any additional modules explicitly.

=item L<PDL::NiceSlice|PDL::NiceSlice>

Note that the L<PDL::NiceSlice|PDL::NiceSlice> syntax is NOT automatically
loaded by C<use PDL;>.  If you want to use the extended slicing syntax in 
a standalone script, you must also say C<use PDL::NiceSlice;>.

=item L<PDL::Math|PDL::Math>

The L<PDL::Math|PDL::Math> module has been added to the list of modules
for versions later than 2.3.1. Note that PDL::Math is still
I<not> included in the L<PDL::Lite|PDL::Lite> and L<PDL::LiteF|PDL::LiteF>
start-up modules.

=back

=head1 SYNOPSIS

 use PDL; # Is equivalent to the following:

   use PDL::Core;
   use PDL::Ops;
   use PDL::Primitive;
   use PDL::Ufunc;
   use PDL::Basic;
   use PDL::Slices;
   use PDL::Bad;
   use PDL::MatrixOps;
   use PDL::Math;
   use PDL::Version;
   use PDL::IO::Misc;
   use PDL::IO::FITS;
   use PDL::IO::Pic;
   use PDL::Lvalue;

=cut


# set the version:
$PDL::VERSION = '2.4.6_014'; # Go to sub numbering per git push

# Main loader of standard PDL package

sub PDL::import {

my $pkg = (caller())[0];
eval <<"EOD";

package $pkg;

# Load the fundamental packages

use PDL::Core;
use PDL::Ops;
use PDL::Primitive;
use PDL::Ufunc;
use PDL::Basic;
use PDL::Slices;
use PDL::Bad;
use PDL::Math;
use PDL::MatrixOps;
use PDL::Lvalue;

# Load these for TPJ compatibility

use PDL::IO::Misc;          # Misc IO (Ascii)
use PDL::IO::FITS;          # FITS IO (rfits/wfits; used by rpic/wpic too)
use PDL::IO::Pic;           # rpic/wpic

EOD

die $@ if $@;

}


# Dummy Package PDL Statement. This is only needed so CPAN
# properly recognizes the PDL package.
package PDL;


# Exit with OK status

1;
