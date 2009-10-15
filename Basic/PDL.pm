=head1 NAME

PDL - the Perl Data Language

=head1 DESCRIPTION

(For the exported PDL constructor, pdl(), see L<PDL::Core> or pdl[2])

PDL is the Perl Data Language, a perl extension that is designed for
scientific and bulk numeric data processing and display.  It extends
perl's syntax and includes fully vectorized, multidimensional array
handling, plus several paths for device-independent graphics output.


For basic information on the PDL language, see L<the pdl(1)
(lowercase) man page|pdl>.

You can run PDL programs directly as perl scripts that include the
PDL module (with "use PDL;"), or via an interactive shell (see
L<the perldl(1) man page|perldl>).  

The PDL language extension includes about a dozen perl modules that
form the core of the language, plus additional modules that add
further functionality.  The perl module "PDL" loads all of the core
modules automatically, making their functions available in the current
perl namespace.  See also L<PDL::Lite|PDL::Lite> or
L<PDL::LiteF|PDL::LiteF> if start-up time becomes an issue.

=over 3

=item EXPORTS: 

C<use PDL;> exports a large number of routines into the calling
namespace.  If you want to avoid namespace pollution, you must instead 
C<use PDL::Lite>, and include any additional modules explicitly.

=item NICESLICE: 

Note that the L<PDL::NiceSlice|PDL::NiceSlice> syntax is NOT automatically
loaded by C<use PDL;>.  If you want to use the extended slicing syntax in 
a standalone script, you must also say C<use PDL::NiceSlice;>.

=item PDL::Math:

The PDL::Math module has been added to the list of modules
for versions later than 2.3.1. Note that PDL::Math is still
I<not> included in the
L<Lite|PDL::Lite> and L<LiteF|PDL::LiteF>
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
$PDL::VERSION = '2.4.4_136'; # Go to sub numbering per git push

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
