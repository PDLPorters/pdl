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


=head1 VECTORIZATION 

For a description of the vectorization (also called "threading"), see
L<PDL::Core|PDL::Core>.


=head1 INTERACTIVE SHELL

The PDL package includes an interactive shell. You can learn about it,
run C<perldoc perldl>, or run the shell C<perldl> or C<pdl2> and type
C<help>.

=head1 LOOKING FOR A FUNCTION?

If you want to search for a function name, you should use the PDL
shell along with the "help" or "apropos" command (to do a fuzzy search).
For example:

 pdl> apropos xval
 xlinvals        X axis values between endpoints (see xvals).
 xlogvals        X axis values logarithmicly spaced...
 xvals           Fills a piddle with X index values...
 yvals           Fills a piddle with Y index values. See the CAVEAT for xvals.
 zvals           Fills a piddle with Z index values. See the CAVEAT for xvals.

To learn more about the PDL shell, see L<perldl|perldl> or L<pdl2|pdl2>.

=head1 LANGUAGE DOCUMENTATION

Most PDL documentation describes the language features. The number of
PDL pages is too great to list here. The following pages offer some
guidance to help you find the documentation you need.


=over 5

=item L<PDL::FAQ|PDL::FAQ>

Frequently asked questions about PDL. This page covers a lot of
questions that do not fall neatly into any of the documentation
categories.

=item L<PDL::Tutorials|PDL::Tutorials>

A guide to PDL's tutorial-style documentation. With topics from beginner
to advanced, these pages teach you various aspects of PDL step by step.

=item L<PDL::Modules|PDL::Modules>

A guide to PDL's module reference. Modules are organized by level
(foundation to advanced) and by category (graphics, numerical methods,
etc) to help you find the module you need as quickly as possible.

=item L<PDL::Course|PDL::Course>

This page compiles PDL's tutorial and reference pages into a comprehensive
course that takes you from a complete beginner level to expert.

=item L<PDL::Index|PDL::Index>

List of all available documentation, sorted alphabetically. If you
cannot find what you are looking for, try here.

=back


=head1 MODULES

PDL includes about a dozen perl modules that form the core of the
language, plus additional modules that add further functionality.
The perl module "PDL" loads all of the core modules automatically,
making their functions available in the current perl namespace.
Some notes:

=over 5

=item Modules loaded by default

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
   use PDL::IO::Storable;
   use PDL::Lvalue;

=cut


# set the version:
$PDL::VERSION = '2.020';

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

# Load this so config/install info is available

use PDL::Config;

# Load this to avoid mysterious Storable segfaults

use PDL::IO::Storable;

EOD

die $@ if $@;

}


# Dummy Package PDL Statement. This is only needed so CPAN
# properly recognizes the PDL package.
package PDL;

# support: use Inline with => 'PDL';
# Returns a hash containing parameters accepted by recent versions of
# Inline, to tweak compilation.  Not normally called by anyone but
# the Inline API.
#
# If you're trying to debug the actual code, you're looking for "IFiles.pm"
# which is currently in the Core directory. --CED 23-Feb-2015
sub Inline {
    require PDL::Install::Files;
    goto &PDL::Install::Files::Inline;
}

##################################################
# Rudimentary handling for multiple Perl threads #
##################################################
my $clone_skip_should_be_quiet = 0;
sub CLONE_SKIP {
    warn("* If you need to share PDL data across threads, use memory mapped data, or\n"
		. "* check out PDL::Parallel::threads, available on CPAN.\n"
        . "* You can silence this warning by saying `PDL::no_clone_skip_warning;'\n"
        . "* before you create your first thread.\n")
        unless $clone_skip_should_be_quiet;
    PDL::no_clone_skip_warning();
    # Whether we warned or not, always return 1 to tell Perl not to clone PDL data
    return 1;
}
sub no_clone_skip_warning {
    $clone_skip_should_be_quiet = 1;
}

# Exit with OK status
1;
