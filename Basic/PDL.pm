=head1 NAME

PDL - Main loader of PDL default modules

=head1 DESCRIPTION

Loads the default set of modules associated
with PDL, making the functions available in
the current namespace. See also C<PDL::Lite>,
C<PDL::LiteF> if start-up time becomes an
issue.

=head1 SYNOPSIS

 use PDL; # Is equivalent to the following:

   use PDL::Core;
   use PDL::Ops;
   use PDL::Primitive;
   use PDL::Basic;
   use PDL::Slices;
   use PDL::Version;
   use PDL::IO::Misc;
   use PDL::Graphics::PGPLOT;

=cut

# Main loader of standard PDL package

sub PDL::import {

my $pkg = (caller())[0];
eval <<"EOD";

package $pkg;

# Load the fundamental packages

use PDL::Core;
use PDL::Ops;
use PDL::Primitive;
use PDL::Basic;
use PDL::Slices;
use PDL::Version;

# Load these for TPJ compatibility

use PDL::IO::Misc;          # Misc IO (Ascii/FITS)
use PDL::Graphics::PGPLOT;  # PGPLOT graphics

EOD

die $@ if $@;

}


;# Exit with OK status

1;
