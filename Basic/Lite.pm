=head1 NAME

PDL::Lite - minimum PDL module OO loader

=head1 DESCRIPTION

Loads the smallest possible set of modules for
PDL to work, importing only those functions always defined by
L<PDL::Core|PDL::Core>) into the current namespace
(C<pdl>, C<piddle>, C<barf> and C<null>).
This is the absolute minimum set for PDL.

Access to other functions is by method syntax, viz:

  $x = PDL->pdl(1, 2, 3, 4, 5);
  $x->wibble(42);

=head1 SYNOPSIS

 use PDL::Lite; # Is equivalent to the following:

   use PDL::Core '';
   use PDL::Ops '';
   use PDL::Primitive '';
   use PDL::Ufunc '';
   use PDL::Basic '';
   use PDL::Slices '';
   use PDL::Bad '';
   use PDL::Version;
   use PDL::Lvalue;

=cut

# Load the fundamental PDL packages, no imports
# Because there are no imports, we do not need
# the usual 'eval in the user's namespace' routine.

use PDL::Core '';
use PDL::Ops '';
use PDL::Primitive '';
use PDL::Ufunc '';
use PDL::Basic '';
use PDL::Slices '';
use PDL::Bad '';
use PDL::Version ;  # Doesn't export anything - no need for ''
use PDL::Lvalue;

package PDL::Lite;
$VERSION = $PDL::Version::VERSION;

@ISA = qw( PDL::Exporter );

@EXPORT = qw( piddle pdl null barf ); # Only stuff always exported!
our %EXPORT_TAGS = (
   Func     => [@EXPORT],
);


;# Exit with OK status

1;
