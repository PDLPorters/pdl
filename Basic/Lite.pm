=head1 NAME

PDL::Lite - minimum PDL module OO loader

=head1 DESCRIPTION

Loads the smallest possible set of modules for
PDL to work, without importing an functions in
to the current namespace. This is the absolute
minimum set for PDL.

Although no functions are defined (apart from
a few always exported by L<PDL::Core|PDL::Core>) you can still
use method syntax, viz:

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

$PDL::Lite::VERSION = $PDL::Version::VERSION;

;# Exit with OK status

1;
