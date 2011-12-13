no warnings qw(misc);

use Test::More;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 1;
  } else {
     print "$@\n";
    plan skip_all => 'PDL::Slatec not available';
  }
  use_ok('PDL::Graphics::Limits');
};

# end
