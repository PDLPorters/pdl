
use strict;
use warnings;
use Test::More;

BEGIN {
  eval "use PDL::Slatec;";
  if ( !$@ ) {
    eval "use PDL::Graphics::Limits;";
    plan tests => 12;
  } else {
    plan skip_all => 'PDL::Slatec not available';
  }
};

*parse_vecspec = \&PDL::Graphics::Limits::parse_vecspec;

#################################################################

# test parsing of hash key specs

my @good = (
            'x<n>p&f' => { data => 'x',
      		     errn   => 'n',
      		     errp   => 'p',
      		     trans => 'f' },
            
            '<n>p&f' => { errn   => 'n',
      		    errp   => 'p',
      		    trans => 'f' },
            
            'x,<n,>p,&f' => { data => 'x',
      			errn   => 'n',
      			errp   => 'p',
      			trans => 'f' },

            'x <n >p &f' => { data => 'x',
      			errn   => 'n',
      			errp   => 'p',
      			trans => 'f' },

            
            '<n>p&f'  => { errn   => 'n',
      		     errp   => 'p',
      		     trans => 'f' },
            
            '&f'      => { trans => 'f' },

            'x'       => { data => 'x' },

            '&'       => { trans => undef },

            undef()   =>  { },

            '<>&'     => { errn   => undef,
      		     errp   => undef,
      		     trans => undef },

            '=s'  => { errn   => 's',
      	         errp   => 's' },


);

while( my ( $spec, $exp ) = splice( @good, 0, 2 ) )
{
  my $res = { parse_vecspec($spec) };
  ok( eq_hash( $exp, $res ), defined $spec ? $spec : 'undef' );
}

my @bad = ( '<<' );

for my $spec ( @bad )
{
  eval { parse_vecspec($spec) };
  ok( $@, "$spec" );
}

