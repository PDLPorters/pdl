use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use PDL::Slatec;

## Issue information
##
## Name: PDL::Slatec::polyfit ignores incorrect length of weight piddle; passes
##       garbage to slatec polfit
##
## <https://sourceforge.net/p/pdl/bugs/368/>
## <https://github.com/PDLPorters/pdl/issues/48>

## Set up data
my $ecf = sequence(999);

my $y = $ecf->lags( 0, 9, 111 );
my $x = sequence( 9 );

my $polyfit_orig;
lives_ok { $polyfit_orig = polyfit( $x, $y, $x->ones, 4, .0001 ); } 'polyfit() works when the weight $w matches the length of $x';

subtest 'Passing the weight in a PDL of length 1' => sub {
	my $polyfit_pdl_len_one;
	lives_ok { $polyfit_pdl_len_one = polyfit( $x, $y, pdl(1), 4, .0001 ); };
	ok( approx($polyfit_orig, $polyfit_pdl_len_one)->all, 'passing a PDL of length 1 expands to the correct length' );
};

subtest 'Passing the weight in a Perl scalar' => sub {
	my $polyfit_perl_scalar;
	lives_ok { $polyfit_perl_scalar = polyfit( $x, $y, 1, 4, .0001 ) };
	ok( approx($polyfit_orig, $polyfit_perl_scalar)->all, 'passing a Perl scalar expands to the correct length' );
};

done_testing;
