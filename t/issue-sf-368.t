use Test::More;
use Test::Exception;

use strict;
use warnings;

## Issue information
##
## Name: PDL::Slatec::polyfit ignores incorrect length of weight piddle; passes
##       garbage to slatec polfit
##
## <https://sourceforge.net/p/pdl/bugs/368/>
## <https://github.com/PDLPorters/pdl/issues/48>

use PDL::LiteF;
BEGIN {
	eval {
		require PDL::Slatec;
		PDL::Slatec->import();
		1;
	} or do {
		plan skip_all => "PDL::Slatec not available";
	}
}

plan tests => 5;

my $ecf = sequence(999);

my $y = $ecf->lags( 0, 9, 111 );
my $x = sequence( 9 );

my $polyfit_orig;

lives_ok { $polyfit_orig = polyfit( $x, $y, $x->ones, 4, .0001 ); } 'polyfit() works when the weight $w matches the length of $x';

TODO: {
	local $TODO = q|The test dies because the weight parameter ($w) is the incorrect length for the underlying Slatec polfit function|;
	my $polyfit_pdl_len_one;
	lives_ok
		{ $polyfit_pdl_len_one = polyfit( $x, $y, pdl(1), 4, .0001 ); }
		'Passing the weight in a PDL of length 1';

	my $polyfit_perl_scalar;
	lives_ok
		{ $polyfit_perl_scalar = polyfit( $x, $y, 1, 4, .0001 ) }
		'Passing the weight in a Perl scalar';

	ok( approx($polyfit_orig, $polyfit_pdl_len_one)->all, 'passing a PDL of length 1 expands to the correct length' );
	ok( approx($polyfit_orig, $polyfit_perl_scalar)->all, 'passing a Perl scalar expands to the correct length' );
}

done_testing;
