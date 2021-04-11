use Test::More;

use strict;
use warnings;

use PDL::LiteF;

## Issue information
##
## Name: BAD value parsing breakage
##
## Parsing of BAD values fails to set the correct BAD value when parsing from
## the string "[BAD]".
##
## <http://sourceforge.net/p/pdl/bugs/367/>
## <https://github.com/PDLPorters/pdl/issues/47>

# input string -> expected string
my $cases = {
	q|BAD|         => q|BAD|,
	q|BAD BAD|     => q|[BAD BAD]|,
	q|BAD BAD BAD| => q|[BAD BAD BAD]|,
	q|[BAD]|       => q|[BAD]|,
	q|[ BAD ]|     => q|[BAD]|,
	q|[BAD BAD]|   => q|[BAD BAD]|,
	q|[ BAD BAD ]| => q|[BAD BAD]|,
};

plan tests => scalar keys %$cases;

while( my ($case_string, $expected_string) = each %$cases ) {
	my $bad_pdl = pdl( $case_string );
	subtest "Testing case: $case_string" => sub {
		ok( $bad_pdl->badflag, 'has badflag enabled');
		ok( $bad_pdl->isbad->all, 'all values in PDL are BAD');

		is($bad_pdl->string, $expected_string, "PDL stringifies back to input string: @{[ $bad_pdl->string ]}");
	};
}

done_testing;
