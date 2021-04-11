use Test::More tests => 5;
use Test::Warn;

use strict;
use warnings;

## Issue information
##
## Name: "isn't numeric in null operation" warning could be more helpful
##
## <http://sourceforge.net/p/pdl/bugs/332/>
## <https://github.com/PDLPorters/pdl/issues/33>

use PDL::LiteF;

# The following code calls the PDL::Ops::eq() function via the operator
# overload for the eq operator. Because the Perl eq operator is usually used
# for strings, the default warning of "isn't numeric in null operation" is
# confusing. Comparing a PDL against a string should give a more useful
# warning.

my $numeric_warning = [qr/not numeric nor a PDL/];
my $no_warning = undef;

sub check_eq_warnings {
	my ($string, $warning) = @_;
	warnings_exist { (pdl() eq $string); } $warning;
	warnings_exist { ($string eq pdl()); } $warning;
}

subtest "String 'x' is not numeric and should warn" => sub {
	check_eq_warnings('x', $numeric_warning);
};
subtest "String 'nancy' is not numeric and should warn" => sub {
	check_eq_warnings('nancy', $numeric_warning);
};
subtest "String 'inf' is numeric" => sub {
	check_eq_warnings('inf', $no_warning);
};
subtest "String 'nan' is numeric" => sub {
	check_eq_warnings('nan', $no_warning);
};
TODO: {
	# implementing this might require checking for strings that can be made into PDLs
	local $TODO = "Using the eq operator with the string 'bad' might be a good feature";
	subtest "String 'bad' is numeric (in PDL)" => sub {
		check_eq_warnings('bad', $no_warning);
	};
}

done_testing;
