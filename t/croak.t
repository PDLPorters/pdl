use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use Test::Exception;

# PDL::Core::set_debugging(1);
my $pb = pdl [[1,1,1],[2,2,2]];

{
	# we are using more dims than are available
	throws_ok { my $pc = $pb->slice(':,:,:,(1)'); $pc->make_physical(); } qr/too many dims/i;
}

{
	# now see if we survive the destruction of this invalid trans
	my $pb = zeroes(5,3,3);
	lives_ok { my $pc = $pb->slice(":,:,1") };
}

{
	my $pb = pdl [[1,1,1],[2,2,2]];
	lives_ok { my $pc = $pb->dummy(5,1); $pc->make_physical(); };
}

{
	my $pb = zeroes(5,3,3);
	lives_ok { my $pc = $pb->slice(":,:,1"); };
}

# This test case points out a problem in the freeing
# of used memory in 1.90_01
lives_ok {
my $pa = pdl (1,2);
my $pb = pdl [[1,2],[1,2],[1,2]];
my $pc = $pa->slice(',*3');
$pc->make_physical;
$pc = $pb->clump(2);
$pb->make_physical;
$pc->make_physical;
};

lives_ok {
my $pa =  zeroes 4,5;
my $pb = $pa->slice('1:3:2,2:4:2');
$pb .=  ones(2,2);
note $pa;
};

# tests for error checking of input args to PP compiled function
{
	my $pb=pdl([1,2,3])->long;
	my $pa=[1,2,3];
	lives_ok { PDL::Ufunc::sumover($pa,$pb) } 'sumover with piddles of compatible dimensions does not die';
}

{
	my $paa=3;
	my $pa=\$paa;
	throws_ok { PDL::Ufunc::sumover($pa,$paa) } qr/Error - tried to use an unknown/;
}

{
	throws_ok { PDL::Ufunc::sumover({}) } qr/Hash given as a pdl - but not \{PDL} key/;
}

{
	my $pc = 0;
	throws_ok { PDL::Ufunc::sumover(\$pc) } qr/Error - tried to use an unknown/;
}

# This is something that would cause an exception on 1.91_00:
# when the original was undef'd, xchghashes would barf.
lives_ok {
	my $pa = xvals zeroes(5,5);
	my $pb = $pa->slice(':,2:3');
	$pa = 1;  # Undefine orig. a
	$pb += 1;
} "no barf when parent of slice undefined";

done_testing;
