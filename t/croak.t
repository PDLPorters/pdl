use PDL::LiteF;

use Test::More tests => 4;
use Test::Exception;

use strict;
use warnings;

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

# if we're here we survived
done_testing;
