# Test datatype sizes in bytes are correct

use strict;
use warnings;

use Test::More;
use PDL::LiteF;
use PDL::Core ':Internal'; # For howbig()

my @types = (
	{ typefunc => *byte  , size => 1 },
	{ typefunc => *short , size => 2 },
	{ typefunc => *ushort, size => 2 },
	{ typefunc => *long  , size => 4 },
	{ typefunc => *float , size => 4 },
	{ typefunc => *double, size => 8 },
);

plan tests => 0+@types;

for my $type (@types) {
	my $pdl = $type->{typefunc}(42); # build a PDL with datatype $type->{type}
	is( howbig( $pdl->get_datatype ), $type->{size} );
}
