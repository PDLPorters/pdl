# tests for error checking of input args to PP compiled function

use Test::More tests => 4;
use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub eprint {
	print "EXPECT ERROR NEXT:\n-----\n";
	print $_[0];
	print "-----\n";
}

my $b=pdl([1,2,3])->long;
my $a=[1,2,3];
eval 'PDL::Ufunc::sumover($a,$b)';

is $@, '';

$aa=3;
$a=\$aa;
eval 'PDL::Ufunc::sumover($a,$b)';
eprint $@;
like($@, qr/Error - tried to use an unknown/);

eval { PDL::Ufunc::sumover({}) };
eprint $@;

like $@, qr/Hash given as a pdl - but not \{PDL} key/;


$c = 0;
eval { PDL::Ufunc::sumover(\$c) };
eprint $@;

like $@, qr/Error - tried to use an unknown/;
