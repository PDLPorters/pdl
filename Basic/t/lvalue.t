use strict;
use warnings;
use Test::More;
use Test::Exception;

use PDL::LiteF;
use PDL::Lvalue;

ok (PDL::Lvalue->subs('slice'),"slice is an lvalue sub");

my $pa = sequence 10;
lives_ok {
	$pa->slice("") .= 0;
} "lvalue slice ran OK";

is($pa->max, 0, "lvalue slice modified values");

lives_ok {
	$pa->broadcast(0) .= 1;
} "lvalue broadcast ran OK";

done_testing;
