use strict;
use warnings;
use Test::More;
use Test::Exception;

use PDL::LiteF;
use PDL::Lvalue;

BEGIN { 
    if ( PDL::Lvalue->subs and !$^P) {
	plan tests => 3;
    } else {
	plan skip_all => "no lvalue sub support";
    }
} 

$| = 1;

ok (PDL::Lvalue->subs('slice'),"slice is an lvalue sub");

my $pa = sequence 10;
lives_ok {
	$pa->slice("") .= 0;
} "lvalue slice ran OK";

is($pa->max, 0, "lvalue slice modified values");
