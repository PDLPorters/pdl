use strict;
use warnings;
use English;

use Test::More;

use PDL::LiteF;
use PDL::Lvalue;

BEGIN { 
    if ( PDL::Lvalue->subs and !$PERLDB) {
	plan tests => 3;
    } else {
	plan skip_all => "no lvalue sub support";
    }
} 

$| = 1;

ok (PDL::Lvalue->subs('slice'),"slice is an lvalue sub");

$a = sequence 10;
eval '$a->slice("") .= 0';

ok (!$@, "lvalue slice ran OK") or diag($@);

is ($a->max, 0, "lvalue slice modified values");
