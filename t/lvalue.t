use strict;
use Test;

use PDL::LiteF;
use PDL::Lvalue;

BEGIN { 
    if ( PDL::Lvalue->subs ) {
	plan tests => 3;
    } else {
	plan tests => 1;
	print "ok 1 # Skipped: no lvalue sub support\n";
	exit;
    }
} 

$| = 1;

ok (PDL::Lvalue->subs('slice'));

$a = sequence 10;
eval '$a->slice("") .= 0';

ok (!$@);

ok ($a->max, 0);
