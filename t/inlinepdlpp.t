use strict;
use Test;
use blib;  # otherwise possible error on virgin systems not finding PDL::Core

use PDL::LiteF;

BEGIN {
    eval 'use Inline 0.43';
    unless ($@) {
	plan tests => 3;
    } else {
	plan tests => 1;
	print "ok 1 # Skipped: Inline not installed\n";
	exit;
    }
}

sub shape { join ',', $_[0]->dims }

use Inline 'Pdlpp';

ok(1); # ok, we made it so far

$a = sequence(3,3);

$b = $a->testinc;

ok(shape($a) eq shape($b));

ok(all $b == $a+1);

__DATA__

__Pdlpp__

# simple PP definition

pp_def('testinc',
	Pars => 'a(); [o] b()',
	Code => '$b() = $a() + 1;' # wow, that's complicated
);
