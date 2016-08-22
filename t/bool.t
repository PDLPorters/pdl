use Test::More tests => 6;
use Test::Exception;
use PDL::LiteF;
use strict;
use warnings;

#  PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

{
	my $pa = zeroes 1,1,1;
	ok !$pa, 'single-element multi-dim piddle collapses';
}

{
	my $pa = ones 3;
	throws_ok { print "oops\n" if $pa } qr/multielement/;
}

$a = ones 3;
eval {print "oops\n" if $a};
like $@, qr/multielement/, 'multielement piddle in conditional expression';

ok all($a),'all elements true';

$a = pdl byte, [ 0, 0, 1 ];
ok any($a > 0),'any element true';

$a = ones 3;
$b = $a + 1e-4;
ok all(PDL::approx $a, $b, 1e-3), 'approx';
