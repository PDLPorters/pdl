use Test::More tests => 5;
use PDL::LiteF;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

$a = zeroes 1,1,1;
isnt $a, undef;

$a = ones 3;
eval {print "oops\n" if $a};
like $@, qr/multielement/;

ok all $a;

$a = pdl byte, [ 0, 0, 1 ];
ok any $a > 0;

$a = ones 3;
$b = $a + 1e-4;
ok all PDL::approx $a, $b, 1e-3;
