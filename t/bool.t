use PDL::LiteF;

$|=1;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub pok { print "ok $_[0]\n" }

print "1..5\n";

$a = zeroes 1,1,1;
if ($a) { print "not " }
pok 1;

$a = ones 3;
eval {print "oops\n" if $a};
print "ERROR WAS: '$@'\n";
ok(2,$@ =~ /multielement/);

unless (all $a) { print "not " };
pok 3;

$a = pdl byte, [ 0, 0, 1 ];
unless (any $a > 0) { print "not " };
pok 4;

$a = ones 3;
$b = $a + 1e-4;
ok(5, all PDL::approx $a, $b, 1e-3);
