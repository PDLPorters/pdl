use PDL::LiteF;
sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

$SIG{BUS} = \&not_ok;
$SIG{SEGV} = \&not_ok;


sub not_ok {
	print STDERR "\ngot fatal signal\n";
	print "not ok ".$::i."\n";
	exit;
}

print "1..4\n";


# PDL::Core::set_debugging(1);
$b = pdl [[1,1,1],[2,2,2]];

# we are using more dims than are available
eval {$c = $b->slice(':,:,:,(1)'); $c->make_physical();};
print "ERROR WAS: '$@'\n";
ok(1,$@ =~ /error/i);

$::i = 2;
# now see if we survive the destruction of this invalid trans
$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");
ok(2,1);  # if we're here we survived

$b = pdl [[1,1,1],[2,2,2]];
eval {$c = $b->clump(3); $c->make_physical();};
print "ERROR WAS: '$@'\n";
ok(3,$@ =~ /error/i);

$::i = 4;

$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");
ok(4,1);  # if we're here we survived
