use PDL::LiteF;
BEGIN {
	eval " use PDL::Slatec; ";
	$loaded = ($@ ? 0 : 1);
}

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
#	print "APR: $a,$b,$c,$d;\n";
	$d < 0.001;
}

print "1..6\n";
unless ($loaded) {
	#print STDERR "PDL::Slatec not installed. All tests are skipped.\n";
	for (1..6) {
		print "ok $_ # Skipped: PDL::Slatec not availalbe.\n";
	}
	exit;
}

my $mat = pdl [1,0.1],[0.1,2];

($eigvals,$eigvecs) = eigsys($mat);

print $eigvecs,$eigvals,"\n";

ok(1,approx($eigvals,pdl(0.9901,2.009)));
ok(2,!approx($eigvals,pdl(0.99,2.5)));

ok(3,approx($eigvecs,pdl([0.995,-0.0985],[0.0985,0.995])));

$mat = pdl [2,3],[4,5];

$inv = matinv($mat);

inner($mat->dummy(2),$inv->xchg(0,1)->dummy(1),($uni=null));

print $mat;
print $inv;

print $uni;

ok(4,approx($uni,pdl[1,0],[0,1]));

$det = $mat->det;
$det->dump;;
$deti = $inv->det;
$deti->dump;;

ok(5,approx($det,-2));
ok(6,approx($deti,-0.5));
