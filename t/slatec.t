use PDL::LiteF;
BEGIN {
	eval " use PDL::Slatec; ";
	$loaded = ($@ ? 0 : 1);
}

# Test counters [should 'use Test' but this was not a standard
# part of perl until 5.005]
my $ntests = 18;
my $n = 0;

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	$n++; # increment test counter
	my $result = shift ;
	print "not " unless $result ;
	print "ok $n\n" ;

}

sub approx {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
#	print "APR: $a,$b,$c,$d;\n";
	$d < 0.001;
}


print "1..$ntests\n";
unless ($loaded) {
	#print STDERR "PDL::Slatec not installed. All tests are skipped.\n";
	for (1..$ntests) {
		print "ok $_ # Skipped: PDL::Slatec not available.\n";
	}
	exit;
}

my $mat = pdl [1,0.1],[0.1,2];

($eigvals,$eigvecs) = eigsys($mat);

print $eigvecs,$eigvals,"\n";

ok(approx($eigvals,pdl(0.9901,2.009)));
ok(!approx($eigvals,pdl(0.99,2.5)));

ok(approx($eigvecs,pdl([0.995,-0.0985],[0.0985,0.995])));

$mat = pdl [2,3],[4,5];

$inv = matinv($mat);

inner($mat->dummy(2),$inv->xchg(0,1)->dummy(1),($uni=null));

print $mat;
print $inv;

print $uni;

ok(approx($uni,pdl[1,0],[0,1]));

$det = $mat->det;
$det->dump;;
$deti = $inv->det;
$deti->dump;;

ok(approx($det,-2));
ok(approx($deti,-0.5));

# Now do the polynomial fitting tests

# Set up tests x, y and weight 
my $y = pdl (1,4,9,16,25,36,49,64.35,32);
my $x = pdl ( 1,2,3,4,5,6,7,8,9);
my $w = pdl ( 1,1,1,1,1,1,1,0.5,0.3);

# input parameters
my $eps = pdl(0);
my $maxdeg = 7;

# Do the fit
my ($ndeg, $r, $ierr, $a) = polyfit($x, $y, $w, $maxdeg, $eps);

ok(($ierr == 1));

print "NDEG, EPS, IERR: $ndeg, $eps, $ierr\n";

# Test POLYCOEF

my $c = pdl(4);           # Expand about x = 4;

my $tc = polycoef($ndeg, $c, $a);

my @tc = $tc->list;
my @r  = $r->list;
my $i = 0;

foreach my $xpos ($x->list) {
  my $ypos = 0;
  my $n = 0;
  foreach my $bit ($tc->list) {
    $ypos += $bit * ($xpos- (($c->list)[0]))**$n;
    $n++;
  }
  print "$xpos, $ypos, $r[$i]\n";

  # Compare with answers from polyfit
  ok(sprintf("%5.2f", $ypos) == sprintf("%5.2f", $r[$i]));
  $i++;
}

# Try polyvalue with a single x pos
my $xx = pdl([4]);
my $nder = 3;

my ($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a);

print "At $xx, $yfit and $yp\n";
ok(int($yp->at(0)) == 8); 

# Test polyvalue
$nder = 3;
$xx    = pdl(12,4,6.25,1.5); # Ask for multiple positions at once

($yfit, $yp) = polyvalue($ndeg, $nder, $xx, $a);

print "At $xx is $yfit and $yp\n";

# Simple test of expected value
ok(int($yfit->at(1)) == 15);
