# -*-perl-*-
# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
use PDL::Types;

# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

use strict;
use Test;

plan tests => 8;

sub approx {
    my($a,$b) = @_;
    print "APPROX: $a $b\n";
    if((join ',',$a->dims) ne (join ',',$b->dims)) {
	print "UNEQDIM\n";
	return 0;
    }
    my $d = max( abs($a-$b) );
    if($d >= 0.01) {
	print "# APPROXFAIL: $a $b\n";
    }
    $d < 0.01;
}

my $a = PDL->pdl([[5,4,3],[2,3,1.5]]);

ok(approx($a->average(), PDL->pdl([4, 2.16666]))); # 1
ok(approx($a->sumover(), PDL->pdl([12, 6.5])));    # 2
ok(approx($a->prodover(), PDL->pdl([60, 9])));     # 3

my $b = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
print "B: $b\n";
my $c = ($b->xvals) + 10;
# print "C: $c\n";

# print "BW: ", $b->where, "\n";
ok(approx($b->where($b>4), PDL->pdl(5,6)));        # 4
ok(approx($b->which, PDL->pdl(0,1,2,7,8,10,11)));  # 5

# print "B, ",$b->which();
# print "C: $c\n";
# print "\nCI, ", $c->index($b->which());
# print "D\n";

ok(approx($c->where($b), PDL->pdl(10,11,12,17,18,20,21))); # 6

# originally in pptest
$a = ones(byte,3000);
dsumover($a,($b=null));
ok( $b->get_datatype, $PDL_D );   # 7
ok( $b->at, 3000 );               # 8
