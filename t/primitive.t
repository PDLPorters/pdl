# -*-perl-*-
# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
use PDL::Types;

# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

use strict;
use Test;

plan tests => $PDL::Bad::Status ? 34 : 31 ;

sub tapprox {
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

ok(tapprox($a->average(), PDL->pdl([4, 2.16666]))); # 1
ok(tapprox($a->sumover(), PDL->pdl([12, 6.5])));    # 2
ok(tapprox($a->prodover(), PDL->pdl([60, 9])));     # 3

my $b = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
print "B: $b\n";
my $c = ($b->xvals) + 10;
# print "C: $c\n";

# print "BW: ", $b->where, "\n";
ok(tapprox($b->where($b>4), PDL->pdl(5,6)));        # 4
ok(tapprox($b->which, PDL->pdl(0,1,2,7,8,10,11)));  # 5

# print "B, ",$b->which();
# print "C: $c\n";
# print "\nCI, ", $c->index($b->which());
# print "D\n";

ok(tapprox($c->where($b), PDL->pdl(10,11,12,17,18,20,21))); 
                                                    # 6

# originally in pptest
$a = ones(byte,3000);
dsumover($a,($b=null));
ok($b->get_datatype, $PDL_D );                      # 7
ok($b->at, 3000 );                                  # 8

my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5];
my $q = zeroes 5;
minimum_n_ind $p, $q;
ok(tapprox $q, pdl(0, 6, 7, 1, 9));                 #9

# check that our random functions work with Perl's srand
srand 5;
my $r1 = random 10;
srand 5;
my $r2 = random 10;
ok(tapprox $r1, $r2);                               #10

srand 10;
$r1 = grandom 10;
srand 10;
$r2 = grandom 10;
ok(tapprox $r1, $r2);                               #11

##############################
# Test that whichND works OK...
my $r = xvals(10,10)+10*yvals(10,10);
$a = whichND( $r % 12 == 0 );

ok(eval 'sum($a != pdl([0,0],[2,1],[4,2],[6,3],[8,4],[0,6],[2,7],[4,8],[6,9]))==0'); 

$a = whichND(pdl(5));
ok($a->nelem==1 && $a==0);

$a = whichND(pdl(0));
ok($a->nelem==0);

$a = whichND( which(pdl(0)) );
ok($a->nelem==0);

                                                    #12

##############################
# Simple test case for interpND...
my $index;
my $z;
$a = xvals(10,10)+yvals(10,10)*10;
$index = cat(3+xvals(5,5)*0.25,7+yvals(5,5)*0.25)->reorder(2,0,1);
$z = 73+xvals(5,5)*0.25+2.5*yvals(5,5);
eval '$b = $a->interpND($index);';
ok(!$@);                                            #13
ok(sum($b != $z) == 0);                             #14

##############################
# Test glue...
$a = xvals(2,2,2);
$b = yvals(2,2,2);
$c = zvals(2,2,2);
our $d;
eval '$d = $a->glue(1,$b,$c);';
ok(!$@);                                            #15
ok(zcheck($d - pdl([[0,1],[0,1],[0,0],[1,1],[0,0],[0,0]],
                   [[0,1],[0,1],[0,0],[1,1],[1,1],[1,1]])));
                                                    #16


# test new empty piddle handling
$a = which ones(4) > 2;
$b = $a->long;
$c = $a->double;

ok(isempty $a);                                     #17
ok($b->avg == 0);                                   #18
ok(! any isfinite $c->average);                     #19

##############################
# Test uniqvec...
$a = pdl([[0,1],[2,2],[0,1]]);
$b = $a->uniqvec;
eval '$c = all($b==pdl([[0,1],[2,2]]))';  ok(!$@ && $c && $b->ndims==2); #20

$a = pdl([[0,1]])->uniqvec;
eval '$c = all($a==pdl([[0,1]]))';  ok(!$@ && $c && $a->ndims==2);  #21

$a = pdl([[0,1,2]]); $a = $a->glue(1,$a,$a);
$b = $a->uniqvec;
eval '$c = all($b==pdl([0,1,2]))';  ok(!$@ && $c && $b->ndims==2);  #22

##############################
# Test bad handling in selector
if($PDL::Bad::Status) {
  $b = xvals(3);
  ok(tapprox($b->which,PDL->pdl(1,2)));             #23.BAD
  setbadat $b, 1;
  ok(tapprox($b->which,PDL->pdl([2])));             #24.BAD
  setbadat $b, 0;
  setbadat $b, 2;
  ok($b->which->nelem,0);                           #25.BAD
}

############################
# Test intersect & setops
my $x = sequence(10);
$a = which(($x % 2) == 0);
$b = which(($x % 3) == 0);
$c = setops($a, 'AND', $b);
ok(tapprox($c, pdl([0, 6])));                       #26
$c = setops($a,'OR',$b);
ok(tapprox($c, pdl([0,2,3,4,6,8,9])));              #27
$c = setops($a,'XOR',$b);
ok(tapprox($c, pdl([2,3,4,8,9])));                  #28


##############################
# Test uniqind...
$a = pdl([0,1,2,2,0,1]);
$b = $a->uniqind;
eval '$c = all($b==pdl([0,1,3]))';  ok(!$@ && $c && $b->ndims==1); #29

$b = pdl(1,1,1,1,1)->uniqind;       # SF.net bug 3076570 
ok(! $b->isempty);                                                 #30
eval '$c = all($b==pdl([0]))';  ok(!$@ && $c && $b->ndims==1);     #31
