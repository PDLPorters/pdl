# -*-perl-*-
# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
use PDL::Types;

# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

use strict;
use Test::More;

plan tests => 58;

sub tapprox {
    my($x,$y) = @_;
    if((join ',',$x->dims) ne (join ',',$y->dims)) {
       diag "APPROX: $x $y\n";
       diag "UNEQDIM\n";
       return 0;
    }
    my $d = max( abs($x-$y) );
    if($d >= 0.01) {
       diag "APPROX: $x $y\n";
       diag "# APPROXFAIL: $x $y\n";
    }
    $d < 0.01;
}

my $x = PDL->pdl([[5,4,3],[2,3,1.5]]);

ok(tapprox($x->average(), PDL->pdl([4, 2.16666])), "average"); # 1
ok(tapprox($x->sumover(), PDL->pdl([12, 6.5])), "sumover");    # 2
ok(tapprox($x->prodover(), PDL->pdl([60, 9])), "prodover");    # 3

my $y = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
# diag "Y: $y\n";
my $c = ($y->xvals) + 10;
# diag "C: $c\n";

# diag "YW: ", $y->where, "\n";
ok(tapprox($y->where($y>4), PDL->pdl(5,6)), "where with >");   # 4
ok(tapprox($y->which, PDL->pdl(0,1,2,7,8,10,11)), "which");    # 5

# diag "Y, ",$y->which();
# diag "C: $c\n";
# diag "\nCI, ", $c->index($y->which());
# diag "D\n";

ok(tapprox($c->where($y), PDL->pdl(10,11,12,17,18,20,21)), "where with mask");  # 6

##############################
# originally in pptest
$x = ones(byte,3000);
dsumover($x,($y=null));
is($y->get_datatype, $PDL_D, "get_datatype" );                 # 7
is($y->at, 3000, "at" );                                       # 8

my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5];
my $q = zeroes 5;
minimum_n_ind $p, $q;
ok(tapprox($q, pdl(0, 6, 7, 1, 9)), "minimum_n_ind");          # 9

##############################
# check that our random functions work with Perl's srand
TODO: {
   local $TODO = 'Some CPAN Testers fails for OpenBSD';

   srand 5;
   my $r1 = random 10;
   srand 5;
   my $r2 = random 10;
   ok(tapprox($r1, $r2), "random and srand");                     #10

   srand 10;
   $r1 = grandom 10;
   srand 10;
   $r2 = grandom 10;
   ok(tapprox($r1, $r2), "grandom and srand");                    #11
}
##############################
# Test that whichND works OK
my $r = xvals(10,10)+10*yvals(10,10);
$x = whichND( $r % 12 == 0 );

# Nontrivial case gives correct coordinates
ok(eval 'sum($x != pdl([0,0],[2,1],[4,2],[6,3],[8,4],[0,6],[2,7],[4,8],[6,9]))==0', "whichND");  #12
ok($x->type eq 'indx', "whichND returns indx-type piddle for non-trivial case");
# Empty case gives matching Empty
$x = whichND( $r*0 );
ok($x->nelem==0, "whichND( 0*\$r ) gives an Empty PDL");           #13
ok($x->ndims==2, "whichND( 0*\$r ) has 2 dims");                   #14
ok(($x->dim(0)==2 and $x->dim(1)==0), "whichND( 0*\$r ) is 2x0");  #15
ok($x->type eq 'indx', "whichND( 0*\$r) type is indx");

# Scalar PDLs are treated as 1-PDLs
$x = whichND(pdl(5));
ok($x->nelem==1 && $x==0, "whichND scalar PDL");                             #16
ok($x->type eq 'indx', "whichND returns indx-type piddle for scalar piddle mask");

# Scalar empty case returns a 1-D vector of size 0
$x = whichND(pdl(0));
ok($x->nelem==0,  "whichND of 0 scalar is empty");                 #17
ok($x->ndims==1,  "whichND of 0 scalar has 1 dim");                #18
ok($x->dim(0)==0, "whichND of 0 scalar: return 0 dim size is 0");  #19
ok($x->type eq 'indx', "whichND returns indx-type piddle for scalar empty case");

# Empty case returns Empty
$y = whichND( which(pdl(0)) );                              
ok($y->nelem==0, "whichND of Empty mask");                         #20
ok($y->type eq 'indx', "whichND returns indx-type piddle for empty case");

# Nontrivial empty mask case returns matching Empty -- whichND(Empty[2x0x2]) should return Empty[3x0]
$y = whichND(zeroes(2,0,2));
ok(($y->ndims==2 and $y->dim(0)==3 and $y->dim(1)==0), "whichND(Empty[2x0x2]) returns Empty[3x0]"); # 21


##############################
# Simple test case for interpND
my $index;
my $z;
$x = xvals(10,10)+yvals(10,10)*10;
$index = cat(3+xvals(5,5)*0.25,7+yvals(5,5)*0.25)->reorder(2,0,1);
$z = 73+xvals(5,5)*0.25+2.5*yvals(5,5);
eval '$y = $x->interpND($index);';
ok(!$@);                                                       #16
ok(sum($y != $z) == 0, "interpND");                            #17

##############################
# Test glue
$x = xvals(2,2,2);
$y = yvals(2,2,2);
$c = zvals(2,2,2);
our $d;
eval '$d = $x->glue(1,$y,$c);';
ok(!$@);                                                              #18
ok(zcheck($d - pdl([[0,1],[0,1],[0,0],[1,1],[0,0],[0,0]],
                   [[0,1],[0,1],[0,0],[1,1],[1,1],[1,1]])), "glue");  #19



##############################
# test new empty piddle handling
$x = which ones(4) > 2;
$y = $x->long;
$c = $x->double;

ok(isempty $x, "isempty");                                     #20
ok($y->avg == 0, "avg of Empty");                              #21
ok(! any isfinite $c->average, "isfinite of Empty");           #22

##############################
# Test uniqvec
$x = pdl([[0,1],[2,2],[0,1]]);
$y = $x->uniqvec;
eval '$c = all($y==pdl([[0,1],[2,2]]))';
ok(!$@ && $c && $y->ndims==2, "uniqvec");                      #23

$x = pdl([[0,1]])->uniqvec;
eval '$c = all($x==pdl([[0,1]]))';
ok(!$@ && $c && $x->ndims==2, "uniqvec");                      #24

$x = pdl([[0,1,2]]); $x = $x->glue(1,$x,$x);
$y = $x->uniqvec;
eval '$c = all($y==pdl([0,1,2]))';
ok(!$@ && $c && $y->ndims==2, "uniqvec");                      #25

##############################
# Test bad handling in selector
SKIP: {
   skip "Bad handling not available", 3 unless $PDL::Bad::Status;

   $y = xvals(3);
   ok(tapprox($y->which,PDL->pdl(1,2)), "which");              #26
   setbadat $y, 1;
   ok(tapprox($y->which,PDL->pdl([2])), "which w BAD");        #27
   setbadat $y, 0;
   setbadat $y, 2;
   is($y->which->nelem,0, "which nelem w BAD");                #28
}

############################
# Test intersect & setops
my $temp = sequence(10);
$x = which(($temp % 2) == 0);
$y = which(($temp % 3) == 0);
$c = setops($x, 'AND', $y);
ok(tapprox($c, pdl([0, 6])), "setops AND");                    #29
ok(tapprox(intersect($x,$y),pdl([0,6])), "intersect same as setops AND");
$c = setops($x,'OR',$y);
ok(tapprox($c, pdl([0,2,3,4,6,8,9])), "setops OR");            #30
$c = setops($x,'XOR',$y);
ok(tapprox($c, pdl([2,3,4,8,9])), "setops XOR");               #31
#Test intersect again
my $intersect_test=intersect(pdl(1,-5,4,0), pdl(0,3,-5,2));
ok (all($intersect_test==pdl(-5,0)), 'Intersect test values');

##############################
# Test uniqind
$x = pdl([0,1,2,2,0,1]);
$y = $x->uniqind;
eval '$c = all($y==pdl([0,1,3]))';
ok(!$@ && $c && $y->ndims==1, "uniqind");                      #32

$y = pdl(1,1,1,1,1)->uniqind;         # SF bug 3076570
ok(! $y->isempty);                                             #33
eval '$c = all($y==pdl([0]))';
ok(!$@ && $c && $y->ndims==1, "uniqind, SF bug 3076570");      #34

##############################
# Test whereND
SKIP: {
   skip "have no whereND", 8 unless defined(&PDL::whereND);

   $x = sequence(4,3,2);
   $y = pdl(0,1,1,0);
   $c = whereND($x,$y);
   ok(all(pdl($c->dims)==pdl(2,3,2))) and                      #35
   ok(all($c==pdl q[ [ [ 1  2] [ 5  6] [ 9 10] ]
                     [ [13 14] [17 18] [21 22] ] ]),
                                     "whereND [4]");           #36

   $y = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];
   $c = whereND($x,$y);
   ok(all(pdl($c->dims)==pdl(4,2))) and                        #37
   ok(all($c==pdl q[ 2  3  5  8 ; 14 15 17 20 ]),
                                "whereND [4,3]");              #38

   $y = (random($x)<0.3);
   $c = whereND($x,$y);
   ok(all($c==where($x,$y)), "whereND vs where");              #39

   # sf.net bug #3415115, whereND fails to handle all zero mask case
   $y = zeros(4);
   $c = whereND($x,$y);
   ok($c->isempty, 'whereND of all-zeros mask');               #40
   
   # Make sure whereND functions as an lvalue:
   $x = sequence(4,3);
   $y = pdl(0, 1, 1, 1);

   eval q{
   	  $x->whereND($y) *= -1;
   };
   is($@, '', 'using whereND in lvalue context does not croak');
                                                               #41
   ok(all($x->slice("1:-1") < 0), 'whereND in lvalue context works');
                                                               #42
}

#Test fibonacci.
my $fib=fibonacci(15);
my $fib_ans = pdl(1,1,2,3,5,8,13,21,34,55,89,144,233,377,610);
ok(all($fib == $fib_ans), 'Fibonacci sequence');

#Test which_both.
my $which_both_test=pdl(1,4,-2,0,5,0,1);
my ($nonzero,$zero)=which_both($which_both_test);
ok(all($nonzero==pdl(0,1,2,4,6)), 'Which_both nonzero indices');
ok(all($zero==pdl(3,5)), 'Which_both zero indices');
