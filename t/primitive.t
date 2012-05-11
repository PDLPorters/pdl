# -*-perl-*-
# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
use PDL::Types;

# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

use strict;
use Test::More;

plan tests => 48;

sub tapprox {
    my($a,$b) = @_;
    if((join ',',$a->dims) ne (join ',',$b->dims)) {
       diag "APPROX: $a $b\n";
       diag "UNEQDIM\n";
       return 0;
    }
    my $d = max( abs($a-$b) );
    if($d >= 0.01) {
       diag "APPROX: $a $b\n";
       diag "# APPROXFAIL: $a $b\n";
    }
    $d < 0.01;
}

my $a = PDL->pdl([[5,4,3],[2,3,1.5]]);

ok(tapprox($a->average(), PDL->pdl([4, 2.16666])), "average"); # 1
ok(tapprox($a->sumover(), PDL->pdl([12, 6.5])), "sumover");    # 2
ok(tapprox($a->prodover(), PDL->pdl([60, 9])), "prodover");    # 3

my $b = PDL->pdl(4,3,1,0,0,0,0,5,2,0,3,6);
# diag "B: $b\n";
my $c = ($b->xvals) + 10;
# diag "C: $c\n";

# diag "BW: ", $b->where, "\n";
ok(tapprox($b->where($b>4), PDL->pdl(5,6)), "where with >");   # 4
ok(tapprox($b->which, PDL->pdl(0,1,2,7,8,10,11)), "which");    # 5

# diag "B, ",$b->which();
# diag "C: $c\n";
# diag "\nCI, ", $c->index($b->which());
# diag "D\n";

ok(tapprox($c->where($b), PDL->pdl(10,11,12,17,18,20,21)), "where with mask");  # 6

##############################
# originally in pptest
$a = ones(byte,3000);
dsumover($a,($b=null));
is($b->get_datatype, $PDL_D, "get_datatype" );                 # 7
is($b->at, 3000, "at" );                                       # 8

my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5];
my $q = zeroes 5;
minimum_n_ind $p, $q;
ok(tapprox($q, pdl(0, 6, 7, 1, 9)), "minimum_n_ind");          # 9

##############################
# check that our random functions work with Perl's srand
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

##############################
# Test that whichND works OK
my $r = xvals(10,10)+10*yvals(10,10);
$a = whichND( $r % 12 == 0 );

# Nontrivial case gives correct coordinates
ok(eval 'sum($a != pdl([0,0],[2,1],[4,2],[6,3],[8,4],[0,6],[2,7],[4,8],[6,9]))==0', "whichND");  #12

# Empty case gives matching Empty
$a = whichND( $r*0 );
ok($a->nelem==0, "whichND( 0*\$r ) gives an Empty PDL");           #13
ok($a->ndims==2, "whichND( 0*\$r ) has 2 dims");                   #14
ok(($a->dim(0)==2 and $a->dim(1)==0), "whichND( 0*\$r ) is 2x0");  #15

# Scalar PDLs are treated as 1-PDLs
$a = whichND(pdl(5));
ok($a->nelem==1 && $a==0, "whichND");                             #16

# Scalar empty case returns a 1-D vector of size 0
$a = whichND(pdl(0));
ok($a->nelem==0,  "whichND of 0 scalar is empty");                 #17
ok($a->ndims==1,  "whichND of 0 scalar has 1 dim");                #18
ok($a->dim(0)==0, "whichND of 0 scalar: return 0 dim size is 0");  #19

# Empty case returns Empty
$b = whichND( which(pdl(0)) );                              
ok($b->nelem==0, "whichND of Empty mask");                         #20

# Nontrivial empty mask case returns matching Empty -- whichND(Empty[2x0x2]) should return Empty[3x0]
$b = whichND(zeroes(2,0,2));
ok(($b->ndims==2 and $b->dim(0)==3 and $b->dim(1)==0), "whichND(Empty[2x0x2]) returns Empty[3x0]"); # 21


##############################
# Simple test case for interpND
my $index;
my $z;
$a = xvals(10,10)+yvals(10,10)*10;
$index = cat(3+xvals(5,5)*0.25,7+yvals(5,5)*0.25)->reorder(2,0,1);
$z = 73+xvals(5,5)*0.25+2.5*yvals(5,5);
eval '$b = $a->interpND($index);';
ok(!$@);                                                       #16
ok(sum($b != $z) == 0, "interpND");                            #17

##############################
# Test glue
$a = xvals(2,2,2);
$b = yvals(2,2,2);
$c = zvals(2,2,2);
our $d;
eval '$d = $a->glue(1,$b,$c);';
ok(!$@);                                                              #18
ok(zcheck($d - pdl([[0,1],[0,1],[0,0],[1,1],[0,0],[0,0]],
                   [[0,1],[0,1],[0,0],[1,1],[1,1],[1,1]])), "glue");  #19



##############################
# test new empty piddle handling
$a = which ones(4) > 2;
$b = $a->long;
$c = $a->double;

ok(isempty $a, "isempty");                                     #20
ok($b->avg == 0, "avg of Empty");                              #21
ok(! any isfinite $c->average, "isfinite of Empty");           #22

##############################
# Test uniqvec
$a = pdl([[0,1],[2,2],[0,1]]);
$b = $a->uniqvec;
eval '$c = all($b==pdl([[0,1],[2,2]]))';
ok(!$@ && $c && $b->ndims==2, "uniqvec");                      #23

$a = pdl([[0,1]])->uniqvec;
eval '$c = all($a==pdl([[0,1]]))';
ok(!$@ && $c && $a->ndims==2, "uniqvec");                      #24

$a = pdl([[0,1,2]]); $a = $a->glue(1,$a,$a);
$b = $a->uniqvec;
eval '$c = all($b==pdl([0,1,2]))';
ok(!$@ && $c && $b->ndims==2, "uniqvec");                      #25

##############################
# Test bad handling in selector
SKIP: {
   skip "Bad handling not available", 3 unless $PDL::Bad::Status;

   $b = xvals(3);
   ok(tapprox($b->which,PDL->pdl(1,2)), "which");              #26
   setbadat $b, 1;
   ok(tapprox($b->which,PDL->pdl([2])), "which w BAD");        #27
   setbadat $b, 0;
   setbadat $b, 2;
   is($b->which->nelem,0, "which nelem w BAD");                #28
}

############################
# Test intersect & setops
my $x = sequence(10);
$a = which(($x % 2) == 0);
$b = which(($x % 3) == 0);
$c = setops($a, 'AND', $b);
ok(tapprox($c, pdl([0, 6])), "setops AND");                    #29
$c = setops($a,'OR',$b);
ok(tapprox($c, pdl([0,2,3,4,6,8,9])), "setops OR");            #30
$c = setops($a,'XOR',$b);
ok(tapprox($c, pdl([2,3,4,8,9])), "setops XOR");               #31


##############################
# Test uniqind
$a = pdl([0,1,2,2,0,1]);
$b = $a->uniqind;
eval '$c = all($b==pdl([0,1,3]))';
ok(!$@ && $c && $b->ndims==1, "uniqind");                      #32

$b = pdl(1,1,1,1,1)->uniqind;         # SF bug 3076570
ok(! $b->isempty);                                             #33
eval '$c = all($b==pdl([0]))';
ok(!$@ && $c && $b->ndims==1, "uniqind, SF bug 3076570");      #34

##############################
# Test whereND
SKIP: {
   skip "have no whereND", 8 unless defined(&PDL::whereND);

   $a = sequence(4,3,2);
   $b = pdl(0,1,1,0);
   $c = whereND($a,$b);
   ok(all(pdl($c->dims)==pdl(2,3,2))) and                      #35
   ok(all($c==pdl q[ [ [ 1  2] [ 5  6] [ 9 10] ]
                     [ [13 14] [17 18] [21 22] ] ]),
                                     "whereND [4]");           #36

   $b = pdl q[ 0 0 1 1 ; 0 1 0 0 ; 1 0 0 0 ];
   $c = whereND($a,$b);
   ok(all(pdl($c->dims)==pdl(4,2))) and                        #37
   ok(all($c==pdl q[ 2  3  5  8 ; 14 15 17 20 ]),
                                "whereND [4,3]");              #38

   $b = (random($a)<0.3);
   $c = whereND($a,$b);
   ok(all($c==where($a,$b)), "whereND vs where");              #39

   # sf.net bug #3415115, whereND fails to handle all zero mask case
   $b = zeros(4);
   $c = whereND($a,$b);
   ok($c->isempty, 'whereND of all-zeros mask');               #40
   
   # Make sure whereND functions as an lvalue:
   $a = sequence(4,3);
   $b = pdl(0, 1, 1, 1);

   eval q{
   	  $a->whereND($b) *= -1;
   };
   is($@, '', 'using whereND in lvalue context does not croak');
                                                               #41
   ok(all($a->slice("1:-1") < 0), 'whereND in lvalue context works');
                                                               #42
}
