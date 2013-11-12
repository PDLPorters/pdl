#!/usr/bin/perl
#
# Test some Basic/Ufunc routines

use strict;
use Test::More tests => 35;

BEGIN {
    # if we've got this far in the tests then 
    # we can probably assume PDL::LiteF works!
    #
    use_ok( "PDL::LiteF" );
}
$| = 1;

sub tapprox ($$) {
    my ( $a, $b ) = @_;
    my $d = abs( $a - $b );
    my $check = ($d <= 0.0001);
    diag "diff = [$d]\n" unless $check;
    return $check;
}

# set up test arrays
#
my $a = pdl(0,0,6,3,5,0,7,14,94,5,5,8,7,7,1,6,7,13,10,2,101,19,7,7,5);  # sf.net bug #2019651
my $a_sort = $a->qsort;
my $b = pdl(55);
my $b_sort = $b->qsort;
my $c = cat($a,$a);
my $c_sort = $c->qsort;
my $d = sequence(10)->rotate(1);
my $d_sort = $d->qsort;
my $e = pdl([[1,2],[0,500],[2,3],[4,2],[3,4],[3,5]]);
my $e_sort = $e->qsortvec;

# Test a range of values
ok( tapprox($a->pctover(-0.5), $a_sort->at(0)), "pct below 0 for 25-elem pdl" );
ok( tapprox($a->pctover( 0.0), $a_sort->at(0)), "pct equal 0 for 25-elem pdl" );
ok( tapprox($a->pctover( 0.9),             17), "pct equal 0.9 for 25-elem pdl [SF bug 2019651]" );
ok( tapprox($a->pctover( 1.0), $a_sort->at($a->dim(0)-1)), "pct equal 1 for 25-elem pdl" );
ok( tapprox($a->pctover( 2.0), $a_sort->at($a->dim(0)-1)), "pct above 1 for 25-elem pdl" );

# test for sf.net bug report 2753869
#
my $x = sequence(10);
ok( tapprox($x->pctover(0.2 ), 1.8 ), "20th percential of 10-elem piddle [SF bug 2753869]");
ok( tapprox($x->pctover(0.23), 2.07), "23rd percential of 10-elem piddle [SF bug 2753869]");

# test for sf.net bug report 2110074
#
ok( ( eval { pdl([])->qsorti }, $@ eq '' ), "qsorti coredump,[SF bug 2110074]");

# Test inplace sorting
$d->inplace->qsort;
ok(all($d == $d_sort));

# Test inplace sorting with bad values
$d->setbadat(3);
$d_sort = $d->qsort;
$d->inplace->qsort;
ok(all($d == $d_sort));

# Test inplace lexicographical sorting
$e->inplace->qsortvec;
ok(all($e == $e_sort));

# Test inplace lexicographical sorting with bad values
$e->setbadat(1,3);
$e_sort = $e->qsortvec;
$e->inplace->qsortvec;
ok(all($e == $e_sort));

# test for sf.net but report 3234141 "max() fails on nan"
#   NaN values are handled inconsistently by min, minimum, max, maximum...
#
local $TODO = "fixing max/min NaN handling";

my $inf = exp(~0>>1);
my $nan = $inf/$inf;
my $a = pdl($nan, 0, 1, 2);
my $b = pdl(0, 1, 2, $nan);

ok($a->min == $b->min, "min with NaNs");
ok($a->max == $b->max, "max with NaNs");

my $empty = which(ones(5)>5);
$a = $empty->double->maximum;
ok( $a->nelem==1, "maximum over an empty dim yields 1 value");
ok(!($a*0==0), "max of empty nonbad float gives NaN");
$a = $empty->byte->maximum;
ok($a==0, "max of empty nonbad int type gives 0");

# test bad value handling with pctover and max
#
SKIP: {
   skip "Bad value support not compiled", 5 unless $PDL::Bad::Status;

   $empty->badflag(1);
   $a = $empty->maximum;
   ok( $a->isbad, "bad flag gets set on max over an empty dim");

   my $abad = $a;
   $abad->badflag(1);
   $abad->inplace->setvaltobad(7);
   my $agood = $abad->where($abad->isgood);
   my $allbad = $abad->where($abad->isbad);

   ok( $abad->pctover(0.1) == $agood->pctover(0.1), "pctover(0.1) badvals" );
   ok( $abad->pctover(0.9) == $agood->pctover(0.9), "pctover(0.9) badvals" );
   ok( $allbad->pctover(0.1)->isbad, "pctover(0.1) all badvals" );
   ok( $allbad->pctover(0.9)->isbad, "pctover(0.9) all badvals" );
};


#Test subroutines directly.

#set up piddles
my $f=pdl(1,2,3,4,5);
my $g=pdl (0,1);
my $h=pdl(1, 0,-1);
my $i=pdl (1,0);
my $j=pdl(-3, 3, -5, 10);

#Test percentile routines
#Test PDL::pct
ok (tapprox(PDL::pct($f, .5),     3), 'PDL::pct 50th percentile');
ok (tapprox(PDL::pct($g, .76), 0.76), 'PDL::pct interpolation test');
ok (tapprox(PDL::pct($i, .76), 0.76), 'PDL::pct interpolation not in order test');

#Test PDL::oddpct
ok (tapprox(PDL::oddpct($f, .5),  3), 'PDL::oddpct 50th percentile');
ok (tapprox(PDL::oddpct($f, .79), 4), 'PDL::oddpct intermediate value test');
ok (tapprox(PDL::oddpct($h, .5),  0), 'PDL::oddpct 3-member 50th percentile with negative value');
ok (tapprox(PDL::oddpct($j, .1), -5), 'PDL::oddpct negative values in-between test');

#Test oddmedian
ok (PDL::oddmedian($g) ==  0, 'Oddmedian 2-value piddle test');
ok (PDL::oddmedian($h) ==  0, 'Oddmedian 3-value not in order test');
ok (PDL::oddmedian($j) == -3, 'Oddmedian negative values even cardinality test');

#Test mode and modeover
my $a = pdl([1,2,3,3,4,3,2],1);
ok( $a->mode == 0, "mode test" );
ok( all($a->modeover == pdl(3,0)), "modeover test");


