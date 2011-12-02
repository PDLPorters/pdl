#!/usr/bin/perl
#
# Test some Basic/Ufunc routines
no warnings qw(misc);

use strict;
use Test::More tests => 15;

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
    print "diff = [$d]\n";
    return $d <= 0.0001;
}

# set up test arrays
#
my $a = pdl(0,0,6,3,5,0,7,14,94,5,5,8,7,7,1,6,7,13,10,2,101,19,7,7,5);  # sf.net bug #2019651
my $a_sort = $a->qsort;
my $b = pdl(55);
my $b_sort = $b->qsort;
my $c = cat($a,$a);
my $c_sort = $c->qsort;

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

# test bad value handling with pctover
#
SKIP: {
   skip "Bad value support not compiled", 4 unless $PDL::Bad::Status;

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

# test for sf.net but report 3234141 "max() fails on nan"
#   NaN values are handled inconsistently by min, minimum, max, maximum...
#
TODO: {
   local $TODO = "fixing max/min NaN handling";

   my $inf = exp(~0>>1);
   my $nan = $inf/$inf;
   my $a = pdl($nan, 0, 1, 2);
   my $b = pdl(0, 1, 2, $nan);

   ok($a->min == $b->min, "min with NaNs");
   ok($a->max == $b->max, "max with NaNs");
}
