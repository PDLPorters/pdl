use strict;
use Test::More;
use PDL::LiteF;

sub tapprox ($$) {
    my ( $x, $y ) = @_;
    my $d = abs( $x - $y );
    my $check = ($d <= 0.0001);
    diag "diff = [$d]\n" unless $check;
    return $check;
}

# set up test arrays
#
my $x = pdl(0,0,6,3,5,0,7,14,94,5,5,8,7,7,1,6,7,13,10,2,101,19,7,7,5);  # sf.net bug #2019651
my $a_sort = $x->qsort;
my $y = pdl(55);
my $b_sort = $y->qsort;
my $c = cat($x,$x);
my $c_sort = $c->qsort;
my $d = sequence(10)->rotate(1);
my $d_sort = $d->qsort;
my $e = pdl([[1,2],[0,500],[2,3],[4,2],[3,4],[3,5]]);
my $e_sort = $e->qsortvec;

# Test a range of values
ok( tapprox($x->pctover(-0.5), $a_sort->at(0)), "pct below 0 for 25-elem pdl" );
ok( tapprox($x->pctover( 0.0), $a_sort->at(0)), "pct equal 0 for 25-elem pdl" );
ok( tapprox($x->pctover( 0.9),             17), "pct equal 0.9 for 25-elem pdl [SF bug 2019651]" );
ok( tapprox($x->pctover( 1.0), $a_sort->at($x->dim(0)-1)), "pct equal 1 for 25-elem pdl" );
ok( tapprox($x->pctover( 2.0), $a_sort->at($x->dim(0)-1)), "pct above 1 for 25-elem pdl" );

# test for sf.net bug report 2753869
#
$x = sequence(10);
ok( tapprox($x->pctover(0.2 ), 1.8 ), "20th percential of 10-elem ndarray [SF bug 2753869]");
ok( tapprox($x->pctover(0.23), 2.07), "23rd percential of 10-elem ndarray [SF bug 2753869]");

# test for sf.net bug report 2110074
#
ok( ( eval { pdl([])->qsorti }, $@ eq '' ), "qsorti coredump,[SF bug 2110074]");

# Test inplace sorting
$d->inplace->qsort;
ok(all($d == $d_sort), "inplace sorting");

# Test inplace sorting with bad values
$d->setbadat(3);
$d_sort = $d->qsort;
$d->inplace->qsort;
ok(all($d == $d_sort), "inplace sorting with bad values");

# Test inplace lexicographical sorting
$e->inplace->qsortvec;
ok(all($e == $e_sort), "inplace lexicographical sorting");

# Test inplace lexicographical sorting with bad values
$e->setbadat(1,3);
$e_sort = $e->qsortvec;
$e->inplace->qsortvec;
ok(all($e == $e_sort), "inplace lexicographical sorting with bad values");

# Test sf.net bug 379 "Passing qsort an extra argument causes a segfault"
# (also qsorti, qsortvec, qsortveci)
eval { random(15)->qsort(5); };
isnt($@, '', "qsort extra argument");
eval { random(15)->qsorti(5); };
isnt($@, '', "qsorti extra argument");
eval {random(10,4)->qsortvec(5); };
isnt($@, '', "qsortvec extra argument");
eval {random(10,4)->qsortveci(2); };
isnt($@, '', "qsortveci extra argument");
#but the dimension size checks for those cases shouldn't derail trivial qsorts:
is(pdl(5)->qsort,pdl(5),'trivial qsort');
is(pdl(8)->qsorti,pdl(0),'trivial qsorti');
ok(all(pdl(42,41)->qsortvec == pdl(42,41)->dummy(1)),'trivial qsortvec');
is(pdl(53,35)->qsortveci,pdl(0),'trivial qsortveci');


# test for sf.net bug report 3234141 "max() fails on nan"
#   NaN values are handled inconsistently by min, minimum, max, maximum...
#
{
 my $inf = inf();
 my $nan = nan();
 my $x = pdl($nan, 0, 1, 2);
 my $y = pdl(0, 1, 2, $nan);

 ok($x->min == $y->min, "min with NaNs");
 ok($x->max == $y->max, "max with NaNs");
}
my $empty = which(ones(5)>5);
$x = $empty->double->maximum;
ok( $x->nelem==1, "maximum over an empty dim yields 1 value");
ok(!($x*0==0), "max of empty nonbad float gives NaN");
$x = $empty->byte->maximum;
ok($x==0, "max of empty nonbad int type gives 0");

# test bad value handling with pctover and max
#
$empty->badflag(1);
$x = $empty->maximum;
ok( $x->isbad, "bad flag gets set on max over an empty dim");
my $xbad = $x;
$xbad->badflag(1);
$xbad->inplace->setvaltobad(7);
my $xgood = $xbad->where($xbad->isgood);
my $allbad = $xbad->where($xbad->isbad);
ok( $xbad->pctover(0.1) == $xgood->pctover(0.1), "pctover(0.1) badvals" );
ok( $xbad->pctover(0.9) == $xgood->pctover(0.9), "pctover(0.9) badvals" );
ok( $allbad->pctover(0.1)->isbad, "pctover(0.1) all badvals" );
ok( $allbad->pctover(0.9)->isbad, "pctover(0.9) all badvals" );

#Test subroutines directly.

#set up ndarrays
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
ok (PDL::oddmedian($g) ==  0, 'Oddmedian 2-value ndarray test');
ok (PDL::oddmedian($h) ==  0, 'Oddmedian 3-value not in order test');
ok (PDL::oddmedian($j) == -3, 'Oddmedian negative values even cardinality test');

#Test mode and modeover
$x = pdl([1,2,3,3,4,3,2],1);
ok( $x->mode == 0, "mode test" );
ok( all($x->modeover == pdl(3,0)), "modeover test");


#the next 4 tests address GitHub issue #248.

#   .... 0000 1010
#   .... 1111 1100
#OR:.... 1111 1110 = -2
is( pdl([10,0,-4])->borover(), -2, "borover with no BAD values");

#     .... 1111 1111
#     .... 1111 1010
#     .... 1111 1100
#AND: .... 1111 1000 = -8

is( pdl([-6,~0,-4])->bandover(), -8, "bandover with no BAD values");

#   0000 1010
#   1111 1100
#OR:1111 1110 = 254 if the accumulator in BadCode is an unsigned char
is( pdl([10,0,-4])->setvaltobad(0)->borover(), -2, "borover with BAD values");
#     1111 1010
#     1111 1100
#AND: 1111 1000 = 248 if the accumulator in BadCode is an unsigned char
is( pdl([-6,~0,-4])->setvaltobad(~0)->bandover(), -8, "bandover with BAD values");

{
  # all calls to functions that handle finding minimum and maximum should return
  # the same values (i.e., BAD).  NOTE: The problem is that perl scalar values
  # have no 'BAD' values while pdls do.  We need to sort out and document the
  # differences between routines that return perl scalars and those that return
  # pdls.
  my $bad_0dim = pdl(q|BAD|);
  is( "". $bad_0dim->min, 'BAD', "does min returns 'BAD'" );
  isnt( "". ($bad_0dim->minmax)[0], "". $bad_0dim->min, "does minmax return same as min" );
  is( "". ($bad_0dim->minmaximum)[0],  "". $bad_0dim->min, "does minmaximum return same as min" );
}

is ushort(65535)->max, 65535, 'max(highest ushort value) should not be BAD';

done_testing;
