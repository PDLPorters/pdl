use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Types;
use Test::PDL;

my $p = pdl([]); $p->setdims([1,0]); $p->qsortvec; # shouldn't segfault!
my $p2d  = pdl([[1,2],[3,4],[1,3],[1,2],[3,3]]);
is $p2d->dice_axis(1,$p2d->qsortveci).'', $p2d->qsortvec.'', "qsortveci";

my $ind_double = zeroes($p2d->dim(1));
$p2d->qsortveci($ind_double); # shouldn't segfault!
is $ind_double.'', '[3 0 2 4 1]';

eval { empty()->medover }; # shouldn't segfault
isnt $@, '', 'exception for percentile on empty ndarray';

eval { sequence(3, 3)->medover(my $o = null, my $t = null); };
isnt $@, '', 'a [t] Par cannot be passed';

my $med_dim = 5;
is_pdl sequence(10,$med_dim,$med_dim)->medover, sequence($med_dim,$med_dim)*10+4.5, 'medover';

my $x = pdl(0,0,6,3,5,0,7,14,94,5,5,8,7,7,1,6,7,13,10,2,101,19,7,7,5);  # sf.net bug #2019651
# Test a range of values
is_pdl $x->pctover(-0.5), pdl(0), "pct below 0 for 25-elem pdl";
is_pdl $x->pctover( 0.0), pdl(0), "pct equal 0 for 25-elem pdl";
is_pdl $x->pctover( 0.9), pdl(17), "pct equal 0.9 for 25-elem pdl [SF bug 2019651]";
is_pdl $x->pctover( 1.0), pdl(101), "pct equal 1 for 25-elem pdl";
is_pdl $x->pctover( 2.0), pdl(101), "pct above 1 for 25-elem pdl";

is_pdl sequence(10)->pctover(0.2 ), pdl(1.8), "20th percentile of 10-elem ndarray [SF bug 2753869]";
is_pdl sequence(10)->pctover(0.23), pdl(2.07), "23rd percentile of 10-elem ndarray [SF bug 2753869]";

ok( ( eval { pdl([])->qsorti }, $@ eq '' ), "qsorti coredump,[SF bug 2110074]");

for (
  [
    pdl(0,0,6,3,5,0,7,14,94,5,5,8,7,7,1,6,7,13,10,2,101,19,7,7,5),
    pdl('0 0 0 1 2 3 5 5 5 5 6 6 7 7 7 7 7 7 8 10 13 14 19 94 101'),
  ], # sf.net bug #2019651
  [ pdl([55]), pdl([55]) ],
  [ pdl(55,55), pdl(55,55) ],
  [ sequence(10)->rotate(1), sequence(10) ],
  [ pdl('0 1 2 BAD 4 5 6 7 8 9'), pdl('0 1 2 4 5 6 7 8 9 BAD') ],
  [ pdl('0 BAD 4'), pdl('0 4 BAD') ],
  [ pdl('BAD 4'), pdl('4 BAD') ],
  [ pdl('[BAD]'), pdl('[BAD]') ],
  [ pdl("0 -100 BAD 100"), pdl('-100 0 100 BAD') ], # test qsort moves values with BAD components to end
  [ pdl('1 2;0 500;2 3;4 2;3 4;3 5'), pdl('0 500;1 2;2 3;3 4;3 5;4 2') ],
  [ pdl('1 2;0 500;2 3;4 BAD;3 4;3 5'), pdl('0 500;1 2;2 3;3 4;3 5;4 BAD') ],
  [ pdl("0 0;-100 0;BAD 0;100 0"), pdl('-100 0; 0 0; 100 0; BAD 0') ], # test qsortvec moves vectors with BAD components to end - GH#252
) {
  my ($in, $exp) = @$_;
  my $meth = $in->ndims > 1 ? 'qsortvec' : 'qsort'; # assume broadcast works
  is_pdl $in->copy->$meth, $exp, "non-inplace qsort $in";
  my $in_copy = $in->copy;
  $in_copy->inplace->$meth;
  is_pdl $in_copy, $exp, "inplace qsort $in";
  $meth .= "i";
  my $inds = $in->$meth;
  is_pdl $in->dice_axis(-1, $inds), $exp, "$in $meth";
}

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
is_pdl pdl(5)->qsort, pdl([5]),'trivial qsort';
is_pdl pdl(8)->qsorti, indx([0]),'trivial qsorti';
is_pdl pdl(42,41)->qsortvec, pdl(42,41)->dummy(1),'trivial qsortvec';
is_pdl pdl(53,35)->qsortveci,indx([0]),'trivial qsortveci';

# test for sf.net bug report 3234141 "max() fails on nan"
#   NaN values are handled inconsistently by min, minimum, max, maximum...
#
{
 my $nan = nan();
 my $x = pdl($nan, 0, 1, 2);
 my $y = pdl(0, 1, 2, $nan);

 is "".$x->min, '0', 'min leading nan';
 is "".$y->min, '0', 'min trailing nan';
 is "".$x->max, '2', 'max leading nan';
 is "".$y->max, '2', 'max trailing nan';
 is join(" ", $x->minmax), '0 2', 'minmax leading nan';
 is join(" ", $y->minmax), '0 2', 'minmax trailing nan';
}
my $empty = empty();
is_pdl $empty->maximum, sbyte('BAD'), "max of empty nonbad float gives BAD";
# test bad value handling with max
$empty->badflag(1);
is_pdl $empty->maximum, sbyte('BAD'), "bad flag gets set on max over an empty dim";
is_pdl $empty->magnover, float('BAD'), "bad flag gets set on empty magnover";

is_pdl zeroes(4)->magnover, pdl(0), 'magnover correct for real zeroes';
is_pdl sequence(4)->magnover, pdl(3.741657), 'magnover correct for real sequence';
is_pdl +(sequence(4)+i())->magnover, cdouble(4.242640), 'magnover correct for complex';

#Test subroutines directly.

#set up ndarrays
my $f=pdl(1,2,3,4,5);
my $g=pdl (0,1);
my $h=pdl(1, 0,-1);
my $i=pdl (1,0);
my $j=pdl(-3, 3, -5, 10);

#Test percentile routines
is_pdl $f->pct(.5), pdl(3), 'PDL::pct 50th percentile';
is_pdl $g->pct(.76), pdl(0.76), 'PDL::pct interpolation test';
is_pdl $i->pct(.76), pdl(0.76), 'PDL::pct interpolation not in order test';

is_pdl $f->oddpct(.5), pdl(3), 'PDL::oddpct 50th percentile';
is_pdl $f->oddpct(.79), pdl(4), 'PDL::oddpct intermediate value test';
is_pdl $h->oddpct(.5), pdl(0), 'PDL::oddpct 3-member 50th percentile with negative value';
is_pdl $j->oddpct(.1), pdl(-5), 'PDL::oddpct negative values in-between test';

#Test oddmedian
is_pdl $g->oddmedian, pdl(0), 'Oddmedian 2-value ndarray test';
is_pdl $h->oddmedian, pdl(0), 'Oddmedian 3-value not in order test';
is_pdl $j->oddmedian, pdl(-3), 'Oddmedian negative values even cardinality';

#Test mode and modeover
$x = pdl([1,2,3,3,4,3,2],1);
is_pdl $x->mode, longlong(0), "mode";
is_pdl $x->modeover, longlong(3,0), "modeover";

#the next 4 tests address GitHub issue #248.

#   .... 0000 1010
#   .... 1111 1100
#OR:.... 1111 1110 = -2
is longlong([10,0,-4])->borover(), -2, "borover with no BAD values";

#     .... 1111 1111
#     .... 1111 1010
#     .... 1111 1100
#AND: .... 1111 1000 = -8

is( longlong([-6,~0,-4])->bandover(), -8, "bandover with no BAD values");

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

{
  is_pdl empty(indx)->long->avg, long(0), "avg of long Empty";
  is_pdl empty(indx)->double->average, nan(), "average double Empty";
}

# provide independent copies of test data.
sub X { PDL->pdl( [ [ 5, 4, 3 ], [ 2, 3, 1.5 ] ] ) }

is_pdl X->average(),  PDL->pdl( [ 4,  2.1666666 ] ), "average";
is_pdl X->sumover(),  PDL->pdl( [ 12, 6.5 ] ),     "sumover";
is_pdl X->prodover(), PDL->pdl( [ 60, 9 ] ),       "prodover";

# provide independent copies of test data.
sub IM {
    PDL->new(
        [
            [ 1,  2,  3,  3,  5 ],
            [ 2,  3,  4,  5,  6 ],
            [ 13, 13, 13, 13, 13 ],
            [ 1,  3,  1,  3,  1 ],
            [ 10, 10, 2,  2,  2, ]
        ]
    );
}

subtest 'minmax' => sub {
    my @minMax = IM->minmax;
    is $minMax[0], 1,  "minmax min";
    is $minMax[1], 13, "minmax max";
};

is_pdl ones(byte, 3000)->dsumover, pdl(3000);

subtest 'minimum_n_ind' => sub {

    subtest 'usage' => sub {
        my $p = pdl [ 1, 2, 3, 4, 7, 9, 1, 1, 6, 2, 5 ];
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is_pdl $q, pdl(indx, 0, 6, 7, 1, 9), "usage 1";
        $q = minimum_n_ind( $p, 5 );
        is_pdl $q, pdl(indx, 0, 6, 7, 1, 9), "usage 2";
        minimum_n_ind( $p, $q = null, 5 );
        is_pdl $q, pdl(indx, 0, 6, 7, 1, 9), "usage 3";
    };

    subtest 'BAD' => sub {
        my $p = pdl '[1 BAD 3 4 7 9 1 1 6 2 5]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 6 7 9 2]', "BAD";
    };

    subtest 'insufficient good' => sub {
        my $p = pdl '[1 BAD 3 4 BAD BAD]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 2 3 BAD BAD]', "insufficient good";
    };

    subtest 'bad & good' => sub {
        my $p = pdl '[1 BAD 3 4 BAD BAD 3 1 5 8 9]';
        my $q = zeroes 5;
        minimum_n_ind $p, $q;
        is $q. '', '[0 7 2 6 3]', "some bad, sufficient good";
    }
};

subtest numdiff => sub {
  my $a = sequence(5) + 2;
  is_pdl $a->numdiff, pdl(2, 1, 1, 1, 1), "numdiff";
  $a->inplace->numdiff;
  is_pdl $a, pdl(2, 1, 1, 1, 1), "numdiff inplace";
};

subtest diff2 => sub {
  my $got = pdl('[BAD 2 3 4]')->diff2;
  is "$got", "[2 1 1]", 'first bad';
  $got = pdl('[BAD BAD 3 4]')->diff2;
  is "$got", "[BAD 3 1]", 'first 2 bad';
  $got = pdl('[2 BAD 3 4]')->diff2;
  is "$got", "[BAD 1 1]", 'second bad';
  $got = pdl('[2 3 BAD 4]')->diff2;
  is "$got", "[1 BAD 1]", 'third bad';
  $got = pdl('[2 BAD BAD 4]')->diff2;
  is "$got", "[BAD BAD 2]", 'middle 2 bad';
  $got = pdl('[2 3 4 BAD]')->diff2;
  is "$got", "[1 1 BAD]", 'last bad';
  $got = pdl('[BAD BAD 4]')->diff2;
  is "$got", "[BAD 4]", 'only 1 good';
  $got = pdl('[BAD BAD]')->diff2;
  is "$got", "[BAD]", 'none good';
  eval {empty()->diff2};
  like $@, qr/diff2/, 'empty gives good error';
  $got = pdl(1)->diff2;
  is "$got", "Empty[0]", 'single-element gives empty';
};

subtest intover => sub {
  for ([1,0], [2,0.5], [3,2], [4,4.5], [5,8], [6,12.5], [7,18]) {
    my ($size, $exp) = @$_;
    is_pdl sequence($size)->intover, pdl($exp), "intover $size";
  }
};

subtest firstnonzeroover => sub {
  my $a = pdl '0 0 3 4; 0 5 0 1';
  is_pdl $a->firstnonzeroover, pdl(3, 5), "firstnonzeroover";
};

done_testing;
