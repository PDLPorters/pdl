use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Math;
use PDL::Types qw(types);
use Test::Warn;
use Test::PDL;

sub abstol { {atol=>1.0e-4, test_name=>$_[0]} }

{
  my $a_bad = pdl double, '[1 BAD 3]';
  my $b_double = zeroes double, 3;
  $a_bad->assgn($b_double);
  ok $b_double->badflag, 'b_double badflag set';
  is $b_double.'', '[1 BAD 3]', 'b_double got badval';
  my $b_float = zeroes float, 3;
  $a_bad->assgn($b_float);
  ok $b_float->badflag, 'b_float badflag set';
  is $b_float.'', '[1 BAD 3]', 'b_float got badval';
}

# check default behaviour (ie no bad data)
# - probably overkill
#
my $x = pdl(1,2,3);
is( $x->badflag(), 0, "no badflag" );

my $y = pdl(4,5,6);
my $c = $x + $y;
is( $c->badflag(), 0, "badflag not set in a copy" );
is( $c->sum(), 21, "sum() works on non bad-flag ndarrays" );

# is the flag propagated?
$x->badflag(1);
ok( $x->badflag(), "bad flag is now set" );

$c = $x + $y;
ok( $c->badflag(), "bad flag is propagated" );
is( $c->sum(), 21, "sum is still 21 with badflag set" );

$x->badflag(0);
$y->badflag(1);
$c = $x + $y;
ok( $c->badflag(), "badflag propagates on rhs of 'x+y'" );

# how about copies/vaffines/whatever
$x = rvals( long, 7, 7, {Centre=>[2,2]} );
$y = $x;
is( $y->badflag, 0, "badflag not set in a copy" );

$x->badflag(1);
$y = $x;
ok( $y->badflag, "badflag is now set in a copy" );

$x->badflag(0);
$y = $x->slice('2:5,3:4');
$c = $y->slice('0:1,(0)');
is( $y->badflag, 0, "slice handling okay with no badflag" );

$x->badflag(1);

# let's check that it gets through to a child of a child
ok( $c->badflag, "badflag propagated through to a child" );

# can we change bad values
is( byte->badvalue, byte->orig_badvalue, "byte bad value is set to the default value" );
byte->badvalue(23);
is( byte->badvalue, 23, "changed bad value for byte" );
byte->badvalue( byte->orig_badvalue );

# check setbadat()
$x = pdl(1,2,3,4,5);
$x->setbadat(2);
is_pdl $x, pdl("[1 2 BAD 4 5]"), "setbadat worked";

$y = $x->copy;
is_pdl $y, pdl("[1 2 BAD 4 5]"), "y correct bad before set_datatype";
$y->set_datatype(ushort->enum);
is_pdl $y, ushort("[1 2 BAD 4 5]"), "y correct bad after set_datatype";

$y = $x->copy;
$y->badvalue('nan');
$y->setbadat(2);
is $y."", "[1 2 BAD 4 5]", "y correct bad before set_datatype with badval=nan";
my $z = $y->convert(ushort);
is_pdl $z, ushort("[1 2 BAD 4 5]"), "non-inplace converting NaN-badvalued pdl preserves badvals";
$y->set_datatype(ushort->enum);
is_pdl $y, ushort("[1 2 BAD 4 5]"), "y correct bad after set_datatype with badval=nan";

# now check that badvalue() changes the ndarray
# (only for integer types)
$x = convert($x,ushort);
is_pdl $x, ushort("[1 2 BAD 4 5]"), "before change badvalue";
my $badval = $x->badvalue;
$x->badvalue(44);
is_pdl $x, ushort("[1 2 BAD 4 5]"), "changed badvalue";
$x->badflag(0);
is_pdl $x, ushort("[1 2 44 4 5]"), "can remove the badflag setting";
# restore the badflag
$x->badflag(1);
is_pdl $x, ushort("[1 2 BAD 4 5]"), "still 'bad' w/changed badvalue";

$x = byte(1,2,3);
$x->badflag(1);
$y = byte('1 BAD 3');
is( PDL::Core::string($y), "[1 BAD 3]", "can convert bad values to a string" );

# does addition work
is_pdl $x + $y, byte('2 BAD 6'), "addition propagates the bad value";

# does conversion of bad types work
$c = float($y);
is_pdl $c, float("[1 BAD 3]"), "type conversion retains bad flag and values";
is_pdl sum($c), float(4), "  and the sum";

$x = byte('1 2 BAD BAD 5 6 BAD 8 9');
is_pdl $x->isbad, long("0 0 1 1 0 0 1 0 0"), "isbad() works";
is_pdl $x->isgood, long("1 1 0 0 1 1 0 1 1"), "isgood() works";
is $x->nbad, 3, "nbad() works";
is $x->ngood, 6, "ngood() works";

$x = byte('BAD BAD; BAD 0; 0 0');
is_pdl $x->nbadover, indx("[2 1 0]"), "nbadover() works";
is_pdl $x->ngoodover, indx("[0 1 2]"), "ngoodover() works";

# check dataflow (or vaffine or whatever it's called)
$x = byte('1 2 BAD 4 5; BAD 0 1 2 BAD');
$y = $x->slice(',(1)');
is( sum($y), 3, "sum of slice works" );
$y++;
is_pdl $x, byte("1 2 BAD 4 5; BAD 1 2 3 BAD"), "inplace addition of slice flows back to parent";

$x = byte->badvalue * ones(byte,3,2);
is $x->type, 'byte', "datatype remains a byte";
$x->badflag(1);
is_pdl PDL::zcover($x), byte("[BAD BAD]"), "zcover() okay";
$x->set(1,1,1);
$x->set(2,1,1);
is_pdl PDL::zcover($x), byte("[BAD 0]"), "  and still okay";

# 255 is the default bad value for a byte array
#
$x = byte(1,2,255,4,5);
is( $x->median, 4, "median() works on good ndarray" );
$x->badflag(1);
is( $x->median, 3, "median() works on bad biddle" );

# as random() creates numbers between 0 and 1 it won't
# accidentally create a bad value by chance (the default
# bad value for a double is a very negative number).
$x = random(20);
$x->badflag(1);
is $x->check_badflag, 0, "check_badflag did not find a bad value";

# check out stats, since it uses several routines
# and setbadif
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 );
my @s = $y->stats();
is_pdl $s[0], pdl(61.9375), abstol("setbadif/stats test 1");
is_pdl $s[1], pdl(27.6079), abstol("setbadif/stats test 2");
is_pdl $s[2], pdl(66.5), "setbadif/stats test 3";
is_pdl $s[3], pdl(22), "setbadif/stats test 4";
is_pdl $s[4], pdl(98), "setbadif/stats test 5";
is_pdl $s[6], pdl(26.7312), abstol("setbadif/stats test 6");
ok !$x->badflag, 'badflag not set on input after setbadif';

# how about setbadtoval
empty()->setbadtoval(20); # shouldn't segfault
ok $y->badflag, 'badflag on';
is_pdl $y->setbadtoval(20), pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20)), "setbadtoval() worked";
ok $y->badflag, 'badflag still on';

# and inplace?
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 );
$y->inplace->setbadtoval(20);
is_pdl $y, pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20)), "   and inplace";

# ditto for copybad
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 );
$c = copybad( $x, $y );
is_pdl $c->isbad, long("[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]"),
  "isbad() worked";

$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 );
$x->inplace->copybad( $y );
is_pdl $x->isbad, long("[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]"),
  "  and inplace";

$x = zeroes(20,30);
$y = $x->slice('0:10,0:10');
$c = $y->slice(',(2)');
ok !$c->badflag, 'no badflag on slice-child of good';
$x->badflag(1);
ok $c->badflag, 'badflag on same slice-child of good set to bad';
$c->badflag(0);
ok !$x->badflag, 'badflag now off for slice-parent of bad slice-child set to good';

$x = pdl '1 BAD';
ok any($x > 0), 'any with some badvals just omits them';
ok all($x > 0), 'all with some badvals just omits them';

## $x->inplace->setbadif( $x % 2 ) does NOT work because
## ($x % 2) is performed inplace - ie the flag is set for
## that function
#
##$x = sequence(3,3);
##$x->inplace->setbadif( $x % 2 );
###$x = $x->setbadif( $x % 2 );              # for when not bothered about inplace
##ok( PDL::Core::string( $x->clump(-1) ),
##    "[0 BAD 2 BAD 4 BAD 6 BAD 8]" );   #

## look at propagation of bad flag using inplace routines...
$x = sequence( byte, 2, 3 );
$x = $x->setbadif( $x == 3 );
$y = $x->slice("(1),:");
$x->inplace->setbadtoval(3);
is( $x->badflag, 0, "direct pdl badflag cleared using inplace setbadtoval()" );
is( $y->badflag, 0, "child pdl badflag cleared using inplace setbadtoval()" );

$x = sequence( byte, 2, 3 );
$y = $x->slice("(1),:");
my $mask = sequence( byte, 2, 3 );
$mask = $mask->setbadif( ($mask % 3) == 2 );
$x->inplace->copybad( $mask );
is( $y->badflag, 1, "badflag propagated using inplace copybad()" );

# test some of the qsort functions
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 );
my $ix = qsorti( $y );
is_pdl $y->index($ix),
    pdl("[22 25 32 32 41 42 47 59 74 76 76 79 96 96 96 98 BAD BAD BAD BAD]"),
    "qsorti() okay"
    ;

# check comparison/bit operators in ops.pd

$x = pdl('2 4 BAD');
is_pdl abs( $x - pdl(2.001,3.9999,234e23) ) > 0.01, pdl("[0 0 BAD]"), "abs() and >";

is_pdl byte('1 2 BAD 4') << 2, byte("[4 8 BAD 16]"), "<<";

$x = pdl([1,2,3]);
$x->badflag(1);
$y = $x->assgn;
is( $y->badflag, 1, "assgn propagated badflag");
$x->badflag(0);
is( $y->badflag, 1, "assgn is not a deep copy for the badflag");

$x = pdl q[BAD];
is( PDL::Core::string($x), 'BAD', 'can convert PDL to string' );
is( $x->at, 'BAD', 'at() returns BAD for a bad value' );
isnt( $x->sclr, 'BAD', 'sclr() ignores bad value' );

$x = pdl 4;
$x->badflag(1);
$x->badvalue(4);
is( $x->at, 'BAD', 'at() returns BAD for a bad value with non-default badvalue' );
is( $x->sclr, 4, 'sclr() ignores bad value' );

is_pdl isbad(bessj0(pdl('0.5 BAD 0'))), long("[0 1 0]"), "bessj0()";

$y = bessjn(pdl('BAD 0.8'),3);  # broadcast over n()
is_pdl $y, pdl('BAD 0.010246'), "broadcast over bessjn()";

$x = pdl( 0.01, 0.0 );
$x->badflag(1);
is_pdl erfi($x), pdl(0.00886,0), {atol=>0.001, test_name=>"erfi()"};

# I haven't changed rotate, but it should work anyway
is_pdl byte('0 1 BAD 4 5')->rotate(2), byte("[4 5 0 1 BAD]"), "rotate()";

# check norm
$x = float('2 BAD 2 2');
is_pdl $x->norm, $x/sqrt(sum($x*$x)), abstol("norm()");

# propagation of badflag using inplace ops (ops.pd)

# test biop fns
$x = sequence(3,3);
$c = $x->slice(',(1)');
$y = $x->setbadif( $x % 2 );
$x->inplace->plus($y,0);
is_pdl $c, pdl("[BAD 8 BAD]"), "inplace biop - plus()";

# test bifunc fns
$x = sequence(3,3);
$c = $x->slice(',(1)');
$y = $x->setbadif( $x % 3 != 0 );
$x->inplace->power($y,0);
is_pdl $c, pdl("[27 BAD BAD]"), "inplace bifunc - power()";

# test histogram (using hist)
$x = pdl('1 BAD 3 4 5 4 3 2 2 1');
is_pdl scalar hist($x, 0, 6, 1), pdl("[0 2 2 2 2 1]"), "hist()";
is_pdl $x->isfinite, long("[1 0 1 1 1 1 1 1 1 1]"), "isfinite()";

# histogram2d
$x = long(1,1,1,2,2);
$y = long('BAD 1 1 1 1');
my @c = ( 1,0,3 );
is_pdl scalar histogram2d($x,$y,@c,@c), long("[0 0 0;0 2 2;0 0 0]"), "histogram2d()";

# badmask: inplace
$x = pdl("0 1 BAD 3 4");
$x->inplace->badmask(0);
is_pdl $x, pdl("[0 1 0 3 4]"), "inplace badmask()";

# setvaltobad
$x = sequence(10) % 4;
$x->inplace->setvaltobad( 1 );
is_pdl $x, pdl('0 BAD 2 3 0 BAD 2 3 0 BAD'), "inplace setvaltobad()";

$x->inplace->setbadtonan;
is_pdl $x, pdl('0 nan 2 3 0 nan 2 3 0 nan'), "inplace setvaltonan()";

# check setvaltobad for non-double ndarrays
is_pdl float(1..4)->setvaltobad(2), float('1 BAD 3 4'), "setvaltobad for float ndarray";
is_pdl double(1..4)->setvaltobad(2), double('1 BAD 3 4'), "setvaltobad for double ndarray";

my $inf2b = pdl('0 inf nan');
$inf2b->inplace->setinftobad;
is_pdl $inf2b, pdl('0 BAD nan'), "inplace setinftobad()";

my $x_copy = pdl('0 inf 2 3 0 nan 2 3 0 nan');
$x_copy->inplace->setnonfinitetobad;
is_pdl $x_copy, pdl('0 BAD 2 3 0 BAD 2 3 0 BAD'), "inplace setnonfinitetobad";

# simple test for setnantobad
# - could have a 1D FITS image containing
#   NaN's and then a simple version of rfits
#   (can't use rfits as does conversion!)
$x->inplace->setnantobad;
is_pdl $x, pdl('0 BAD 2 3 0 BAD 2 3 0 BAD'), "inplace setnantobad";

# check that we can change the value used to represent
# missing elements for floating points (earlier tests only did integer types)
#
is( float->badvalue, float->orig_badvalue, "default bad value for floats matches" );
is( float->badvalue(23), 23, "changed floating-point bad value" );
float->badvalue( float->orig_badvalue );

$x = sequence(4);
$x->badvalue(3);
$x->badflag(1);
$y = $x->slice('2:3');
is( $y->badvalue, 3, "can propagate per-ndarray bad value");
is( $y->sum, 2, "and the propagated value is recognised as bad");
$x->badvalue(2);
is "$x", '[0 1 BAD 3]', 'change badvalue, badness right in orig';
is( $y->badvalue, 2, "per-ndarray bad value propagated after change");
$x = sequence(4);
is ($x->badvalue, double->orig_badvalue, "no long-term effects of per-ndarray changes [1]");

for my $t (map +([$_, undef], [$_, 'nan']), grep !$_->integer, types()) {
  my $p = sequence $t->[0], 2;
  $p->badvalue($t->[1]) if defined $t->[1];
  $p->setbadat(1);
  my $msg = "badvalue works right $t->[0], bv=".join '', grep $_, explain($t->[1]);
  eval {is $p.'', '[0 BAD]', $msg};
  is $@, '', $msg;
}

## Name: "isn't numeric in null operation" warning could be more helpful
## <http://sourceforge.net/p/pdl/bugs/332/>
## <https://github.com/PDLPorters/pdl/issues/33>
# The following code calls the PDL::Ops::eq() function via the operator
# overload for the eq operator. Because the Perl eq operator is usually used
# for strings, the default warning of "isn't numeric in null operation" is
# confusing. Comparing a PDL against a string should give a more useful
# warning.
my $numeric_warning = qr/not numeric nor a PDL/;
my $no_warning = undef;
sub check_eq_warnings {
	my ($string, $warning) = @_;
        $warning ||= qr/^\s*$/;
        my @w;
        local $SIG{__WARN__} = sub { push @w, @_ };
	my $dummy = pdl() eq $string;
        like "@w", $warning; @w = ();
	$dummy = $string eq pdl();
        like "@w", $warning; @w = ();
}

subtest "String 'x' is not numeric and should warn" => sub {
	check_eq_warnings('x', $numeric_warning);
};
subtest "String 'nancy' is not numeric and should warn" => sub {
	check_eq_warnings('nancy', $numeric_warning);
};
subtest "String 'inf' is numeric" => sub {
	check_eq_warnings('inf', $no_warning);
};
subtest "String 'nan' is numeric" => sub {
	check_eq_warnings('nan', $no_warning);
};
TODO: {
	# implementing this might require checking for strings that can be made into PDLs
	local $TODO = "Using the eq operator with the string 'bad' might be a good feature";
	subtest "String 'bad' is numeric (in PDL)" => sub {
		check_eq_warnings('bad', $no_warning);
	};
}

## Issue information
##
## Name: scalar PDL with badvalue always compares BAD with perl scalars
##
## <http://sourceforge.net/p/pdl/bugs/390/>
## <https://github.com/PDLPorters/pdl/issues/124>
subtest "Issue example code" => sub {
  my $x = pdl(1, 2, 3, 0);
  $x->badflag(1);
  $x->badvalue(0);
  # bad value for $x is now set to 0
  is_pdl $x, pdl("[1 2 3 BAD]"), "PDL with bad-value stringifies correctly";
  my ($m, $s) = stats($x);
  is_pdl $m, pdl(2), "Mean of [1 2 3] is 2";
  is_pdl $s, pdl(1), "And std. dev is 1";
  $s->badflag(1);
  $s->badvalue(0);
  my @warnings;
  local $SIG{__WARN__} = sub { push @warnings, @_ };
  is_pdl $s >  0, pdl(1), "is 1 >  0? -> true";
  is_pdl $s <  0, pdl(0), "is 1 <  0? -> false";
  is_pdl $s == 0, pdl(0), "is 1 == 0? -> false";
  ok scalar(@warnings), 'bad gave warnings';
};

subtest "Badvalue set on 0-dim PDL + comparison operators" => sub {
  my $val = 2;
  my $badval_sclr = 5;
  my $p_val = pdl($val);

  $p_val->badflag(1);
  $p_val->badvalue($badval_sclr);

  is_pdl $p_val, pdl($val), "Sanity test";

  my @values_to_compare = ( $badval_sclr, $badval_sclr + 1, $badval_sclr - 1  );
  subtest "Comparing a 0-dim PDL w/ a scalar should be the same as comparing a scalar w/ a scalar" => sub {
    for my $cmpval_sclr (@values_to_compare) {
      subtest "Bad value for PDL $p_val is $badval_sclr and we are comparing with a scalar of value $cmpval_sclr" => sub {
        is_pdl $p_val <  $cmpval_sclr, pdl(0+($val < $cmpval_sclr)),
          "$val < $cmpval_sclr";
        is_pdl $p_val == $cmpval_sclr, pdl(0+($val == $cmpval_sclr)),
          "$val == $cmpval_sclr";
        is_pdl $p_val > $cmpval_sclr, pdl(0+($val > $cmpval_sclr)),
          "$val > $cmpval_sclr";
      };
    }
  };

  subtest "Comparing a 0-dim PDL w/ bad value with a 0-dim PDL without bad value set should not set BAD" => sub {
    for my $not_bad_sclr (@values_to_compare) {
      subtest "Bad value for PDL $p_val is $badval_sclr and we are comparing with a PDL of value $not_bad_sclr, but with no badflag" => sub {
        my $p_not_bad = pdl($not_bad_sclr);
        $p_not_bad->badflag(0); # should not have bad flag

        my $lt_p = $p_val <  $p_not_bad;
        is_pdl $lt_p, pdl(0+(   $val <  $not_bad_sclr)),
          "$val <  $not_bad_sclr";
        ok $lt_p->badflag, "cmp for < does set badflag";

        my $eq_p = $p_val ==  $p_not_bad;
        is_pdl $eq_p, pdl(0+(  $val == $not_bad_sclr)),
          "$val == $not_bad_sclr";
        ok $eq_p->badflag, "cmp for == does set badflag";

        my $gt_p = $p_val >  $p_not_bad;
        is_pdl $gt_p, pdl(0+(  $val >  $not_bad_sclr)),
          "$val >  $not_bad_sclr";
        ok $gt_p->badflag, "cmp for > does set badflag";
      };
    }
  };
};


subtest "stats() badvalue behavior" => sub {
	my $stats_data = [
		{
			name => "stats() should not set the badflag for output with only one badvalue",
			func => \&stats,
			input => do { pdl [1, 2, 3] },
			badvalue => 2,
			string => "[1 BAD 3]",
			mean => "2",
			badflag => 1,
		},
		{
			name => "stats() should set the badflag for output with all badvalues and mean should be BAD" ,
			func => \&stats,
			input => do { pdl [1, 1, 1] },
			badvalue => 1,
			string => "[BAD BAD BAD]",
			mean => "BAD",
			badflag => 1,
		},
		{
			name => "and statsover() on a row of BAD values",
			func => \&statsover,
			input => do { zeroes(3,3)->yvals+1 },
			badvalue => 1,
			string => do {
my $p_str = <<'EOF';

[
 [BAD BAD BAD]
 [  2   2   2]
 [  3   3   3]
]
EOF
			},
			mean => "[BAD 2 3]",
			badflag => 1,
		},
		{
			name => "and statsover() on a diagonal of BAD values",
			func => \&statsover,
			input => do { my $p = ones(3,3)*2; $p->diagonal(0,1) .= 1; $p },
			string => do {
my $p_str = <<'EOF';

[
 [BAD   2   2]
 [  2 BAD   2]
 [  2   2 BAD]
]
EOF
			},
			badvalue => 1,
			mean => "[2 2 2]",
			badflag => 1,
		}
	];

	for my $case (@$stats_data) {
		subtest $case->{name} => sub {
			my $p = $case->{input};
			$p->badflag(1);
			$p->badvalue($case->{badvalue});

			note "\$p = $p";
			is( "$p", $case->{string}, "stringifies properly");

			my $m = $case->{func}->($p);

			note "\$m = $m";
			is( "$m", $case->{mean}, "Mean of \$p" );
			is( $m->badflag, $case->{badflag}, "Mean does @{[ ('not ')x!!( ! $case->{badflag} ) ]}have badflag set");
		};
	}


};

subtest "Comparison between a vector and scalar" => sub {
	my $p = pdl [1, 2, 3, 4];
	$p->badflag(1);
	$p->badvalue(2);

	note "\$p = $p";
	is( "$p", "[1 BAD 3 4]", "PDL vector (with bv = 2)");

	is( "" . ( $p > 1 ), '[0 BAD 1 1]', "compare PDL against (scalar = 1)");
	is( "" . ( $p > 2 ), '[0 BAD 1 1]', "compare PDL against (scalar = 2)" );
	is( "" . ( $p > 3 ), '[0 BAD 0 1]', "compare PDL against (scalar = 3)");
	is( "" . ( $p > 4 ), '[0 BAD 0 0]', "compare PDL against (scalar = 4)");
};

subtest "Throw a warning when badvalue is set to 0 or 1 and a comparison operator is used" => sub {
	my $warn_msg_re = qr/badvalue is set to 0 or 1/;

	# We do not need to change the contents of this PDL.
	# Only the value of badvalue changes.
	my $p = pdl([0, 1, 2]);
	$p->badflag(1);
	subtest "Badvalue set to 0" => sub {
		$p->badvalue(0);
		warning_like { $p == 1 } $warn_msg_re, "A warning thrown for badval == 0 and == operator";
	};

	subtest "Badvalue set to 1" => sub {
		$p->badvalue(1);
		warning_like { $p == 1 } $warn_msg_re, "A warning thrown for badval == 1 and == operator";
	};

	subtest "Badvalue set to 2" => sub {
		$p->badvalue(2);
		warning_like { $p == 1 } undef, "No warning thrown for badval == 2 and == operator";
	};

	subtest "Badvalue set to 0 and other operators" => sub {
		$p->badvalue(0);

		warning_like { $p > 1 } $warn_msg_re, "A warning thrown for badval == 0 and > operator";
		warning_like { $p >= 1 } $warn_msg_re, "A warning thrown for badval == 0 and >= operator";
		warning_like { $p < 1 } $warn_msg_re, "A warning thrown for badval == 0 and < operator";
		warning_like { $p <= 1 } $warn_msg_re, "A warning thrown for badval == 0 and <= operator";

		warning_like { $p == 1 } $warn_msg_re, "A warning thrown for badval == 0 and == operator";
		warning_like { $p != 1 } $warn_msg_re, "A warning thrown for badval == 0 and != operator";

		warning_like { $p + 1 } undef, "No warning thrown for badval == 0 and + operator";
	};
};

subtest "locf" => sub {
  my $withbad = pdl '[BAD 1 BAD 3 BAD 5]';
  my $locf = $withbad->locf;
  is $locf."", '[0 1 1 3 3 5]', 'locf worked';
};

subtest "badvalues for native complex" => sub {
  my $pdl = pdl '1+i';
  $pdl->badflag(1);
  $pdl->badvalue($pdl);
  is "$pdl", "BAD", 'set badvalue with complex ndarray';
  $pdl->badvalue($pdl->sclr);
  is "$pdl", "BAD", 'set badvalue with complex Perl scalar'
    or diag "badvalue:", $pdl->badvalue->info, "=", $pdl->badvalue;
};

subtest "badvalue propagated to different-typed child not break them" => sub {
  my $pd = pdl 7;
  my $pl = $pd->_convert_int(long->enum);
  $pd->badvalue(inf());
  eval { $pl->dump };
  is $@, '', 'badval propagate to different-typed child not break it';
};

done_testing;
