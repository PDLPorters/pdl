use Test::More;

use strict;
use warnings;

use PDL::Config;

plan skip_all => "Bad values disabled" unless $PDL::Config{WITH_BADVAL};
plan skip_all => "Skip if badvalue support only supports NaN badvalues" if $PDL::Config{BADVAL_USENAN};

use PDL::LiteF;

## Issue information
##
## Name: scalar PDL with badvalue always compares BAD with perl scalars
##
## <http://sourceforge.net/p/pdl/bugs/390/>
## <https://github.com/PDLPorters/pdl/issues/124>

plan tests => 4;

subtest "Issue example code" => sub {
	my $x = pdl(1, 2, 3, 0);
	$x->badflag(1);
	$x->badvalue(0);
	# bad value for $x is now set to 0

	is( "$x", "[1 2 3 BAD]", "PDL with bad-value stringifies correctly" );

	my ($m, $s) = stats($x);

	is( "$m", 2, "Mean of [1 2 3] is 2" );
	is( "$s", 1, "And std. dev is 1" );

	is( "".($s >  0), "1", "is 1 >  0? -> true" );
	is( "".($s <  0), "0", "is 1 <  0? -> false");
	is( "".($s == 0), "0", "is 1 == 0? -> false");
};

subtest "Badvalue set on 0-dim PDL + comparision operators" => sub {
	my $val = 2;
	my $badval_sclr = 5;
	my $p_val = pdl($val);

	# set the bad flag to 0
	$p_val->badflag(1);
	$p_val->badvalue($badval_sclr);

	note "\$p_val = $p_val";
	is( "$p_val", "$val", "Sanity test" );

	my @values_to_compare = ( $badval_sclr, $badval_sclr + 1, $badval_sclr - 1  );
	subtest "Comparing a 0-dim PDL w/ a scalar should be the same as comparing a scalar w/ a scalar" => sub {
		for my $cmpval_sclr (@values_to_compare) {
			subtest "Bad value for PDL $p_val is $badval_sclr and we are comparing with a scalar of value $cmpval_sclr" => sub {
				is
					"".($p_val <  $cmpval_sclr),
					(0+(  $val <  $cmpval_sclr)),
					     "$val <  $cmpval_sclr";

				is
					"".($p_val == $cmpval_sclr),
					(0+(  $val == $cmpval_sclr)),
					     "$val == $cmpval_sclr";

				is
					"".($p_val >  $cmpval_sclr),
					(0+(  $val >  $cmpval_sclr)),
					     "$val >  $cmpval_sclr";
			};
		}
	};

	subtest "Comparing a 0-dim PDL w/ bad value with a 0-dim PDL without bad value set should not set BAD" => sub {
		for my $not_bad_sclr (@values_to_compare) {
			subtest "Bad value for PDL $p_val is $badval_sclr and we are comparing with a PDL of value $not_bad_sclr, but with no badflag" => sub {
				my $p_not_bad = pdl($not_bad_sclr);
				$p_not_bad->badflag(0); # should not have bad flag

				my $lt_p = $p_val <  $p_not_bad;
				is
					"".       $lt_p,
					0+(   $val <  $not_bad_sclr),
					     "$val <  $not_bad_sclr";
				ok $lt_p->badflag, "cmp for < does set badflag";

				my $eq_p = $p_val ==  $p_not_bad;
				is
					"".      $eq_p,
					0+(  $val == $not_bad_sclr),
					    "$val == $not_bad_sclr";
				ok $eq_p->badflag, "cmp for == does set badflag";

				my $gt_p = $p_val >  $p_not_bad;
				is
					"".      $gt_p,
					0+(  $val >  $not_bad_sclr),
					    "$val >  $not_bad_sclr";
				ok $gt_p->badflag, "cmp for > does set badflag";
			};
		}
	};
};


subtest "stats() badvalue behavior" => sub {
	subtest "stats() should not set the badflag for output with only one badvalue" => sub {
		my $p = pdl [1, 2, 3];
		$p->badflag(1);
		$p->badvalue(2);

		note "\$p = $p";
		is( "$p", "[1 BAD 3]", "stringifies properly");

		my $m = stats($p);

		note "\$m = $m";
		is( "$m", "2", "Mean of [1 3] is 2" );
		ok( !$m->badflag, "Mean does not have badflag set");
	};

	subtest "stats() should set the badflag for output with all badvalues" => sub {
		my $p = pdl [1, 1, 1];
		$p->badflag(1);
		$p->badvalue(1);

		note "\$p = $p";
		is( "$p", "[BAD BAD BAD]", "stringifies properly");

		my $m = stats($p);

		note "\$m = $m";
		is( "$m", "BAD", "Mean of a vector of all BAD values is BAD" );
		ok( $m->badflag, "Mean does have badflag set");
	};
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
