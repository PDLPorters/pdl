use Test::More;

use strict;
use warnings;

use PDL::Config;
use Test::Warn;

plan skip_all => "Bad values disabled" unless $PDL::Config{WITH_BADVAL};
plan skip_all => "Skip if badvalue support only supports NaN badvalues" if $PDL::Config{BADVAL_USENAN};

use PDL::LiteF;

## Issue information
##
## Name: scalar PDL with badvalue always compares BAD with perl scalars
##
## <http://sourceforge.net/p/pdl/bugs/390/>
## <https://github.com/PDLPorters/pdl/issues/124>

plan tests => 5;

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
	my $stats_data = [
		{
			name => "stats() should not set the badflag for output with only one badvalue",
			func => \&stats,
			input => do { pdl [1, 2, 3] },
			badvalue => 2,
			string => "[1 BAD 3]",
			mean => "2",
			badflag => 0
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
			badflag => 0,
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
			is( $m->badflag, $case->{badflag}, "Mean does @{[ (' not ')x!!( ! $case->{badflag} ) ]} have badflag set");
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
	my $warn_msg_re = qr/Badvalue is set to 0 or 1/;

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
}
