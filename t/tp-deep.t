use strict;
use warnings;
use Test::More 0.88;
use Test::Deep qw( cmp_deeply code );
use Test::PDL qw( :deep eq_pdl );
use Test::Builder::Tester;
use Test::Exception;
use PDL;
use PDL::Types;

my @types = PDL::Types::types;

isa_ok test_pdl( 1,2,3 ), 'Test::Deep::PDL';
for my $type ( @types ) {
	no strict 'refs';
	my $sub = "test_$type";
	isa_ok $sub->( 1,2,3 ), 'Test::Deep::PDL';
}

{
	my $pdl1     = pdl( 1,2,3.13 );
	my $got      = { name => 'Histogram', data => $pdl1 };
	my $pdl2     = pdl( 1,2,3.13 );
	my $expected = { name => 'Histogram', data => $pdl2 };
	throws_ok { ok $pdl1 == $pdl2 } qr/multielement ndarray in conditional expression /, '== dies with an error message';
	throws_ok { is $pdl1, $pdl2 } qr/multielement ndarray in conditional expression /, 'is() dies with an error message';
}

{
	my $pdl      = pdl( 1,2,3.13 );
	my $got      = { name => 'Histogram', data => $pdl };
	my $expected = { name => 'Histogram', data => $pdl };
	throws_ok { ok $pdl == $pdl } qr/^multielement ndarray in conditional expression /, 'even shallow reference comparisons do not work with ==';
}

{
	my $pdl      = pdl( 1,2,3.13 );
	my $got      = { name => 'Histogram', data => $pdl };
	my $expected = { name => 'Histogram', data => $pdl };
	test_out 'ok 1';
	cmp_deeply $got, $expected;
	test_test 'cmp_deeply() without test_pdl() performs only shallow reference comparison';
}

{
	my $pdl      = pdl( 1,2,3.13 );
	my $got      = { name => 'Histogram', data => $pdl };
	my $expected = { name => 'Histogram', data => $pdl->copy };
	test_out 'not ok 1';
	test_fail +4;
	test_diag 'Compared ${$data->{"data"}}';
	test_err  "/#    got : '-?\\d+'/",
		  "/# expect : '-?\\d+'/";
	cmp_deeply $got, $expected;
	test_test 'but shallow reference comparison is not powerful enough';
}

=pod

The following test code may be a bit hard to follow. We're basically trying to
ensure that

	my @vals     = ( ... );
	my $got      = { data => long( @vals ) };
	my $expected = { data => test_long( @vals ) };
	cmp_deeply $got, $expected;

passes for every conceivable type of ndarray (not only I<long>), and for
different sets of values @vals, some of which may contain bad values. We also
test that

	{ data => test_long( @vals ) }
	{ data => test_pdl( long(@vals) ) }
	{ data => code( sub { eq_pdl shift, long(@vals) } ) }

yield the same results.

=cut

for my $vals ( [ 0 ], [ 2,3,0,1,99 ], [ 99,99,99 ] ) {
	for my $type ( @types ) {
		my @vals      = @$vals;
		my $ctor      = do { local *slot = $PDL::{ $type }; *slot{CODE} };
		my $tester    = do { local *slot = $Test::PDL::{ 'test_' . $type }; *slot{CODE} };
		my $pdl       = $ctor->( @vals )->inplace->setvaltobad( 99 );
		note 'test with pdl = ', $pdl->info('%-8T'), ' ', $pdl;
		my $got       = { data => $pdl };
		my $expected1 = { data => $tester->( @vals ) };
		$expected1->{data}->{expected}->inplace->setvaltobad( 99 );
		test_out 'ok 1';
		cmp_deeply $got, $expected1;
		test_test "$type succeeds when it should succeed, with ndarray supplied as values (@vals)";
		my $expected2 = { data => test_pdl( $pdl ) };
		test_out 'ok 1';
		cmp_deeply $got, $expected2;
		test_test "... ($type) also when ndarray is supplied directly (@vals)";
		my $expected3 = { data => code( sub { eq_pdl shift, $pdl } ) };
		test_out 'ok 1';
		cmp_deeply $got, $expected3;
		test_test "... ($type) and it's the same thing as using code() (@vals)";
	}
}

{
	my $pdl1 = 2;
	my $pdl2 = pdl( 3,4,9.999 );
	ok !eq_pdl( $pdl1, $pdl2 ), 'ndarrays are unequal to begin with';
	my $got = { data => $pdl1 };
	my $expected = { data => test_pdl( $pdl2 ) };
	test_out 'not ok 1';
	test_fail +5;
	test_diag 'Comparing $data->{"data"} as an ndarray:',
		  'received value is not an ndarray';
	test_err  "/#    got : \\('2'\\)/",
		  '/# expect : Double\s+D\s+\[3\].*/';
	cmp_deeply $got, $expected;
	test_test 'fails with correct message and diagnostics when received value is not an ndarray';
	test_out 'not ok 1';
	test_fail +6;
	test_diag 'Ran coderef at $data->{"data"} on';
	test_err  '/#?\s*/';
	test_diag "'2'",
		  'and it said',
		  'received value is not an ndarray';
	cmp_deeply $got, { data => code( sub { eq_pdl shift, $pdl2 } ) };
	test_test '... but the diagnostics are better than with code()';
}

{
	my $pdl1 = pdl( 3,4,9.999 );
	my $pdl2 = pdl( 3,4,10 );
	ok !eq_pdl( $pdl1, $pdl2 ), 'ndarrays are unequal to begin with';
	my $got = { data => $pdl1 };
	my $expected = { data => test_pdl( $pdl2 ) };
	test_out 'not ok 1';
	test_fail +5;
	test_diag 'Comparing $data->{"data"} as an ndarray:',
		  '1/3 values do not match';
	test_err  '/#    got : Double\s+D\s+\[3\].*/',
		  '/# expect : Double\s+D\s+\[3\].*/';
	cmp_deeply $got, $expected;
	test_test 'fails with correct message and diagnostics on value mismatch';
	test_out 'not ok 1';
	test_fail +6;
	test_diag 'Ran coderef at $data->{"data"} on';
	test_err  '/#?\s*/',
		  '/# PDL=SCALAR\(0x[0-9A-Fa-f]+\)/';
	test_diag 'and it said',
		  '1/3 values do not match';
	cmp_deeply $got, { data => code( sub { eq_pdl shift, $pdl2 } ) };
	test_test '... but the diagnostics are better than with code()';
}

{
	my $pdl1 = short( 3,4,-6 );
	my $pdl2 = long( 3,4,10 );
	ok !eq_pdl( $pdl1, $pdl2 ), 'ndarrays are unequal to begin with';
	my $got = { data => $pdl1 };
	my $expected = { data => test_pdl( $pdl2 ) };
	test_out 'not ok 1';
	test_fail +5;
	test_diag 'Comparing $data->{"data"} as an ndarray:',
		  'types do not match (\'require_equal_types\' is true)';
	test_err  '/#    got : Short\s+D\s+\[3\].*/',
		  '/# expect : Long\s+D\s+\[3\].*/';
	cmp_deeply $got, $expected;
	test_test 'fails with correct message and diagnostics on type mismatch';
	test_out 'not ok 1';
	test_fail +6;
	test_diag 'Ran coderef at $data->{"data"} on';
	test_err  '/#?\s*/',
		  '/# PDL=SCALAR\(0x[0-9A-Fa-f]+\)/';
	test_diag 'and it said',
		  'types do not match (\'require_equal_types\' is true)';
	cmp_deeply $got, { data => code( sub { eq_pdl shift, $pdl2 } ) };
	test_test '... but the diagnostics are better than with code()';
}

done_testing;
