use Test::More tests => 9;
use PDL::LiteF;
use Test::Deep;
use Data::Dumper;

use strict;
use warnings;

$|=1;

#  PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $pa = zeroes(20);
$pa->hdrcpy(1);
$pa->dump;
$pa->sethdr( {Field1=>'arg1',
	     Field2=>'arg2'});
note "pa: ", Dumper $pa->gethdr();

ok($pa->hdrcpy);

{
	my $pb = $pa+1;
	note "pb: ", Dumper $pb->gethdr();
	ok( defined($pb->gethdr));
	is_deeply($pa->gethdr,$pb->gethdr);
}

{
	my $pb = ones(20) + $pa;
	note "pb: ", Dumper $pb->gethdr();
	ok( defined($pb->gethdr));
	is_deeply($pa->gethdr,$pb->gethdr);
}

{
	my $pc = $pa->slice('0:5');
	note "pc: ", Dumper $pc->gethdr();
	is_deeply($pa->gethdr,$pc->gethdr);
}

{
	my $pd = $pa->copy;
	note "pd: ", Dumper $pd->gethdr();
	is_deeply($pa->gethdr,$pd->gethdr);
}

{
	$pa->hdrcpy(0);
	ok(defined($pa->slice('3')->hdr) && !( keys (%{$pa->slice('3')->hdr})));
	ok(!defined($pa->slice('3')->gethdr));
}

done_testing;
