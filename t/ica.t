use PDL::LiteF;

# TODO This file does not currently test anything beyond loading PDL::Slatec.

use strict; # TODO fix the disabled code and enable strict
use warnings;
use Test::More tests => 2;

our $HAVE_PDL_SLATEC = 0;
our $HAVE_PDL_ICA = 0;
BEGIN {

        eval {
		require PDL::Slatec;
		$HAVE_PDL_SLATEC = 1;
	};

        eval {
		require PDL::ICA;
		$HAVE_PDL_ICA = 1;
	};
}

SKIP: {
	skip "Could not load PDL::Slatec", 1 unless $HAVE_PDL_SLATEC;
	pass("PDL::Slatec loads");
}

SKIP: {
	skip "Could not load PDL::ICA", 1 unless $HAVE_PDL_ICA;
	pass("PDL::ICA loads");
}

# use PDL::Graphics::PG;
# dev "/XSERVE",2,2;

$|=1;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Carp;

$SIG{__DIE__} = sub {print Carp::longmess(@_); die 'FOO';};

#DEFERRED

SKIP: { # XXX DISABLED
	# XXX There are no tests in here.
	# TODO change the print() to note()

if(0) {

# Generate data: oblique lattice

{
	my $pars = pdl 2,3,4;
	my $rot = PDL::LinICA::_cayleygen({NVars=>3},$pars);

	note $rot;

	my $inv = inv($rot);
	note $inv;
}

if(0) {

if(0) {
	my ($data, $xx);

	$data = long zeroes(2,36);
	axisvalues($data->xchg(0,1));
	($xx = $data->slice('(0)')) %= 6;
	($xx = $data->slice('(1)')) /= 6;
	print $data;
	$data = float $data;
} else {
	my ($data, $xx);

	$data = long zeroes(2,9);
	axisvalues($data->xchg(0,1));
	($xx = $data->slice('(0)')) %= 3;
	($xx = $data->slice('(1)')) /= 3;
	print $data;
	$data = float $data;
}

my ($data, $xx, $ica, $newdata, $pcadata);
$data *= 0.1;
($xx = $data->slice('(0)')) *= 2;
($xx = $data->slice('(0)')) += $data->slice('(1)') * 0.3;

$ica = new PDL::LinICA($data,4,{Accuracy => 0.001});

$newdata = $ica->get_newdata();

$pcadata = $ica->transform_pca($ica->{Data});

sub pdata {
	my($newdata) = @_;
	note $newdata;

#	points $newdata->slice('(0)'),
#		$newdata->slice('(1)');
}

note "PRINTOUT\n";

pdata($data);
pdata($pcadata);
pdata($newdata);

# Then, try some similar 3D lattice data.
}

note "NEWLATT\n";

my ($data, $data0, $ica, $newdata);

$data0 = zeroes(3,4,4,4);
axisvalues($data0->slice('(0)'));
axisvalues($data0->slice('(1)')->xchg(1,0));
axisvalues($data0->slice('(2)')->xchg(2,0));

$data = $data0->xchg(0,3)->clump(3)->xchg(0,1);

note("DATA: $data\n");

$ica = new PDL::LinICA($data,4,{Accuracy => 0.001});

$newdata = $ica->get_newdata();

note $newdata;

# points $newdata->slice('(0)'),$newdata->slice('(1)');

}

}
