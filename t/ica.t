
use PDL::LiteF;

BEGIN {
        eval " use PDL::Slatec; ";
        $loaded = ($@ ? 0 : 1);
	eval " use PDL::ICA; ";
	$loaded++ unless $@;
}

# use PDL::Graphics::PG;
# dev "/XSERVE",2,2;

$|=1;

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use Carp;

$SIG{__DIE__} = sub {print Carp::longmess(@_); die FOO;};

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
#	print "APR: $a,$b,$c,$d;\n";
	$d < 0.001;
}

print "1..1\n";
unless ($loaded) {
        #print STDERR "PDL::Slatec not installed. Skipping all tests.\n";
        for (1..1) {
                print "ok $_ # Skipped: PDL::Slatec not available\n";
        }
        exit;
}
ok(1,$loaded);

#DEFERRED

if(0) {

# Generate data: oblique lattice

$pars = pdl 2,3,4;
$rot = PDL::LinICA::_cayleygen({NVars=>3},$pars);

print $rot;

$inv = inv($rot);
print $inv;

if(0) {

if(0) {
$data = long zeroes(2,36);
axisvalues($data->xchg(0,1));
($xx = $data->slice('(0)')) %= 6;
($xx = $data->slice('(1)')) /= 6;
print $data;
$data = float $data;
} else {
$data = long zeroes(2,9);
axisvalues($data->xchg(0,1));
($xx = $data->slice('(0)')) %= 3;
($xx = $data->slice('(1)')) /= 3;
print $data;
$data = float $data;
}

$data *= 0.1;
($xx = $data->slice('(0)')) *= 2;
($xx = $data->slice('(0)')) += $data->slice('(1)') * 0.3;

$ica = new PDL::LinICA($data,4,{Accuracy => 0.001});

$newdata = $ica->get_newdata();

$pcadata = $ica->transform_pca($ica->{Data});

sub pdata {
	my($newdata) = @_;
	print $newdata;

#	points $newdata->slice('(0)'),
#		$newdata->slice('(1)');
}

print "PRINTOUT\n";

pdata($data);
pdata($pcadata);
pdata($newdata);

# Then, try some similar 3D lattice data.
}

print "NEWLATT\n";

$data0 = zeroes(3,4,4,4);
axisvalues($data0->slice('(0)'));
axisvalues($data0->slice('(1)')->xchg(1,0));
axisvalues($data0->slice('(2)')->xchg(2,0));

$data = $data0->xchg(0,3)->clump(3)->xchg(0,1);

print("DATA: $data\n");

$ica = new PDL::LinICA($data,4,{Accuracy => 0.001});

$newdata = $ica->get_newdata();

print $newdata;

# points $newdata->slice('(0)'),$newdata->slice('(1)');

}

