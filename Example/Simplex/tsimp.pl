#use blib;
use PDL;
use PDL::Primitive;
use PDL::Graphics::PGPLOT;
use PDL::Opt::Simplex;

use Carp;

$SIG{__DIE__} = sub {print Carp::longmess(@_); die FOO};

# First, a 1-dimensional test:

sub func1 {
	my($x) = @_;
	return ($x**2)->slice('(0)');
}

sub logs1 {
	print "NOW: $_[0],$_[1]\n\n";
}
simplex(ones(1)*10,0.3,0.01,15,\&func1,\&logs1);

# Try a simple ellipsoid:

my $mult = pdl 4,1;

dev "/XSERVE";
env -15,5,-15,5;
hold;

sub func {
	my($x) = @_;
	my $b = ($mult * $x) ** 2;
	sumover($b,(my $res = null));
	$res;
}

sub logs {
	print "NOW: $_[0],$_[1]\n\n";
	line($_[0]->slice("(0)"),$_[0]->slice("(1)"));
	line($_[0]->slice("(0),0:2:2"),$_[0]->slice("(1),0:2:2"));
}

simplex(pdl(-10,-10), 0.5, 0.01, 30,
	\&func,
	\&logs
);



