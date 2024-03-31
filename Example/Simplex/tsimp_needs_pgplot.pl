use PDL;
use PDL::Graphics::PGPLOT;
use PDL::Opt::Simplex;

dev $^O =~ /MSWin32/ ? '/GW' : '/XW';
env -15,5,-15,5;
hold;

# Try a simple ellipsoid:
my $mult = pdl 4,1;
sub func { (($mult * $_[0]) ** 2)->sumover }
sub logs {
  line($_[0]->slice("(0)"),$_[0]->slice("(1)"));
  line($_[0]->slice("(0),0:2:2"),$_[0]->slice("(1),0:2:2"));
  sleep 1;
}
simplex(pdl(-10,-10), 0.5, 0.01, 30,
  \&func,
  \&logs
);
