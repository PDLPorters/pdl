package PDL::Demos::TkTriD_demo;
use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Contours;
use PDL::Graphics::TriD::GL;
use Tk;
use PDL::Graphics::TriD::Tk;
use strict;
my ($TriDW,$graph); # declare the graph object in main, defined in initialize
my $e_button;

PDL::Demos::Routines->import();
sub act($);
sub comment($);


sub run {
  use English;
  print "$0\n";
  exec("$EXECUTABLE_NAME TriD/test6.p");
}

1;
