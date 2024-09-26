# This tests the 16-bit image capabilities of the rpic() and wpic()
# commands.  The new code works with PNM output files and PNG format
# too.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use PDL::LiteF;
use PDL::NiceSlice;
use PDL::IO::Pic;

my $can_png = PDL->wpiccan('PNG');

$PDL::IO::Pic::debug=20;

# test save/restore of 8-bit image
my $x = sequence(16, 16);
my $tmpdir = tempdir( CLEANUP => 1 );
my $filestub = File::Spec->catfile($tmpdir, 't byte_a');
$x->wpic("$filestub.pnm");
my $a_pnm = rpic("$filestub.pnm");
ok(sum(abs($x-$a_pnm)) == 0, 'pnm byte image save+restore');
unlink "$filestub.pnm";

if ($can_png) {
  $x->wpic("$filestub.png");
  my $a_png;
  unless ($^O =~ /MSWin32/i) { $a_png = rpic("$filestub.png") }
  else { $a_png = rpic("$filestub.png", {FORMAT => 'PNG'}) }
  ok all($x == $a_png), 'png byte image save+restore';
  unlink "$filestub.png";
}

# test save/restore of 16-bit image
my $a16 = sequence(256, 255)->ushort * 231;
my $pnm_file = File::Spec->catfile($tmpdir, 'tushort_a16.pnm');
$a16->wpic($pnm_file);
my $a16_pnm = rpic($pnm_file);
ok all($a16 == $a16_pnm), 'pnm ushort image save+restore';

if ($can_png) {
  my $png_file = File::Spec->catfile($tmpdir, 'tushort_a16.png');
  $a16->wpic($png_file);
  my $a16_png = rpic($png_file, $^O =~ /MSWin32/i ? {FORMAT => 'PNG'} : ());
  ok all($a16 == $a16_png), 'png ushort image save+restore';
}

done_testing;
