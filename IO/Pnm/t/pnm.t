use strict;
use warnings;

use PDL::LiteF;
use PDL::IO::Pnm;
use PDL::Dbg;
use File::Temp qw(tempdir);
use File::Spec;
use Test::More;

# we need tests with index shuffling once vaffines are fixed

sub tapprox {
	my($pa,$pb,$mdiff) = @_;
	all approx($pa, $pb,$mdiff || 0.01);
}

my $tmpdir = tempdir( CLEANUP => 1 );
sub rpnm_unlink {
  my ($data, $ext, $format, $raw) = @_;
  my $file = File::Spec->catfile($tmpdir, "temp.$ext");
  wpnm($data, $file, $format, $raw);
  my $pdl = rpnm($file);
  unlink $file;
  return $pdl;
}

$PDL::debug = $PDL::debug = 0;
$PDL::debug = 1 if defined($ARGV[0]) && $ARGV[0] =~ /-v/;

#              [FORMAT, extension, ushort-divisor,
#               only RGB/no RGB/any (1/-1/0), mxdiff]
#  no test of PCX format because seems to be severely brain damaged
my @formats = ( ['PNM', 'pnm',  1, 0, 0.01],
	        ['GIF', 'gif',256, 0, 0.01],
	        ['TIFF','tif',  1, 0, 0.01],);

## GIF doesn't handle 16-bit so it has 2 * 2 tests
## while the other formats have 2 * 3 tests each

my $im1 = ushort([[0,65535,0], [256,256,256], [65535,256,65535]]);
my $im2 = byte($im1/256);

# make the resulting file at least 12 byte long
# otherwise we run into a problem when reading the magic (Fix!)
my $im3 = byte [[0,0,255,255,12,13],[1,4,5,6,11,124],
	        [100,0,0,0,10,10],[2,1,0,1,0,14],[2,1,0,1,0,14],
	        [2,1,0,1,0,14]];

if ($PDL::debug) {
  note $im1; $im1->px;
  note $im2; $im2->px;
  note $im3>0; $im3->px;
}

# for some reason the pnmtotiff converter coredumps when trying
# to do the conversion for the ushort data, haven't yet tried to
# figure out why
for my $raw (0,1) {
  foreach my $form (@formats) {
    my $in2 = rpnm_unlink($im2, $form->[1], 'PGM', $raw);
    my $in3 = rpnm_unlink($im3, $form->[1], 'PBM', $raw);
    my $comp = ($form->[3] ? $im2->dummy(0,3) : $im2);
    ok(tapprox($in2,$comp)) or diag "got=$in2\nexpected=$comp";
    $comp = $form->[3] ? ($im3->dummy(0,3)>0)*255 : ($im3 > 0);
    $comp = $comp->ushort*65535 if $form->[0] eq 'SGI'; # yet another format quirk
    ok(tapprox($in3,$comp)) or diag "got=$in3\nexpected=$comp";
    next if $form->[0] eq 'GIF';
    my $in1 = rpnm_unlink($im1, $form->[1], 'PGM', $raw);
    my $scale = $form->[3] ? $im1->dummy(0,3) : $im1;
    $comp = $scale / $form->[2];
    ok(tapprox($in1,$comp,$form->[4]), $form->[0])
      or diag "got=$in1\nexpected=$comp";
    note $in1->px if $PDL::debug and $form->[0] ne 'TIFF';
  }
}

done_testing;
