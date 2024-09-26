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
my $tmpdir = tempdir( CLEANUP => 1 );

sub roundtrip {
  my ($in, $file, $label, @extra) = @_;
  $file = File::Spec->catfile($tmpdir, $file);
  $in->wpic($file);
  my $got = rpic($file, @extra);
  ok all($in == $got), "$label image save+restore";
}

# test save/restore of 8-bit image
roundtrip(my $x = sequence(16,16), 'byte_a.pnm', 'pnm byte');

roundtrip($x, 'byte_a.png', 'png byte',
  $^O =~ /MSWin32/i ? {FORMAT => 'PNG'} : ()) if $can_png;

# test save/restore of 16-bit image
roundtrip(
  my $a16 = sequence(256, 255)->ushort * 231,
  'tushort_a16.pnm', 'pnm ushort',
);

roundtrip($a16, 'tushort_a16.png', 'png ushort',
  $^O =~ /MSWin32/i ? {FORMAT => 'PNG'} : ()) if $can_png;

done_testing;
