use strict;
use warnings;
use PDL::LiteF;
use PDL::IO::Pic;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;

my $fmt = uc(my $ext = 'pnm');
my $file = File::Spec->catfile(tempdir( CLEANUP => 1 ), "ushort.$ext");

test_pdl(sequence(3,3)->ushort * 213, 0, $file);
test_pdl(sequence(3,3,3)->ushort * 213, 1, $file);
test_pdl(sequence(3,4,4)->ushort * 213, 1, $file);

done_testing;

sub test_pdl {
  my ($in, $expect_reorder, $file) = @_;
  my $orig_info = $in->info;
  $in->wpic($file, {FORMAT => $fmt});
  my $out1 = rim($file, {FORMAT => $fmt});
  my $out2 = PDL->null;
  rim($out2, $file, {FORMAT => $fmt});
  my $out3 = PDL->rpic($file, {FORMAT => $fmt});
  if ($expect_reorder) { $_ = $_->mv(-1,0) for $out1, $out2 }
  eval {ok all($out1 == $in), "\$out1 & \$in are the same $orig_info"};
  is $@, '', $orig_info;
  eval {ok all($out2 == $in), "\$out2 & \$in are the same $orig_info"};
  is $@, '', $orig_info;
  eval {ok all($out3 == $in), "\$out3 & \$in are the same $orig_info"}
    or diag "in=$in\nout1=$out1";
  is $@, '', $orig_info;
}
