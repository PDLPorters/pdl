use strict;
use warnings;
use PDL;
use PDL::IO::Pic;
use Test::More tests => 3;

# The rim() function was failing badly for a number of reasons ...
# and perhaps is still failing.
# See http://mailman.jach.hawaii.edu/pipermail/pdl-porters/2012-July/004916.html
# This script serves firstly as a reminder that rim() needs fixing,
# and subsequently that it stays in a basically functional form.
# AFAIK, this script itself breaks none of the rules regarding the
# the usage of the rim() function - Sisyphus.

my $cols = 3;
my $rows = 3;

my $ext = 'pnm';
my $fmt = uc($ext);

my $file = "ushort.$ext";

my $in  = sequence($cols, $rows)->ushort * 213;
$in->wpic($file, {FORMAT => $fmt});

my $out1 = rim($file, {FORMAT => $fmt});

my $out2 = sequence($cols, $rows);
rim($out2, $file, {FORMAT => $fmt});

my $out3 = PDL->rpic($file, {FORMAT => $fmt});

# Test 1
ok(sum(abs($out1 - $out2)) == 0, "\$out1 & \$out2 are the same");

# Test 2
ok(sum(abs($out3 - $out2)) == 0, "\$out3 & \$out2 are the same");

# Test 3
ok(sum(abs($out1 - $in  )) == 0, "\$out1 & \$in are the same");

END {
 unlink $file;
};
