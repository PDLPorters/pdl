# Run ops.t with the experimental ‘bitwise’ feature enabled.

BEGIN {
 if ("$]" < 5.022) {
   print "1..0 # skip Requires Perl 5.22\n";
   exit;
 }
}

use feature 'bitwise';

use FindBin;
open my $fh, "$FindBin::Bin/ops.t"
  or die "Cannot read $FindBin::Bin/ops.t: $!";

my $source = do { local $/; <$fh> };
close $fh;

$source =~ s/use warnings;\K/no warnings 'experimental::bitwise';/;

eval "#line 1 t/ops.t-run_by_ops-bitwise.t\n$source";
die $@ if $@;
