use strict;
use warnings;

# Run niceslice.t with Filter::Util::Call engine
BEGIN {
  $ENV{PDL_NICESLICE_ENGINE} = 'Filter::Util::Call';
  $::UC = $::UC = 1;
}

use FindBin;
open my $fh, "$FindBin::Bin/niceslice.t"
  or die "Cannot read $FindBin::Bin/niceslice.t: $!";

my $source = do { local $/; <$fh> };
close $fh;

eval "#line 1 t/niceslice.t-run_by_niceslice-utilcall.t\n$source";
die $@ if $@;
