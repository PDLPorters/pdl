use strict;
use warnings;

# Run fits.t with Astro::FITS::Header disabled

use FindBin;
open my $fh, "$FindBin::Bin/fits.t"
  or die "Cannot read $FindBin::Bin/fits.t: $!";

my $source = do { local $/; <$fh> };
close $fh;

$INC{'Astro/FITS/Header.pm'} = 0;

eval "#line 1 t/fits.t-run_by_fits-noafh.t\n$source";
die $@ if $@;
