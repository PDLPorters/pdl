#!/usr/bin/perl
#
use PDL;
use PDL::NiceSlice;
use PDL::Graphics::TriD;

$PDL::debug_trid=1;
$PDL::Graphics::TriD::verbose = 100;
$im = sequence(640,480)/640.0/480.0;
$im3 = $im->dummy(0,3);

print "\$im3 has dims of @{[$im3->dims()]}\n";

imagrgb $im3;

