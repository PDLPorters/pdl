#!/usr/local/bin/perl

# Test subsections/insertion 

use PDL; 
$PDL::verbose=1;

print "Testing subsections/insertion...\n";

$,=" ";

$a = log10(300+ rfits "foo.fits");

print "\nOriginal image...\n";

imag $a; sleep 2;

print "\nSubsection...\n";

$b = sec($a, 201, 250, 211, 260);

imag $b; sleep 2;

print "\nTesting insertions...\n";

$c = rvals( zeroes(200,200) )/20 ; # Test pattern
$c = max($c)-$c;

$d = ins( $a, $c, 169, 166 );
$d = ins( $d, 5.5-$b, 50,50);

imag $d;
