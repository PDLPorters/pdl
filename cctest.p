#!/usr/local/bin/perl

use PDL;

$a = rfits "cctest.fits";
$b = $a>100;

# Load a super multicolour table

$ctab = pdl ( 
[0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1],
[0,1,0,0,0,1,1,1,1],
[0,0,1,0,1,0,1,1,0.5],
[0,0,0,1,1,1,0,1,0]
);

imag ($b); 

sleep 1;

$c = cc8compt( $b );
imag $c;
ctab $ctab;

print "Browsing image\n";
print "Hit Q to exit\n";

while($ch ne "q") {
     pgcurs($x,$y,$ch);
     print sec($c, $x-4,$x+4,$y-4,$y+4);
}
