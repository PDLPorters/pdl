#!/usr/local/bin/perl

# Simple test

use PDL;

$PDL::verbose=1;

$aa = pdl [1..10];
print '$aa = ',$aa,"\n";

$bb = $aa*0 + 3;
print '$bb = ',$bb,"\n";

$cc = pdl [200..209];
$cc++;
print '$cc = ',$cc,"\n";

print "\n";

print 'Expr  = ', ($cc-$aa)+2/$bb,"\n\n";

$pi = 4*atan2(1,1);

print 'Expr2 = ', exp($aa),"\n\n";
print 'Expr3 = ', $aa % 3,"\n\n";
print 'Expr4 = ', atan2(short($aa),$bb),"\n\n";

$aa = pdl [-20..20,42,-100]; 
print '$aa   = ', ($aa),"\n\n";
print 'Min Max = ', min($aa)," ",max($aa),"\n\n";

$aa = pdl [1..100];
line +sin($aa)/$aa ;

print "Calculating factorials for recursion test...\n";

sub nfac { my $x = shift; return at($x,0,0)==0?  $x*0+1 : $x*nfac($x-1) } 

print nfac( pdl([5,5,5],[5,5,5]) ), "\n";
print nfac( zeroes(10,10)+10 ),        "\n";



