#!/usr/local/bin/perl

# Test m-dim array setting and printing

use PDL;
$PDL::verbose=1;

$,=" ";
#$PDL::use_commas=1;

$a = pdl 42;
print "\n\n\$a=", $a, "\n\n";
print "Dims = [", @{ $$a{Dims} }, "] \n";


$a = pdl [1,2,3,4];

print "\n\n\$a=", $a, "\n\n";
print "Dims = [", @{ $$a{Dims} }, "] \n";


$a = pdl [ [1,2,3], [4,5,6], [7,8,9], [4,5,6]];

print "\n\n\$a=", $a, "\n\n";
print "Dims = [", @{ $$a{Dims} }, "] \n";


$a = pdl [ [[1,1,1,1],[2,2,2,2],[3,3,3,3]], [[4,4,4,4],[5,5,5,5],[6,6,6,6]] ];

print "\n\n\$a=", $a, "\n\n";
print "Dims = [", @{ $$a{Dims} }, "] \n";

$a = pdl [ [[[1,1],[1,1],[1,1],[1,1]],[[2,2],[2,2],[2,2],[2,2]],[[3,3],[3,3],[3,3],[3,3]]], [[[4,4],[4,4],[4,4],[4,4]],[[5,5],[5,5],[5,5],[5,5]],[[6,6],[6,6],[6,6],[6,6]]] ];

print "\n\n\$a=", $a, "\n\n";
print "Dims = [", @{ $$a{Dims} }, "] \n";


print "\nTesting matrix functions......\n";

$a = pdl [1,2,3,0],[1,-1,2,7],[1,0,0,1];
$b = pdl [1,1],[0,2],[0,2],[1,1];

print '$a = ',$a,"\n";
print '$b = ',$b,"\n";

$c = $a x $b;

print '$a x $b = ',$c,"\n";


$a = pdl [2,3],[4,5];
$b = transpose(pdl [6,7]);

$c = $a x $b;

print '$a = ',$a,"\n";
print '$b = ',$b,"\n";

$c = $a x $b;

print '$a x $b = ',$c,"\n";

