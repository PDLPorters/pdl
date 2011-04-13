#!/usr/bin/perl
#
use PDL;
my $MB;
my @data;
my $chunk = (scalar @ARGV) ? $ARGV[0] : 1;
for ( $MB=0; $MB<5000; $MB+=$chunk) {
   print "Allocating: total data -> ${MB}MB..";
   push @data, zeros($chunk,125,1000);
   print ".. done\n";
   sleep $ARGV[1] if scalar(@ARGV) == 2;
}
print "Got total of $MB!\n";
