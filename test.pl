#!/usr/local/bin/perl

# Run and check all the tests

$| = 1; # Unbuffer STDOUT

$ENV{PERL_DL_NONLAZY}=0; # Workaround PGPLOT stuff

$ENV{PERL5LIB}=join(':',@INC); # Propagate to subscripts

print "Default Device for plot tests [recommend /XSERVE] ? ";
$dev = <STDIN>; chomp $dev;
$dev = "/XSERVE" unless $dev=~/\w/;
$ENV{PGPLOT_DEV}=$dev;

$ENV{GPLOT_XW_WIDTH}=0.3;

foreach $i (1..5) {

   $name = "test$i.p";

   print "========== Running $name... ==========\n";

   system "perl $name | tee /tmp/test$$";

   print "========== verifying vs $name.out... ==========\n";

   system "diff /tmp/test$$ $name.out";

   unlink "/tmp/test$$";

   sleep 2;
}
