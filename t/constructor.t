#!/usr/bin/perl
#
# Test for bug in the pdl constructor for mixed arguments.
# Separate from core.t because the problem crashes perl
# and I'd like to keep the granularity of the core.t tests
#
use Test::More tests => 1;
use PDL::LiteF;
use PDL::Config;

TODO: {

   local $TODO = 'Known_problems bug sf.net #3011879' if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS});

   # This is from sf.net bug #3011879
   $c[0][0]=pdl(0,4,2,1);
   $c[1][0]=pdl(0,0,1,1);
   $c[2][0]=pdl(0,0,0,1);
   $c[0][1]=pdl(0,0,3,1);
   $c[1][1]=pdl(0,0,2,1);
   $c[2][1]=pdl(5,1,1,1);
   $d = pdl(@c);

   # diag("\$d is $d\n");

   isa_ok($d, 'PDL');
};
