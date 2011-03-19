#!/usr/bin/perl
#
# Simple tests for PDL::Constants
#
BEGIN {
   use Test::More tests => 3;
}

BEGIN {
   use_ok( 'PDL::Constants' );
}

# just checks values, assumes constant part is ok
ok( abs( PI - 3.14159265358979 ) < 0.0001, 'PI is defined');
ok( abs( E  - 2.71828182845905 ) < 0.0001, 'E  is defined');

# done_testing();
