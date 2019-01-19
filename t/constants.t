#!/usr/bin/perl
#
# Simple tests for PDL::Constants
#
BEGIN {
   use Test::More tests => 4;
}

BEGIN {
   use_ok( 'PDL::Constants', qw(PI E DEGRAD) );
}

# just checks values, assumes constant part is ok
ok( abs( PI - 3.14159265358979 ) < 0.0001, 'PI is defined');
ok( abs( E  - 2.71828182845905 ) < 0.0001, 'E  is defined');
ok( abs( DEGRAD - 57.295779513082321 ) < 0.0001, 'DEGRAD is defined');

# need to add tests for imaginary units (TODO)

# done_testing();
