# -*-perl-*-
use strict;
use warnings;
use Test::More tests => 2;
use PDL::LiteF;
use PDL::Math;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

approx(pdl(0), pdl(0), 0.01); # set eps

{
my $pa = pdl( 0.01, 0.0 );
ok( all( approx( erfi($pa), pdl(0.00886,0.0) )), "erfi" );

# inplace
$pa->inplace->erfi;
ok( all( approx( $pa, pdl(0.00886,0.0) )), "erfi inplace" );
}
