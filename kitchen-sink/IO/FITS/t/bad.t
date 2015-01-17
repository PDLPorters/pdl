# -*-perl-*-
#
# test bad value handling in PDL
# - as it's a compile-time option we
#   skip unless $PDL::Config{WITH_BADVAL}
#

use strict;
use Test::More;

# although approx() caches the tolerance value, we
# use it in every call just to document things
#
use constant ABSTOL => 1.0e-4;

use File::Temp qw( tempfile );
my $fname;
{
   local $^W = 0;
   (undef, $fname) = tempfile( 'delmeXXXXX', SUFFIX => '.fits', OPEN => 0 );
}

END {
    unlink $fname if -e $fname;
}

use PDL::LiteF;
$| = 1;

use PDL::Config;
if ( $PDL::Config{WITH_BADVAL} ) {
    plan tests => 4;
} else {
    # reduced testing
    plan tests => 10;

    my $a = pdl(1,2,3);
    is( $a->badflag(), 0 ); # 1
    
    my $b = pdl(4,5,6);
    my $c = $a + $b;
    is( $c->badflag(), 0 ); # 2
    is( $c->sum(), 21 );    # 3
    
    # can not set the bad flag
    $a->badflag(1);
    is( $a->badflag(), 0 );

    # and piddles do not have a bad value
    ok( ! defined $a->badvalue );

    # can not change a piddle to include bad values
    ok( all( ($a->setbadif( $a == 2 ) - pdl(1,2,3)) == 0 ) );

    $a = ones(3,2,4);
    $b = zeroes(2,4);
    $c = ones(2,4) * 3;
    is( $a->nbad, 0 );
    is( $a->ngood, 24 );
    ok( all( ($a->nbadover  - $b) == 0 ) );
    ok( all( ($a->ngoodover - $c) == 0 ) );

    exit;
}

# test r/wfits
use PDL::IO::FITS;

$a = sequence(10)->setbadat(0);
print "Writing to fits: $a  type = (", $a->get_datatype, ")\n";
$a->wfits($fname);
$b = rfits($fname);
print "Read from fits:  $b  type = (", $b->get_datatype, ")\n";

ok( $b->slice('0:0')->isbad, "rfits/wfits propagated bad flag" );
ok( sum(abs($a-$b)) < 1.0e-5, "  and values" );

# now force to integer
$a->wfits($fname,16);
$b = rfits($fname);
print "BITPIX 16: datatype == ", $b->get_datatype, " badvalue == ", $b->badvalue(), "\n";
ok( $b->slice('0:0')->isbad, "wfits coerced bad flag with integer datatype" );
ok( sum(abs(convert($a,short)-$b)) < 1.0e-5, "  and the values" );
