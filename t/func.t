# -*-perl-*-
#
use PDL::LiteF;

my $ctr = 1;
sub ok {
    my $result = shift ;
    print "not " unless $result ;
    print "ok $ctr\n" ;
    $ctr++;
#    print "[ $result ]\n";
}

$loaded = 0; $slatec = 0;
eval "use PDL::Func;";
$loaded = ($@ ? 0 : 1);
eval "use PDL::Slatec";
$slatec = ($@ ? 0 : 1);
 
my $ntests = 15;
print "1..$ntests\n";

##########################################################

use PDL::Func;

my $x = float( 1, 2, 3, 4, 5, 6, 8, 10 );
my $y = ($x * 3) * ($x - 2);

my $obj = init PDL::Func ( x => $x, y => $y );
ok( $obj->scheme() eq "Linear" );

my $xi = $x - 0.5;
my $yi = $obj->interpolate( $xi );
ok( $obj->status == -1 );

# compare to direct version
my ( $ans, $err ) = PDL::Primitive::interpolate( $xi, $x, $y );
my $d = abs( $ans - $yi ); 
ok( all $d < 1.0e-5 );

my $oerr = $obj->get( 'err' );
ok( all ($oerr-$err) == 0 );

# check we trap a call to an unavailable method
eval { $obj->gradient( $xi ); };
ok( $@ ne "" );

## Test: Hermite
#
unless ( $slatec ) {
    for ($ctr..$ntests) {
        print "ok $_ # Skipped: PDL::Slatec not available.\n";
    }
    exit;                                                                       
}

$x = sequence(float,10);
$y = $x*$x + 0.5;
$obj->set( Interpolate => "Hermite", x => $x, y => $y, bc => "simple" );

ok( $obj->scheme() eq "Hermite" ); 
ok( $obj->status == 1 );

my $gi;

$xi = sequence(float,5) + 2.3;
$yi = $obj->interpolate( $xi );
ok( $obj->status == 1 );

$ans = $xi*$xi + 0.5;
$d   = abs( $ans - $yi );
ok( all $d <= 0.03 );

$gi = $obj->gradient( $xi );
ok( $obj->status == 1 );

$ans = 2*$xi;
$d   = abs( $ans - $gi );
ok( all $d <= 0.04 );

# see how they cope with threading 
#
$y = cat( $x*$x+43.3, $x*$x*$x-23 );

$obj->set( x => $x, y => $y );
ok( $obj->status == 1 );

$yi = $obj->interpolate( $xi );
ok( $obj->status == 1 );
ok( dims($yi) == 2 & $yi->getdim(0) == $xi->getdim(0) & $yi->getdim(1) == 2 );

$ans = cat( $xi*$xi+43.3, $xi*$xi*$xi-23 );
$d   = abs( $ans - $yi );
ok( all $d <= 6 );

# end


