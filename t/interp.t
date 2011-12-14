# NOTE: 
#  currently not in use anymore
#  - see PDL::Func (in Lib/) and t/func.t
print "1..1\nok 1 # Skipped: see PDL::Func\n";
exit;

use PDL::LiteF;

use strict;

my $ctr = 0;
sub ok {
    $ctr++;
    my $result = shift ;
    print "not " unless $result ;
    print "ok $ctr\n" ;
}

print "1..5\n";

##########################################################

eval "use PDL::Interpolate;";

my $x = float( 1, 2, 3, 4, 5, 6, 8, 10 );
my $y = ($x * 3) * ($x - 2);

my $obj = new PDL::Interpolate( x => $x, y => $y );
ok( UNIVERSAL::isa( $obj, 'PDL::Interpolate' ) );
ok( $obj->library eq "PDL" );

my $xi = $x - 0.5;
my $yi = $obj->interpolate( $xi );
ok( $obj->status == -1 );

# compare to direct version
my ( $ans, $err ) = PDL::Primitive::interpolate( $xi, $x, $y );
my $d = abs( $ans - $yi ); 
ok( all $d < 1.0e-5 );

my $oerr = $obj->get( 'err' );
ok( all ($oerr-$err) == 0 );

#print "x:  ", $x, "\n";
#print "xi: ", $xi, "\n";
#print "$oerr\n";

# end



