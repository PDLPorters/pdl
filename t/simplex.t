use PDL::LiteF;
use Test;
use strict;
use warnings;

BEGIN {
    eval "use PDL::Opt::Simplex;";
    unless ($@) {
        plan tests => 8;
    }       
    else {
        plan tests => 1;
        print "ok 1 # Skipped: PDL::Opt::Simplex not installed\n";
        exit;
    }
}

my $dis = pdl( 0, 1 );
my $func = sub {
    # f = x^2 + (y-1)^2 + 1
    return sumover( ( $_[0] - $dis )**2 ) + 1;
};

my ( $opt, $ssize, $optval );
my ( $x, $y );

# first test
( $opt, $ssize, $optval ) = PDL::Opt::Simplex::simplex( 
    pdl( [ 2, 2 ] ), pdl( [ 0.01, 0.01 ] ), 1e-4, 1e4, $func,
);

( $x, $y ) = $opt->list;

ok( $x < 0.001 );
ok( ( $y - 1 ) < 0.001 );
ok( $ssize < 0.001 );
ok( ( $optval - 1 ) < 0.001 ); 

# second test
my $logsub = sub {};
( $opt, $ssize, $optval ) = PDL::Opt::Simplex::simplex( 
    pdl( [ [ -1, -1 ], [ -1.1, -1 ], [ -1.1, -0.9 ] ] ), pdl( [ 0.01, 0.01 ] ),
    1e-4, 1e4, $func,
);

( $x, $y ) = $opt->list;

ok( $x < 0.001 );
ok( ( $y - 1 ) < 0.001 );
ok( $ssize < 0.001 );
ok( ( $optval - 1 ) < 0.001 ); 
