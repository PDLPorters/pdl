use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;

subtest interpol => sub {

    subtest real => sub {
        my $yvalues = PDL->new( 0 .. 5 ) - 20;
        my $xvalues = -PDL->new( 0 .. 5 ) * .5;
        my $x       = PDL->new(-2);
        is( $x->interpol( $xvalues, $yvalues ), -16, "result" );
    };

    subtest complex => sub {
        my $yvalues = ( PDL->new( 0 .. 5 ) - 20 ) * ( 1 + i() );
        my $xvalues = -PDL->new( 0 .. 5 ) * .5;
        my $x       = PDL->new(-2);


        ok( all( $x->interpol( $xvalues, $yvalues ) == ( -16 - 16 * i ) ),
            "result" );

        throws_ok { $x->interpol( $xvalues * i(), $yvalues ) }
            qr/must be real/,
            "x must be real";
    };

};

subtest interpND => sub {

    my $x     = xvals( 10, 10 ) + yvals( 10, 10 ) * 10;
    my $index = cat( 3 + xvals( 5, 5 ) * 0.25, 7 + yvals( 5, 5 ) * 0.25 )
      ->reorder( 2, 0, 1 );
    my $z = 73 + xvals( 5, 5 ) * 0.25 + 2.5 * yvals( 5, 5 );
    my $y;
    lives_ok { $y = $x->interpND($index) } 'interpND';
    ok !any( $y != $z ), "result";
};

done_testing;
