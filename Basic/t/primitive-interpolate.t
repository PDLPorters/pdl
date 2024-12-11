use strict;
use warnings;
use Test::More;
use Test::Exception;
use PDL::LiteF;
use Test::PDL;

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

subtest PCHIP => sub {
  my $x = sequence(10);
  my $y = pdl('43.3 44.3 47.3 52.3 59.3 68.3 79.3 92.3 107.3 124.3; -23 -22 -15 4 41 102 193 320 489 706');
  my $xi = sequence(float,5) + 2.3;
  my ($g) = pchip_chsp([0,0], [0,0], $x, $y);
  is_pdl $g, pdl('0 2 4 6 8 10 12 14 16 18; 0 3 12 27 48 75 108 147 192 243'), 'pchip_chsp';
  ($g) = pchip_chic([0,0], [0,0], 0, $x, $y);
  is_pdl $g, pdl('0 1.5 3.75 5.8333333 7.875 9.9 11.916667 13.928571 15.9375 18; 0 1.75 10.230769 25.107143 46.061224 73.039474 106.02752 145.02027 190.01554 241'), 'pchip_chic';
  ($g) = pchip_chim($x, $y);
  is_pdl $g, pdl('0 1.5 3.75 5.8333333 7.875 9.9 11.916667 13.928571 15.9375 18; 0 1.75 10.230769 25.107143 46.061224 73.039474 106.02752 145.02027 190.01554 241'), 'pchip_chim';
  my ($yi) = pchip_chfe($x, $y, $g, [1,1], $xi);
  is_pdl $yi, my $yi_exp = pdl('48.56375 54.173375 61.777925 71.38055 82.98225; -10.973827 12.780893 56.345513 125.71307 226.88177'), 'pchip_chfe';
  ($yi) = pchip_chfd($x, $y, $g, [0,0], $xi);
  is_pdl $yi, $yi_exp, 'pchip_chfd';
  my ($integral) = pchip_chia($x, $y, $g, [0,0], 3, 4);
  is_pdl $integral, pdl(55.6298611111111,20.7538265306122), 'pchip_chia';
  ($integral) = pchip_chid($x, $y, $g, [0,0], 3, 4);
  is_pdl $integral, pdl(55.6298611111111,20.7538265306122), 'pchip_chid';
  my $nknots = zeroes(longlong, 2);
  my $t = zeroes( 2*$x->dim(0)+4, 2 );
  my ($bcoef, $ndim, $kord) = pchip_chbs($x, $y, $g, 0, $nknots, $t);
  is_pdl $nknots, longlong(24,24), 'pchip_chbs nknots';
  is_pdl $t, pdl('0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9 9; 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9 9'), 'pchip_chbs t';
  is_pdl $bcoef, pdl('43.3 43.3 43.8 44.8 46.05 48.55 50.355556 54.244444 56.675 61.925 65 71.6 75.327778 83.272222 87.657143 96.942857 101.9875 112.6125 118.3 124.3; -23 -23 -22.583333 -21.416667 -18.410256 -11.589744 -4.3690476 12.369048 25.646259 56.353741 77.653509 126.34649 157.65749 228.34251 271.65991 368.34009 425.66149 552.33851 625.66667 706'), 'pchip_chbs bcoef';
  is_pdl $ndim, longlong(20,20), 'pchip_chbs ndim';
  is_pdl $kord, longlong(4,4), 'pchip_chbs kord';
  my $x_slice = $x->slice('*1,0:-2'); # because calling with last value is out of range
  my ($val) = pchip_bvalu($t, $bcoef, 0, $x_slice);
  is_pdl $val->t, pdl('43.3 44.3 47.3 52.3 59.3 68.3 79.3 92.3 107.3; -23 -22 -15 4 41 102 193 320 489'), 'pchip_bvalu';
};

done_testing;
