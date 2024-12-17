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

  $x = float( 3 .. 10 );
  my $f = $x*$x*$x + 425.42352;
  my $answer = 3*$x*$x;
  my ( $d, $err ) = pchip_chim( $x, float($f) );
  is_pdl $err, indx 0;
  # don't check the first and last elements, as expect the
  # error to be largest there
  # value of 5% comes from tests on linux and solaris machines
  is_pdl +(map $_->slice('1:-2'), $d, $answer), {rtol=>0.05};
  my $d2 = $f->zeroes;
  pchip_chic( pdl([0, 0]), pdl([0, 0]), 1, $x, $f, $d2, my $err2=null );
  is_pdl $err2, indx 0;
  is_pdl $d2, $d->double, {atol=>2e-2};
  pchip_chsp( pdl([0, 0]), pdl([0, 0]), $x, $f, my $d3=null, my $err3=null );
  is_pdl $err3, indx 0;
  is_pdl $d3, $d->double, {atol=>2};
  my $xe = float( pdl( 4 .. 8 ) + 0.5 );
  my ( $fe, $de );
  ( $fe, $de, $err ) = pchip_chfd( $x, $f, $d, 1, $xe );
  is_pdl $err, indx(0);
  $answer = $xe*$xe*$xe + 425.42352;
  is_pdl $fe, $answer, {rtol=>1e-5};
  $answer = 3.0*$xe*$xe;
  is_pdl $de, $answer, {rtol=>2e-2};
  ( $fe, $err ) = pchip_chfe( $x, $f, $d, 1, $xe );
  is_pdl $fe, $xe*$xe*$xe + 425.42352, {rtol=>1e-3};
  is_pdl $err, indx 0;
  $x   = float( 1, 2, 3, 5, 6, 7 );
  $f   = float( 1, 2, 3, 4, 3, 4 );
  ( $d, $err ) = pchip_chim($x, $f);
  is_pdl $err, indx 2;
  $x = double( sequence(11) - 0.3 );
  $f = $x * $x;
  ( $d, $err ) = pchip_chim($x, $f);
  my $ans = pdl( 9.0**3, (8.0**3-1.0**3) ) / 3.0;
  ( my $int, $err ) = pchip_chia($x, $f, $d, my $skip=zeroes(2), pdl(0.0,1.0), pdl(9.0,8.0));
  is_pdl $err, indx 0,0;
  is_pdl $int, $ans, {atol=>4e-2};
  my $hi = pdl( $x->at(9), $x->at(7) );
  my $lo = pdl( $x->at(0), $x->at(1) );
  $ans = ($hi**3 - $lo**3) / 3;
  ( $int, $err ) = pchip_chid( $x, $f, $d, $skip=zeroes(2), pdl(0,1), pdl(9,7) );
  is_pdl $err, indx 0,0;
  is_pdl $int, $ans, {atol=>6e-2};
};

done_testing;
