use Test::More;
use PDL::LiteF;
use PDL::Types;
use Test::PDL;

subtest hist => sub {
  my $y = pdl( 0.7422, 0.0299, 0.6629, 0.9118, 0.1224, 0.6173, 0.9203, 0.9999,
    0.1480, 0.4297, 0.5000, 0.9637, 0.1148, 0.2922, 0.0846, 0.0954, 0.1379,
    0.3187, 0.1655, 0.5777, 0.3047 );
  is_pdl scalar $y->hist(0, 1, 0.1), pdl("3 5 1 2 1 2 2 1 0 4"), 'hist works';
};

subtest norm => sub {
    my $x = pdl('[[i 2+3i] [4+5i 6+7i]]');
    is_pdl $x->norm,
      pdl(
        [
            [ 0.267261 * i,            0.534522 + 0.801783 * i ],
            [ 0.356348 + 0.445435 * i, 0.534522 + 0.623609 * i ],
        ]
      ),
      'native complex norm works';
};

subtest glue => sub {
    my $x = xvals( 2, 2, 2 );
    my $y = yvals( 2, 2, 2 );
    my $c = zvals( 2, 2, 2 );
    is_deeply $x->glue( 1, $y, $c )->unpdl,
      [
        [ [ 0, 1 ], [ 0, 1 ], [ 0, 0 ], [ 1, 1 ], [ 0, 0 ], [ 0, 0 ] ],
        [ [ 0, 1 ], [ 0, 1 ], [ 0, 0 ], [ 1, 1 ], [ 1, 1 ], [ 1, 1 ] ]
      ];
};

subtest 'fibonacci' => sub {
    is_pdl fibonacci(15), indx('1 1 2 3 5 8 13 21 34 55 89 144 233 377 610'), 'Fibonacci sequence';
};

subtest 'indadd' => sub {
    my $a1  = pdl( 1, 2, 3 );
    my $ind = pdl( 1, 4, 6 );
    my $sum = zeroes(10);
    indadd( $a1, $ind, $sum );
    is_pdl $sum->sum, pdl(6), "indadd";
};

subtest 'one2nd' => sub {
    my $a1      = zeroes( 3, 4, 5 );
    my $indices = pdl( 0, 1, 4, 6, 23, 58, 59 );
    my ( $x, $y, $z ) = $a1->one2nd($indices);
    is_pdl $x, indx( 0, 1, 1, 0, 2, 1, 2 ), "one2nd x";
    is_pdl $y, indx( 0, 0, 1, 2, 3, 3, 3 ), "one2nd y";
    is_pdl $z, indx( 0, 0, 0, 0, 1, 4, 4 ), "one2nd z";
};

subtest approx_artol => sub {
  my $got_str = '1e-5 1e-6 1e-7 BAD 1; 1.00000005 -1.0000001 1.00002 NaN NaN';
  my $fgot = pdl($got_str);
  my $fexpected = pdl('0 0 0 BAD NaN; 1 -1 1 NaN 1');
  my $exp_a_mask = pdl('0 1 1 1 0; 1 1 0 1 0');
  my $got_a = $fgot->approx_artol($fexpected, 1e-6);
  ok all($got_a == $exp_a_mask), 'atol right' or diag "got=$got_a\nexp=$exp_a_mask";
  (my $got_str_cplx = $got_str) =~ s/ /+0i /;
  my $got_a_cplx = pdl($got_str_cplx)->approx_artol($fexpected, 1e-6);
  ok all($got_a_cplx == $exp_a_mask), 'complex atol right' or diag "got=$got_a_cplx\nexp=$exp_a_mask";
  my $got_r = $fgot->approx_artol($fexpected, 0, 1e-6);
  my $exp_r_mask = pdl('0 0 0 1 0; 1 1 0 1 0');
  ok all($got_r == $exp_r_mask), 'rtol right' or diag "got=$got_r\nexp=$exp_r_mask";
  my $fgot_badoff = $fgot->copy; $fgot_badoff->badflag(0);
  my $exp_badoff_mask = pdl('0 1 1 0 0; 1 1 0 1 0');
  my $got_badoff = $fgot_badoff->approx_artol($fexpected, 1e-6);
  ok all($got_badoff == $exp_badoff_mask), 'atol right with badflag off' or diag "got=$got_badoff\nexp=$exp_badoff_mask";
  $fexpected = long( 4,5,6,-1,8,9 )->inplace->setvaltobad(-1);
  $fgot = long( 4,5,6,7,-1,9 )->inplace->setvaltobad(-1);
  $got_a = $fgot->approx_artol($fexpected, 1e-6);
  $exp_a_mask = pdl('1 1 1 0 0 1');
  ok all($got_a == $exp_a_mask), 'bad values pattern' or diag "got=$got_a\nexp=$exp_a_mask";
  $got_a = inf(1)->approx_artol(inf(1));
  $exp_a_mask = pdl([1]);
  ok all($got_a == $exp_a_mask), 'inf matches inf' or diag "got=$got_a\nexp=$exp_a_mask";
  $got_a = pdl('inf bad')->approx_artol(pdl('inf bad'));
  $exp_a_mask = pdl([1,1]);
  ok all($got_a == $exp_a_mask), 'inf,bad matches inf,bad' or diag "got=$got_a\nexp=$exp_a_mask";
  ok all(approx_artol i,i), 'i is approx i';
  ok !all(approx_artol i,5*i), 'i is not approx 5i';
};

done_testing;
