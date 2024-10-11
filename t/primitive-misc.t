use Test::More;
use PDL::LiteF;
use PDL::Types;

use lib 't/lib';
use My::Test::Primitive;

subtest hist => sub {
  my $y = pdl( 0.7422, 0.0299, 0.6629, 0.9118, 0.1224, 0.6173, 0.9203, 0.9999,
    0.1480, 0.4297, 0.5000, 0.9637, 0.1148, 0.2922, 0.0846, 0.0954, 0.1379,
    0.3187, 0.1655, 0.5777, 0.3047 );
  is $y->hist(0, 1, 0.1).'', "[3 5 1 2 1 2 2 1 0 4]", 'hist works';
};

subtest norm => sub {

    my $x = pdl('[[i 2+3i] [4+5i 6+7i]]');
    ok tapprox $x->norm,
      pdl(
        [
            [ 0.267261 * i,            0.534522 + 0.801783 * i ],
            [ 0.356348 + 0.445435 * i, 0.534522 + 0.623609 * i ],
        ]
      ),
      'native complex norm works'
      or diag $x->norm;

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
    my $fib = fibonacci(15);
    my $fib_ans =
      pdl( 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610 );
    ok tapprox( $fib, $fib_ans ), 'Fibonacci sequence';
};

subtest 'indadd' => sub {
    my $a1  = pdl( 1, 2, 3 );
    my $ind = pdl( 1, 4, 6 );
    my $sum = zeroes(10);
    indadd( $a1, $ind, $sum );
    ok( tapprox( $sum->sum, 6 ), "indadd" );
};

subtest 'one2nd' => sub {
    my $a1      = zeroes( 3, 4, 5 );
    my $indices = pdl( 0, 1, 4, 6, 23, 58, 59 );
    my ( $x, $y, $z ) = $a1->one2nd($indices);
    ok tapprox( $x, pdl( 0, 1, 1, 0, 2, 1, 2 ) ), "one2nd x";
    ok tapprox( $y, pdl( 0, 0, 1, 2, 3, 3, 3 ) ), "one2nd y";
    ok tapprox( $z, pdl( 0, 0, 0, 0, 1, 4, 4 ) ), "one2nd z";
};

subtest approx_artol => sub {
  my $fgot = pdl('1e-5 1e-6 1e-7; 1.00000005 1.0000001 1.00002');
  my $fexpected = pdl('0 0 0; 1 1 1');
  my $exp_a_mask = pdl('0 1 1; 1 1 0');
  my $got_a = $fgot->approx_artol($fexpected, 1e-6);
  ok all($got_a == $exp_a_mask), 'atol right' or diag "got=$got_a\nexp=$exp_a_mask";
  my $got_r = $fgot->approx_artol($fexpected, 0, 1e-6);
  my $exp_r_mask = pdl('0 0 0; 1 1 0');
  ok all($got_r == $exp_r_mask), 'rtol right' or diag "got=$got_r\nexp=$exp_r_mask";
};

done_testing;
