use Test2::V0 '!float';
use PDL::LiteF;
use PDL::Types;

use Test::Lib;
use My::Test::Primitive;

subtest 'why are these tested in Primitive?' => sub {

    subtest 'non-wrapped xvals from byte ndarray' => sub {
        my $orig  = ones( byte, 300 );
        my $xvals = $orig->xvals;
        is $xvals->at(280), 280,;
    };

    subtest 'empty ndarray' => sub {
        my $x = which ones(4) > 2;
        my $y = $x->long;
        my $c = $x->double;

        ok( isempty $x,                "isempty" );
        ok( $y->avg == 0,              "avg of Empty" );
        ok( !any isfinite $c->average, "isfinite of Empty" );
    };

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

    is $x->glue( 1, $y, $c )->unpdl,
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

# diag one2nd is undocumented.
subtest 'one2nd' => sub {
    my $a1      = zeroes( 3, 4, 5 );
    my $indices = pdl( 0, 1, 4, 6, 23, 58, 59 );
    my ( $x, $y, $z ) = $a1->one2nd($indices);
    ok tapprox( $x, pdl( 0, 1, 1, 0, 2, 1, 2 ) ), "one2nd x";
    ok tapprox( $y, pdl( 0, 0, 1, 2, 3, 3, 3 ) ), "one2nd y";
    ok tapprox( $z, pdl( 0, 0, 0, 0, 1, 4, 4 ) ), "one2nd z";
};

done_testing;
